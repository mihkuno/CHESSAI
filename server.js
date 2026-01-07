// castling, enpassant, and promotion are not supported 

const path = require('path');
const express = require('express');
const bodyParser = require('body-parser');
const pty = require('node-pty');
const cors = require('cors');
const cookieParser = require('cookie-parser');
const { v4: uuidv4 } = require('uuid');

const app = express();
const port = 3000;

app.use(cors({ origin: true, credentials: true }));
app.use(bodyParser.json());
app.use(cookieParser());

const shell = process.platform === 'win32' ? 'Rterm.exe' : 'R';
const sessions = new Map();

// 2 hours of no requests = delete the R process to save RAM
const SESSION_TIMEOUT = 1000 * 60 * 60 * 2; 

/**
 * Creates/Restarts an R session
 */
async function createSession(sessionId, existingHistory = []) {
    const ptyProcess = pty.spawn(shell, ['--vanilla', '--no-save'], {
        name: 'xterm-color', cols: 80, rows: 30
    });

    const sessionData = {
        ptyProcess,
        movesHistory: existingHistory, // Preserve history if resurrecting
        ready: false,
        lastSeen: Date.now()
    };

    sessions.set(sessionId, sessionData);

    // Initial Setup
    return new Promise((resolve) => {
        const initListener = async (data) => {
            const str = data.toString();
            if (str.includes('> ')) {
                ptyProcess.removeListener('data', initListener);
                
                // 1. Source the engine
                await executeRCommand(sessionId, 'source("chess.R")');
                
                // 2. RESURRECTION: If we have history, re-play moves to catch up
                if (existingHistory.length > 0) {
                    console.log(`Resurrecting session ${sessionId} with ${existingHistory.length} moves...`);
                    for (const move of existingHistory) {
                        const from = move.substring(0, 2);
                        const to = move.substring(2, 4);
                        await executeRCommand(sessionId, `play_move("${from}", "${to}")`);
                    }
                }

                sessionData.ready = true;
                resolve(sessionData);
            }
        };
        ptyProcess.on('data', initListener);
    });
}

// Helper for R commands
function executeRCommand(sessionId, command) {
    const session = sessions.get(sessionId);
    if (!session) return Promise.reject(new Error("Session not found"));
    
    return new Promise((resolve, reject) => {
        let output = '';
        const listener = (data) => {
            const str = data.toString();
            output += str;
            if (str.includes('> ')) {
                session.ptyProcess.removeListener('data', listener);
                
                // RAW OUTPUT LOGGING
                console.log(`\n===== R RAW OUTPUT (${command}) =====\n${output}\n==============================\n`);

                if (output.includes('Error')) {
                    reject(new Error(output));
                } else {
                    resolve(output);
                }
            }
        };
        session.ptyProcess.on('data', listener);
        session.ptyProcess.write(command + '\r');
    });
}

// Helper for R commands expecting JSON output
function executeRJsonCommand(sessionId, command) {
    const session = sessions.get(sessionId);
    if (!session) return Promise.reject(new Error("Session not found"));

    return new Promise((resolve, reject) => {
        let output = '';
        const timeout = setTimeout(() => {
            session.ptyProcess.removeListener('data', listener);
            reject(new Error("R command timed out"));
        }, 10000);

        const listener = (data) => {
            const str = data.toString();
            output += str;
            if (str.includes('> ')) {
                clearTimeout(timeout);
                session.ptyProcess.removeListener('data', listener);
                
                // RAW OUTPUT LOGGING
                console.log(`\n===== R RAW OUTPUT (JSON REQ: ${command}) =====\n${output}\n==============================\n`);

                // Extract the JSON block from the R output
                const jsonMatch = output.match(/\{[\s\S]*\}/);
                if (jsonMatch) {
                    try {
                        const parsed = JSON.parse(jsonMatch[0]);
                        resolve(parsed);
                    } catch (err) {
                        reject(new Error("Failed to parse R JSON output: " + err.message));
                    }
                } else {
                    reject(new Error('No JSON found in R output: ' + output));
                }
            }
        };
        session.ptyProcess.on('data', listener);
        session.ptyProcess.write(command + '\r');
    });
}

// Middleware: Handles Cookie & Resurrection
app.use(async (req, res, next) => {
    let sessionId = req.cookies.chess_session_id;
    
    if (!sessionId) {
        sessionId = uuidv4();
        res.cookie('chess_session_id', sessionId, { maxAge: 1000 * 60 * 60 * 24 * 7 });
        await createSession(sessionId);
    } else if (!sessions.has(sessionId)) {
        await createSession(sessionId); 
    } else {
        sessions.get(sessionId).lastSeen = Date.now();
    }
    
    req.sessionId = sessionId;
    next();
});

// Move Endpoint
app.post('/move', async (req, res) => {
    const session = sessions.get(req.sessionId);
    const { from, to } = req.body;

    if (!session || !session.ready) return res.status(503).send("Engine warming up...");

    try {
        console.log(`player move: ${from.toUpperCase()} to ${to.toUpperCase()}`);

        const result = await executeRJsonCommand(req.sessionId, `play_move("${from.toUpperCase()}", "${to.toUpperCase()}")`);
        
        if (result.ai_from && result.ai_from.length > 0) {
            console.log(`AI move: ${result.ai_from[0]} to ${result.ai_to[0]}`);
        }

        printChessBoard(result);

        session.movesHistory.push(from + to);
        if (result.ai_from && result.ai_from.length > 0) {
            session.movesHistory.push(result.ai_from[0] + result.ai_to[0]);
        }

        res.json({ ...result, movesHistory: session.movesHistory });
    } catch (err) {
        res.status(500).json({ error: err.message });
    }
});

function printChessBoard(result) {
  const boardArray = result.board;
  let maxLen = 0;
  
  // Find max length for alignment
  boardArray.forEach(row => {
    row.forEach(cell => {
      const val = cell || ".";
      if (val.length > maxLen) maxLen = val.length;
    });
  });
  if (maxLen < 1) maxLen = 1;

  // Helper to pad cells and align labels
  const pad = (str) => (str || ".").padEnd(maxLen, " ");
  
  // Dynamic labels for files
  const files = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];
  // The label should be centered over the padded width. 
  // Each column is "cell + ' | '" (except the last)
  const labeledFiles = files.map(f => f.padEnd(maxLen, " ")).join("   ");
  const fileLabels = "    " + labeledFiles;

  console.log("\nVisual Chess Board:");
  console.log(fileLabels);
  
  // Divider line
  const divider = "  " + "-".repeat((maxLen + 3) * 8 + 1);
  console.log(divider);

  boardArray.forEach((row, index) => {
    const rank = 8 - index;
    const line = row.map(pad).join(" | ");
    console.log(`${rank} | ${line} | ${rank}`);
  });

  console.log(divider);
  console.log(fileLabels + "\n");
}

app.post('/reset', async (req, res) => {
    const session = sessions.get(req.sessionId);
    if (!session) return res.status(404).json({ error: 'Session not found' });

    try {
        await executeRCommand(req.sessionId, 'source("chess.R")');
        session.movesHistory = [];
        res.json({ message: 'Session game reset', movesHistory: [] });
    } catch (err) {
        res.status(500).json({ error: err.message });
    }
});

app.get('/moves', (req, res) => {
    const session = sessions.get(req.sessionId);
    res.json({ movesHistory: session ? session.movesHistory : [] });
});

setInterval(() => {
    const now = Date.now();
    for (const [id, session] of sessions.entries()) {
        if (now - session.lastSeen > SESSION_TIMEOUT) {
            console.log(`Cleaning up stale session: ${id}`);
            session.ptyProcess.kill();
            sessions.delete(id);
        }
    }
}, 1000 * 60 * 10);


// Serve static files from "public" folder
app.use(express.static(path.join(__dirname, 'public')));

// Serve index.html for root requests
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'public', 'index.html'));
});


app.listen(port, () => console.log("Resilient server active."));