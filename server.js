const path = require('path');
const express = require('express');
const bodyParser = require('body-parser');
const pty = require('node-pty');
const cors = require('cors');
const cookieParser = require('cookie-parser');
const { v4: uuidv4 } = require('uuid');

const app = express();
const port = process.env.PORT || 8080;

// Log startup attempt immediately
console.log("Starting server process...");

app.use(cors({ origin: true, credentials: true }));
app.use(bodyParser.json());
app.use(cookieParser());

const shell = process.platform === 'win32' ? 'Rterm.exe' : 'R';
const sessions = new Map();
const SESSION_TIMEOUT = 1000 * 60 * 60 * 2; 

// 1. Health check endpoint - MUST be defined before other middleware
app.get('/health', (req, res) => {
    res.status(200).send('OK');
});

/**
 * Creates/Restarts an R session
 */
async function createSession(sessionId, existingHistory = []) {
    try {
        console.log(`Spawning R process for session: ${sessionId}`);
        const ptyProcess = pty.spawn(shell, ['--vanilla', '--no-save'], {
            name: 'xterm-color', cols: 80, rows: 30
        });

        const sessionData = {
            ptyProcess,
            movesHistory: existingHistory,
            ready: false,
            lastSeen: Date.now()
        };

        sessions.set(sessionId, sessionData);

        return new Promise((resolve, reject) => {
            const timeout = setTimeout(() => {
                reject(new Error("R Startup Timeout"));
            }, 20000);

            const initListener = (data) => {
                const str = data.toString();
                if (str.includes('> ')) {
                    clearTimeout(timeout);
                    ptyProcess.removeListener('data', initListener);
                    
                    executeRCommand(sessionId, 'source("chess.R")')
                        .then(async () => {
                            if (existingHistory.length > 0) {
                                for (const move of existingHistory) {
                                    const from = move.substring(0, 2);
                                    const to = move.substring(2, 4);
                                    await executeRCommand(sessionId, `play_move("${from}", "${to}")`);
                                }
                            }
                            sessionData.ready = true;
                            console.log(`Session ${sessionId} is ready.`);
                            resolve(sessionData);
                        })
                        .catch(reject);
                }
            };
            ptyProcess.on('data', initListener);
        });
    } catch (e) {
        console.error("CRITICAL: Failed to spawn PTY:", e);
        // We don't throw here to prevent the whole server from crashing
        return null;
    }
}

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
                resolve(output);
            }
        };
        session.ptyProcess.on('data', listener);
        session.ptyProcess.write(command + '\r');
    });
}

function executeRJsonCommand(sessionId, command) {
    const session = sessions.get(sessionId);
    if (!session) return Promise.reject(new Error("Session not found"));

    return new Promise((resolve, reject) => {
        let output = '';
        const timeout = setTimeout(() => {
            session.ptyProcess.removeListener('data', listener);
            reject(new Error("R command timed out"));
        }, 15000);

        const listener = (data) => {
            const str = data.toString();
            output += str;
            if (str.includes('> ')) {
                clearTimeout(timeout);
                session.ptyProcess.removeListener('data', listener);
                const jsonMatch = output.match(/\{[\s\S]*\}/);
                if (jsonMatch) {
                    try {
                        resolve(JSON.parse(jsonMatch[0]));
                    } catch (err) {
                        reject(new Error("Failed to parse R JSON output"));
                    }
                } else {
                    reject(new Error('No JSON found in R output'));
                }
            }
        };
        session.ptyProcess.on('data', listener);
        session.ptyProcess.write(command + '\r');
    });
}

// Session Middleware
app.use(async (req, res, next) => {
    // Skip session logic for system paths
    if (req.path === '/health' || req.path === '/favicon.ico') return next();

    let sessionId = req.cookies.chess_session_id;
    if (!sessionId) {
        sessionId = uuidv4();
        res.cookie('chess_session_id', sessionId, { maxAge: 1000 * 60 * 60 * 24 * 7, httpOnly: true });
    }
    
    req.sessionId = sessionId;

    if (!sessions.has(sessionId)) {
        console.log(`New user detected: ${sessionId}. Initializing engine...`);
        createSession(sessionId).catch(err => console.error("Async Session Init Error:", err));
    } else {
        sessions.get(sessionId).lastSeen = Date.now();
    }
    next();
});

app.post('/move', async (req, res) => {
    const session = sessions.get(req.sessionId);
    if (!session || !session.ready) return res.status(503).json({ error: "Engine warming up. Please try again in a few seconds." });

    const { from, to } = req.body;
    try {
        const result = await executeRJsonCommand(req.sessionId, `play_move("${from.toUpperCase()}", "${to.toUpperCase()}")`);
        session.movesHistory.push(from + to);
        if (result.ai_from && result.ai_from.length > 0) {
            session.movesHistory.push(result.ai_from[0] + result.ai_to[0]);
        }
        res.json({ ...result, movesHistory: session.movesHistory });
    } catch (err) {
        res.status(500).json({ error: err.message });
    }
});

app.post('/reset', async (req, res) => {
    if (!sessions.has(req.sessionId)) return res.status(404).json({ error: 'Session not found' });
    try {
        await executeRCommand(req.sessionId, 'source("chess.R")');
        sessions.get(req.sessionId).movesHistory = [];
        res.json({ message: 'Reset successful' });
    } catch (err) {
        res.status(500).json({ error: err.message });
    }
});

app.get('/moves', (req, res) => {
    const session = sessions.get(req.sessionId);
    res.json({ movesHistory: session ? session.movesHistory : [] });
});

app.use(express.static(path.join(__dirname, 'public')));

app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

// Final error handler to catch any sync errors
process.on('uncaughtException', (err) => {
    console.error('FATAL UNCAUGHT EXCEPTION:', err);
});

// Bind to port 8080 immediately
const server = app.listen(port, "0.0.0.0", () => {
    console.log(`Resilient server bound to port ${port}`);
});