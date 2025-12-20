// server.js
// Requirements: npm install express body-parser node-pty cors

const express = require('express');
const bodyParser = require('body-parser');
const pty = require('node-pty');
const cors = require('cors'); // <-- import cors

const app = express();
const port = 3000;

// Enable CORS for all origins (you can restrict it later if needed)
app.use(cors());
app.use(bodyParser.json());

// Determine shell for R
const shell = process.platform === 'win32' ? 'Rterm.exe' : 'R';

const ptyProcess = pty.spawn(shell, ['--vanilla', '--no-save'], {
  name: 'xterm-color',
  cols: 80,
  rows: 30,
  cwd: process.cwd(),
  env: process.env
});

// Memory storage for moves
let movesHistory = [];

// Function to execute an R command and capture output until next prompt
function executeRCommand(command) {
  return new Promise((resolve, reject) => {
    let output = '';
    const listener = (data) => {
      const str = data.toString();
      output += str;
      if (str.includes('> ')) {
        ptyProcess.removeListener('data', listener);
        if (output.includes('Error')) {
          reject(new Error(output));
        } else {
          resolve(output);
        }
      }
    };
    ptyProcess.on('data', listener);
    ptyProcess.write(command + '\r');
  });
}

// Function to execute an R command expecting JSON output
function executeRJsonCommand(command) {
  return new Promise((resolve, reject) => {
    let output = '';
    const listener = (data) => {
      const str = data.toString();
      output += str;
      if (str.includes('> ')) {
        ptyProcess.removeListener('data', listener);
        const jsonMatch = output.match(/\{[\s\S]*\}/);
        if (jsonMatch) {
          try {
            const parsed = JSON.parse(jsonMatch[0]);
            resolve(parsed);
          } catch (err) {
            reject(err);
          }
        } else {
          reject(new Error('No JSON found in output: ' + output));
        }
      }
    };
    ptyProcess.on('data', listener);
    ptyProcess.write(command + '\r');
  });
}

// Initial setup: source the chess.R file after a delay to allow R to start
setTimeout(async () => {
  try {
    const sourceOutput = await executeRCommand('source("chess.R")');
    console.log('chess.R sourced successfully:', sourceOutput);
  } catch (err) {
    console.error('Error sourcing chess.R:', err.message);
  }
}, 2000);

// API endpoint for moves
app.post('/move', async (req, res) => {
  const { from, to } = req.body;
  if (!from || !to) {
    return res.status(400).json({ error: 'Missing from or to' });
  }

  // Push user's move to history
  movesHistory.push(from + to);

  try {
    const result = await executeRJsonCommand(`play_move("${from}", "${to}")`);
    
    // Push AI's move to history if available
    if (result.ai_from && result.ai_from.length > 0 && result.ai_to && result.ai_to.length > 0) {
      movesHistory.push(result.ai_from[0] + result.ai_to[0]);
    }

    res.json({ ...result, movesHistory });
    
    // user move
    console.log(`User move: ${from} to ${to}`);
    // computer move
    console.log(`Computer move: ${JSON.stringify(result, null, 2)}`);
    

  } catch (err) {
    // If error, remove the last pushed user move to keep history consistent
    movesHistory.pop();
    res.status(500).json({ error: err.message });
  }
});

// Optional: Reset game by re-sourcing chess.R and clearing moves
app.post('/reset', async (req, res) => {
  try {
    await executeRCommand('source("chess.R")');
    movesHistory = []; // clear memory
    res.json({ message: 'Game reset', movesHistory });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// GET endpoint to retrieve the entire move history
app.get('/moves', (req, res) => {
  res.json({ movesHistory });
});

app.listen(port, () => {
  console.log(`Server running on http://localhost:${port}`);
});