import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { getConfig, DebugLevel } from './config';

let outputChannel: vscode.OutputChannel | null = null;
let logStream: fs.WriteStream | null = null;

function getOutputChannel(): vscode.OutputChannel {
  if (!outputChannel) {
    outputChannel = vscode.window.createOutputChannel('Hemis Debug');
  }
  return outputChannel;
}

function getLogStream(): fs.WriteStream {
  if (!logStream) {
    const hemisDir = process.env.HEMIS_DIR || path.join(os.homedir(), '.hemis');
    if (!fs.existsSync(hemisDir)) {
      fs.mkdirSync(hemisDir, { recursive: true });
    }
    const logPath = path.join(hemisDir, 'vscode-debug.log');
    logStream = fs.createWriteStream(logPath, { flags: 'a' });
  }
  return logStream;
}

function getTimestamp(): string {
  const now = new Date();
  const pad = (n: number, w = 2) => n.toString().padStart(w, '0');
  return `${pad(now.getHours())}:${pad(now.getMinutes())}:${pad(now.getSeconds())}.${pad(now.getMilliseconds(), 3)}`;
}

function writeLog(line: string): void {
  const channel = getOutputChannel();
  channel.appendLine(line);
  const stream = getLogStream();
  stream.write(line + '\n');
}

export function debug(message: string, level: DebugLevel = 'basic'): void {
  const config = getConfig();
  if (config.debug === 'off') {
    return;
  }
  if (level === 'verbose' && config.debug !== 'verbose') {
    return;
  }

  writeLog(`[${getTimestamp()}] ${message}`);
}

export function debugVerbose(message: string, data?: unknown): void {
  const config = getConfig();
  if (config.debug !== 'verbose') {
    return;
  }

  writeLog(`[${getTimestamp()}] ${message}`);
  if (data !== undefined) {
    writeLog(`  ${JSON.stringify(data, null, 2).split('\n').join('\n  ')}`);
  }
}

export function disposeDebug(): void {
  if (outputChannel) {
    outputChannel.dispose();
    outputChannel = null;
  }
  if (logStream) {
    logStream.end();
    logStream = null;
  }
}
