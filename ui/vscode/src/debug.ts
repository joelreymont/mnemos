import * as vscode from 'vscode';
import { getConfig, DebugLevel } from './config';

let outputChannel: vscode.OutputChannel | null = null;

function getOutputChannel(): vscode.OutputChannel {
  if (!outputChannel) {
    outputChannel = vscode.window.createOutputChannel('Hemis Debug');
  }
  return outputChannel;
}

function getTimestamp(): string {
  const now = new Date();
  const pad = (n: number, w = 2) => n.toString().padStart(w, '0');
  return `${pad(now.getHours())}:${pad(now.getMinutes())}:${pad(now.getSeconds())}.${pad(now.getMilliseconds(), 3)}`;
}

export function debug(message: string, level: DebugLevel = 'basic'): void {
  const config = getConfig();
  if (config.debug === 'off') {
    return;
  }
  if (level === 'verbose' && config.debug !== 'verbose') {
    return;
  }

  const channel = getOutputChannel();
  channel.appendLine(`[${getTimestamp()}] ${message}`);
}

export function debugVerbose(message: string, data?: unknown): void {
  const config = getConfig();
  if (config.debug !== 'verbose') {
    return;
  }

  const channel = getOutputChannel();
  channel.appendLine(`[${getTimestamp()}] ${message}`);
  if (data !== undefined) {
    channel.appendLine(`  ${JSON.stringify(data, null, 2).split('\n').join('\n  ')}`);
  }
}

export function disposeDebug(): void {
  if (outputChannel) {
    outputChannel.dispose();
    outputChannel = null;
  }
}
