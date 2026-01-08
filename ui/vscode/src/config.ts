import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

export type DebugLevel = 'off' | 'basic' | 'verbose';

export interface MnemosConfig {
  backend: string;
  mnemosDir: string;
  notesPath: string;
  autoRefresh: boolean;
  displayStyle: 'full' | 'minimal';
  debug: DebugLevel;
}

function findBackend(): string {
  const config = vscode.workspace.getConfiguration('mnemos');
  const configured = config.get<string>('backend');

  if (configured) {
    return configured;
  }

  // Try to find mnemos in PATH
  const pathDirs = (process.env.PATH || '').split(path.delimiter);
  for (const dir of pathDirs) {
    const candidate = path.join(dir, 'mnemos');
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }

  // Try common locations
  const homeDir = os.homedir();
  const extensionPath = vscode.extensions.getExtension('mnemos.mnemos')?.extensionPath;
  const devRoot = extensionPath ? path.resolve(extensionPath, '..', '..') : '';
  const commonPaths = [
    // Development builds
    ...(devRoot ? [
      path.join(devRoot, 'zig-out', 'bin', 'mnemos'),
    ] : []),
    // Installed locations
    path.join(homeDir, '.local', 'bin', 'mnemos'),
    path.join(homeDir, 'bin', 'mnemos'),
    '/usr/local/bin/mnemos',
    '/usr/bin/mnemos',
  ];

  for (const p of commonPaths) {
    if (fs.existsSync(p)) {
      return p;
    }
  }

  return '';
}

function getMnemosDir(): string {
  const config = vscode.workspace.getConfiguration('mnemos');
  const configured = config.get<string>('mnemosDir');
  if (configured) {
    return configured;
  }
  // Default to ~/.mnemos
  return path.join(os.homedir(), '.mnemos');
}

export function getConfig(): MnemosConfig {
  const config = vscode.workspace.getConfiguration('mnemos');

  return {
    backend: findBackend(),
    mnemosDir: getMnemosDir(),
    notesPath: config.get<string>('notesPath') || '',
    autoRefresh: config.get<boolean>('autoRefresh') ?? true,
    displayStyle: config.get<'full' | 'minimal'>('displayStyle') || 'full',
    debug: config.get<DebugLevel>('debug') || 'off',
  };
}
