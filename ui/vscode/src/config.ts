import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

export interface HemisConfig {
  backend: string;
  databasePath: string;
  autoRefresh: boolean;
  displayStyle: 'full' | 'minimal';
}

function findBackend(): string {
  const config = vscode.workspace.getConfiguration('hemis');
  const configured = config.get<string>('backend');

  if (configured) {
    return configured;
  }

  // Try to find hemis in PATH
  const pathDirs = (process.env.PATH || '').split(path.delimiter);
  for (const dir of pathDirs) {
    const candidate = path.join(dir, 'hemis');
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }

  // Try common locations
  const homeDir = os.homedir();
  const commonPaths = [
    path.join(homeDir, '.cargo', 'bin', 'hemis'),
    '/usr/local/bin/hemis',
    '/usr/bin/hemis',
  ];

  for (const p of commonPaths) {
    if (fs.existsSync(p)) {
      return p;
    }
  }

  return '';
}

export function getConfig(): HemisConfig {
  const config = vscode.workspace.getConfiguration('hemis');

  return {
    backend: findBackend(),
    databasePath: config.get<string>('databasePath') || '',
    autoRefresh: config.get<boolean>('autoRefresh') ?? true,
    displayStyle: config.get<'full' | 'minimal'>('displayStyle') || 'full',
  };
}
