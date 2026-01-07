import * as net from 'net';
import * as path from 'path';
import * as fs from 'fs';
import { EventEmitter } from 'events';
import { getConfig } from './config';
import { debug } from './debug';

// Event types matching backend/src/events.rs
export interface NoteCreatedEvent {
  type: 'note-created';
  id: string;
  file: string;
  line: number;
  project_root?: string;
}

export interface NoteUpdatedEvent {
  type: 'note-updated';
  id: string;
}

export interface NoteDeletedEvent {
  type: 'note-deleted';
  id: string;
}

export interface NotePositionChangedEvent {
  type: 'note-position-changed';
  id: string;
  file: string;
  old_line: number;
  new_line: number;
  stale: boolean;
}

export interface AiCompleteEvent {
  type: 'ai-complete';
  note_id: string;
  success: boolean;
  provider?: string;
}

export interface IndexCompleteEvent {
  type: 'index-complete';
  project: string;
  files_indexed: number;
}

export interface FileIndexedEvent {
  type: 'file-indexed';
  file: string;
  project: string;
}

export type MnemosEvent =
  | NoteCreatedEvent
  | NoteUpdatedEvent
  | NoteDeletedEvent
  | NotePositionChangedEvent
  | AiCompleteEvent
  | IndexCompleteEvent
  | FileIndexedEvent;

/**
 * Client for receiving push events from the Mnemos backend.
 * Connects to ~/.mnemos/events.sock and emits typed events.
 */
export class EventClient extends EventEmitter {
  private socket: net.Socket | null = null;
  private buffer = '';
  private reconnectTimer: NodeJS.Timeout | null = null;
  private connecting = false;
  private stopped = false;

  private getSocketPath(): string {
    return path.join(getConfig().mnemosDir, 'events.sock');
  }

  /**
   * Start listening for events. Automatically reconnects on disconnect.
   */
  start(): void {
    if (this.socket || this.connecting) {
      return;
    }
    this.stopped = false;
    this.connect();
  }

  /**
   * Stop listening for events.
   */
  stop(): void {
    this.stopped = true;
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
    if (this.socket) {
      this.socket.destroy();
      this.socket = null;
    }
    this.buffer = '';
  }

  private connect(): void {
    if (this.stopped || this.connecting) {
      return;
    }

    const socketPath = this.getSocketPath();
    if (!fs.existsSync(socketPath)) {
      // Socket doesn't exist yet, retry later
      this.scheduleReconnect();
      return;
    }

    this.connecting = true;
    debug('Events: connecting to ' + socketPath);

    const socket = net.createConnection(socketPath);

    socket.on('connect', () => {
      this.connecting = false;
      this.socket = socket;
      debug('Events: connected');
      this.emit('connected');
    });

    socket.on('data', (data: Buffer) => {
      this.onData(data.toString());
    });

    socket.on('close', () => {
      debug('Events: disconnected');
      this.socket = null;
      this.buffer = '';
      this.emit('disconnected');
      this.scheduleReconnect();
    });

    socket.on('error', (err) => {
      debug('Events: error - ' + err.message);
      this.connecting = false;
      // Socket will emit 'close' after error
    });
  }

  private scheduleReconnect(): void {
    if (this.stopped || this.reconnectTimer) {
      return;
    }
    // Reconnect after 2 seconds
    this.reconnectTimer = setTimeout(() => {
      this.reconnectTimer = null;
      this.connect();
    }, 2000);
  }

  private onData(data: string): void {
    this.buffer += data;

    // Events are JSON lines (newline-delimited)
    let newlineIndex: number;
    while ((newlineIndex = this.buffer.indexOf('\n')) !== -1) {
      const line = this.buffer.substring(0, newlineIndex);
      this.buffer = this.buffer.substring(newlineIndex + 1);

      if (line.trim()) {
        try {
          const event: MnemosEvent = JSON.parse(line);
          this.handleEvent(event);
        } catch (err) {
          debug('Events: failed to parse - ' + line);
        }
      }
    }
  }

  private handleEvent(event: MnemosEvent): void {
    debug(`Events: ${event.type}`);

    // Emit typed event
    this.emit(event.type, event);

    // Also emit generic 'event' for catch-all handlers
    this.emit('event', event);
  }

  isConnected(): boolean {
    return this.socket !== null;
  }
}

// Singleton instance
let client: EventClient | null = null;

export function getEventClient(): EventClient {
  if (!client) {
    client = new EventClient();
  }
  return client;
}

export function disposeEventClient(): void {
  if (client) {
    client.stop();
    client = null;
  }
}
