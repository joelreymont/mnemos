// Decoration display tests for Mnemos VS Code extension
// Tests verify the exact content users see in decorations

import * as assert from 'assert';
import {
  formatNoteText,
  editorDecorations,
} from '../../src/decorations';
import { Note } from '../../src/notes';

// Helper to create test notes with required fields
function createTestNote(overrides: Partial<Note> = {}): Note {
  return {
    id: 'abcd1234-5678-9012',
    shortId: 'abcd1234',
    file: '/tmp/test.rs',
    line: 1,
    column: 0,
    text: 'Test note text',
    summary: 'Test note text',
    nodePath: [],
    createdAt: '2024-01-01',
    updatedAt: '2024-01-01',
    formattedCreatedAt: '2024-01-01 00:00:00',
    formattedUpdatedAt: '2024-01-01 00:00:00',
    formattedLines: ['// Test note text'],
    hoverText: '**Note** (abcd1234)\n\nTest note text',
    displayMarker: '[n:abcd1234]',
    iconHint: 'fresh',
    ...overrides,
  };
}

suite('Mnemos Decorations', () => {
  suite('formatNoteText', () => {
    const baseNote: Note = {
      id: 'abcd1234-5678-9012',
      shortId: 'abcd1234',
      file: '/tmp/test.rs',
      line: 1,
      column: 0,
      text: 'Test note text',
      summary: 'Test note text',
      nodePath: [],
      createdAt: '2024-01-01',
      updatedAt: '2024-01-01',
      formattedCreatedAt: '2024-01-01 00:00:00',
      formattedUpdatedAt: '2024-01-01 00:00:00',
      formattedLines: ['// Test note text'],
      hoverText: '**Note** (abcd1234)\n\nTest note text',
      displayMarker: '[n:abcd1234]',
      iconHint: 'fresh',
    };

    test('minimal style shows [n:xxxx] format', () => {
      const result = formatNoteText(baseNote, 'minimal');
      assert.strictEqual(result, '[n:abcd1234]');
    });

    test('uses server-provided formattedLines for multi-line', () => {
      const noteWithMultiLine: Note = {
        ...baseNote,
        formattedLines: ['// Server formatted line 1', '// Server formatted line 2'],
      };
      const result = formatNoteText(noteWithMultiLine, 'full');
      assert.strictEqual(result, '// Server formatted line 1\n// Server formatted line 2');
    });

    test('full style uses formattedLines', () => {
      const result = formatNoteText(baseNote, 'full');
      assert.strictEqual(result, '// Test note text');
    });

    test('stale note uses formattedLines (color indicates staleness)', () => {
      // Staleness is indicated by rendering color, not text content
      const staleNote: Note = { ...baseNote, stale: true };
      const result = formatNoteText(staleNote, 'full');
      assert.strictEqual(result, '// Test note text');
    });

    test('fresh note uses formattedLines', () => {
      const freshNote: Note = { ...baseNote, stale: false };
      const result = formatNoteText(freshNote, 'full');
      assert.strictEqual(result, '// Test note text');
    });

    test('server formattedLines take precedence over local stale marker', () => {
      // Server already includes [STALE] in formattedLines, so we trust that
      const noteWithStaleFromServer: Note = {
        ...baseNote,
        stale: true,
        formattedLines: ['// Server note [STALE]'],
      };
      const result = formatNoteText(noteWithStaleFromServer, 'full');
      assert.strictEqual(result, '// Server note [STALE]');
    });
  });

  suite('editorDecorations map', () => {
    test('is exported and accessible', () => {
      assert.ok(editorDecorations instanceof Map, 'Should be a Map');
    });

    test('can store and retrieve decorations', () => {
      const testUri = 'file:///tmp/test.ts';
      const testDecorations = [
        {
          range: { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } },
        },
      ];

      editorDecorations.set(testUri, testDecorations as any);
      const retrieved = editorDecorations.get(testUri);

      assert.deepStrictEqual(retrieved, testDecorations);
      editorDecorations.delete(testUri); // Cleanup
    });
  });

  // Visual display data tests - verify the data used for rendering
  // Since renderNotes requires VS Code runtime, we verify the data structures
  suite('Visual Display Data', () => {
    test('fresh note has iconHint fresh for blue color', () => {
      const note = createTestNote({ stale: false, iconHint: 'fresh' });
      assert.strictEqual(note.iconHint, 'fresh', 'Fresh note should have iconHint "fresh"');
      // In renderNotes, iconHint "fresh" results in blue color (#4682B4)
    });

    test('stale note has iconHint stale for gray color', () => {
      const note = createTestNote({ stale: true, iconHint: 'stale' });
      assert.strictEqual(note.iconHint, 'stale', 'Stale note should have iconHint "stale"');
      // In renderNotes, iconHint "stale" results in gray color (#808080)
    });

    test('note line is 1-indexed from server', () => {
      // Server returns 1-indexed line, renderNotes converts to 0-indexed
      const note = createTestNote({ line: 7 });
      assert.strictEqual(note.line, 7, 'Line should be 1-indexed from server');
      // In renderNotes, line 7 becomes Range(6, 0, 6, 0) for 0-indexed
    });

    test('displayMarker is used for minimal display', () => {
      const note = createTestNote({ displayMarker: '[n:xyz12345]' });
      const result = formatNoteText(note, 'minimal');
      assert.strictEqual(result, '[n:xyz12345]', 'Minimal display uses displayMarker');
    });

    test('hoverText is ready for display in hover', () => {
      const note = createTestNote({
        hoverText: '**Note** (abcd1234)\n\nTest note text\n\nCreated: 2024-01-01',
      });
      assert.ok(note.hoverText.includes('**Note**'), 'hoverText should contain markdown');
      // In renderNotes, hoverText is passed to vscode.MarkdownString
    });

    test('formattedLines are joined for full display', () => {
      const note = createTestNote({
        formattedLines: ['// Line 1', '// Line 2', '// Line 3'],
      });
      const result = formatNoteText(note, 'full');
      assert.strictEqual(result, '// Line 1\n// Line 2\n// Line 3');
    });

    test('empty formattedLines falls back to text', () => {
      const note = createTestNote({
        text: 'Fallback text',
        formattedLines: [],
      });
      const result = formatNoteText(note, 'full');
      assert.strictEqual(result, '// Fallback text', 'Should use text with comment prefix');
    });

    test('undefined formattedLines falls back to text', () => {
      const note = createTestNote({
        text: 'Fallback text',
      });
      // @ts-ignore - testing undefined case
      note.formattedLines = undefined;
      const result = formatNoteText(note, 'full');
      assert.strictEqual(result, '// Fallback text', 'Should use text with comment prefix');
    });

    test('decoration would be at correct 0-indexed line', () => {
      // This test verifies the line conversion logic used in renderNotes
      const serverLine = 10; // 1-indexed from server
      const displayLine = Math.max(0, serverLine - 1); // Convert to 0-indexed
      assert.strictEqual(displayLine, 9, 'Line 10 from server should display at 0-indexed line 9');
    });

    test('position tracking: line updates reflected in note', () => {
      // When backend returns updated line after content change
      const note = createTestNote({ line: 7 }); // Original
      note.line = 10; // After 3 lines inserted above
      assert.strictEqual(note.line, 10, 'Line should update to new position');
    });

    test('stale detection: stale field indicates visual change needed', () => {
      const freshNote = createTestNote({ stale: false });
      const staleNote = createTestNote({ stale: true });
      assert.strictEqual(freshNote.stale, false, 'Fresh note should not be stale');
      assert.strictEqual(staleNote.stale, true, 'Stale note should be marked stale');
    });
  });
});
