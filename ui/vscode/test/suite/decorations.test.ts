// Decoration display tests for Hemis VS Code extension
// Tests verify the exact content users see in decorations

import * as assert from 'assert';
import {
  formatNoteText,
  editorDecorations,
} from '../../src/decorations';
import { Note } from '../../src/notes';

suite('Hemis Decorations', () => {
  suite('formatNoteText', () => {
    const baseNote: Note = {
      id: 'abcd1234-5678-9012',
      file: '/tmp/test.rs',
      line: 1,
      column: 0,
      text: 'Test note text',
      nodePath: [],
      createdAt: '2024-01-01',
      updatedAt: '2024-01-01',
    };

    test('minimal style shows [n:xxxx] format', () => {
      const result = formatNoteText(baseNote, 'minimal');
      assert.strictEqual(result, '[n:abcd1234]');
    });

    test('uses server-provided formattedLines when available', () => {
      const noteWithFormatted: Note = {
        ...baseNote,
        formattedLines: ['// Server formatted line 1', '// Server formatted line 2'],
      };
      const result = formatNoteText(noteWithFormatted, 'full');
      assert.strictEqual(result, '// Server formatted line 1\n// Server formatted line 2');
    });

    test('fallback uses raw text with // prefix when no formattedLines', () => {
      const result = formatNoteText(baseNote, 'full');
      assert.strictEqual(result, '// Test note text');
    });

    test('stale note fallback includes [STALE] marker', () => {
      const staleNote: Note = { ...baseNote, stale: true };
      const result = formatNoteText(staleNote, 'full');
      assert.ok(result.includes('[STALE]'), 'Should include [STALE] marker');
    });

    test('fresh note has no [STALE] marker', () => {
      const freshNote: Note = { ...baseNote, stale: false };
      const result = formatNoteText(freshNote, 'full');
      assert.ok(!result.includes('[STALE]'), 'Should not include [STALE] marker');
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
});
