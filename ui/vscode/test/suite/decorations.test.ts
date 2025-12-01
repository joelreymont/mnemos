// Decoration display tests for Hemis VS Code extension
// Tests verify the exact content users see in decorations

import * as assert from 'assert';
import {
  formatNoteText,
  getCommentPrefix,
  editorDecorations,
} from '../../src/decorations';
import { Note } from '../../src/notes';

suite('Hemis Decorations', () => {
  suite('getCommentPrefix', () => {
    test('returns // for rust', () => {
      assert.strictEqual(getCommentPrefix('rust'), '//');
    });

    test('returns # for python', () => {
      assert.strictEqual(getCommentPrefix('python'), '#');
    });

    test('returns -- for lua', () => {
      assert.strictEqual(getCommentPrefix('lua'), '--');
    });

    test('returns // for unknown languages', () => {
      assert.strictEqual(getCommentPrefix('unknown'), '//');
    });
  });

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
      const result = formatNoteText(baseNote, 'rust', 'minimal');
      assert.strictEqual(result, '[n:abcd1234]');
    });

    test('full style includes comment prefix', () => {
      const result = formatNoteText(baseNote, 'rust', 'full');
      assert.ok(result.startsWith('//'), 'Should start with // for rust');
      assert.ok(result.includes('Test note text'), 'Should include note text');
    });

    test('stale note has [STALE] marker', () => {
      const staleNote: Note = { ...baseNote, stale: true };
      const result = formatNoteText(staleNote, 'rust', 'full');
      assert.ok(result.includes('[STALE]'), 'Should include [STALE] marker');
    });

    test('fresh note has no [STALE] marker', () => {
      const freshNote: Note = { ...baseNote, stale: false };
      const result = formatNoteText(freshNote, 'rust', 'full');
      assert.ok(!result.includes('[STALE]'), 'Should not include [STALE] marker');
    });

    test('multiline note formats each line with prefix', () => {
      const multilineNote: Note = {
        ...baseNote,
        text: 'Line one\nLine two\nLine three',
      };
      const result = formatNoteText(multilineNote, 'rust', 'full');
      const lines = result.split('\n');
      assert.strictEqual(lines.length, 3, 'Should have 3 lines');
      assert.ok(lines[0].startsWith('// Line one'), 'First line should have prefix');
      assert.ok(lines[1].startsWith('// Line two'), 'Second line should have prefix');
      assert.ok(lines[2].startsWith('// Line three'), 'Third line should have prefix');
    });

    test('python uses # comment prefix', () => {
      const result = formatNoteText(baseNote, 'python', 'full');
      assert.ok(result.startsWith('#'), 'Should start with # for python');
    });

    test('lua uses -- comment prefix', () => {
      const result = formatNoteText(baseNote, 'lua', 'full');
      assert.ok(result.startsWith('--'), 'Should start with -- for lua');
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

// Additional integration tests would go here when running with @vscode/test-electron
// These would:
// 1. Open a temp document
// 2. Call renderNotes with test data
// 3. Verify editorDecorations contains expected DecorationOptions
// 4. Check contentText, color, hoverMessage fields
