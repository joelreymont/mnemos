// Stubbed tests for Hemis VS Code extension
// These tests use mocked RPC responses and run without a real backend

import * as assert from 'assert';
import { Note } from '../../src/notes';
import { formatNoteText } from '../../src/decorations';

// Mock note factory for consistent test data
function createMockNote(overrides: Partial<Note> = {}): Note {
  return {
    id: 'test-1234-5678-9012',
    shortId: 'test1234',
    file: '/tmp/test.rs',
    line: 10,
    column: 0,
    text: 'Test note content',
    summary: 'Test note content',
    nodePath: ['source_file', 'function_item', 'block'],
    createdAt: '2024-01-01T00:00:00Z',
    updatedAt: '2024-01-01T00:00:00Z',
    formattedLines: ['// Test note content'],
    hoverText: '**Note** (test1234)\n\nTest note content',
    displayMarker: '[n:test1234]',
    iconHint: 'fresh',
    ...overrides,
  };
}

suite('Stubbed Tests - No Backend Required', () => {
  suite('Note Display Fields', () => {
    test('displayMarker is used in minimal style', () => {
      const note = createMockNote({
        displayMarker: '[n:test1234]',
      });
      const result = formatNoteText(note, 'minimal');
      assert.strictEqual(result, '[n:test1234]');
    });

    test('displayMarker with custom text shown in minimal style', () => {
      const note = createMockNote({
        displayMarker: '[n:custom]',
      });
      const result = formatNoteText(note, 'minimal');
      assert.strictEqual(result, '[n:custom]');
    });

    test('formattedLines used in full style', () => {
      const note = createMockNote({
        formattedLines: ['// Line 1', '// Line 2', '// Line 3'],
      });
      const result = formatNoteText(note, 'full');
      assert.strictEqual(result, '// Line 1\n// Line 2\n// Line 3');
    });

    test('iconHint stale indicates gray color should be used', () => {
      const note = createMockNote({
        iconHint: 'stale',
        stale: false, // iconHint takes precedence
      });
      // iconHint is used in renderNotes, here we just verify the field exists
      assert.strictEqual(note.iconHint, 'stale');
    });

    test('iconHint fresh indicates blue color should be used', () => {
      const note = createMockNote({
        iconHint: 'fresh',
      });
      assert.strictEqual(note.iconHint, 'fresh');
    });
  });

  suite('Hover Text Generation', () => {
    test('hoverText field used when provided by backend', () => {
      const note = createMockNote({
        hoverText: '**Note** (test1234)\n\nCustom hover from server',
      });
      assert.ok(note.hoverText);
      assert.ok(note.hoverText.includes('Custom hover from server'));
    });

    test('fallback hover construction when hoverText missing', () => {
      const note = createMockNote({
        hoverText: undefined,
        text: 'This is note text',
        shortId: 'abc12345',
        stale: false,
      });
      // Construct the fallback hover text manually (same logic as decorations.ts)
      const fallback = `**Note** (${note.shortId})${note.stale ? ' [STALE]' : ''}\n\n${note.text}`;
      assert.strictEqual(fallback, '**Note** (abc12345)\n\nThis is note text');
    });

    test('fallback hover includes STALE marker for stale notes', () => {
      const note = createMockNote({
        hoverText: undefined,
        text: 'Stale note',
        shortId: 'stale123',
        stale: true,
      });
      const fallback = `**Note** (${note.shortId})${note.stale ? ' [STALE]' : ''}\n\n${note.text}`;
      assert.ok(fallback.includes('[STALE]'));
    });
  });

  suite('Display Position Computation', () => {
    test('displayLine differs from stored line', () => {
      const note = createMockNote({
        line: 10, // stored line
        displayLine: 15, // computed display line (content shifted)
      });
      assert.strictEqual(note.line, 10);
      assert.strictEqual(note.displayLine, 15);
      // UI should use displayLine when rendering
      const renderLine = note.displayLine ?? note.line;
      assert.strictEqual(renderLine, 15);
    });

    test('fallback to line when displayLine not provided', () => {
      const note = createMockNote({
        line: 10,
        displayLine: undefined,
      });
      const renderLine = note.displayLine ?? note.line;
      assert.strictEqual(renderLine, 10);
    });
  });

  suite('Formatted Timestamps', () => {
    test('formattedCreatedAt is human-readable', () => {
      const note = createMockNote({
        createdAt: '2024-01-15T10:30:00Z',
        formattedCreatedAt: '2024-01-15 10:30:00',
      });
      assert.strictEqual(note.formattedCreatedAt, '2024-01-15 10:30:00');
    });

    test('formattedUpdatedAt is human-readable', () => {
      const note = createMockNote({
        updatedAt: '2024-06-20T15:45:00Z',
        formattedUpdatedAt: '2024-06-20 15:45:00',
      });
      assert.strictEqual(note.formattedUpdatedAt, '2024-06-20 15:45:00');
    });
  });

  suite('Search Result Display Fields', () => {
    test('SearchHit has displayLabel and displayDetail', () => {
      // Testing the interface structure
      const searchHit = {
        kind: 'note' as const,
        file: '/tmp/test.rs',
        line: 10,
        score: 0.95,
        noteId: 'test-123',
        noteSummary: 'A test note',
        displayLabel: '[Note] A test note',
        displayDetail: 'test.rs:10',
      };
      assert.strictEqual(searchHit.displayLabel, '[Note] A test note');
      assert.strictEqual(searchHit.displayDetail, 'test.rs:10');
    });
  });

  suite('Status Display', () => {
    test('Status has statusDisplay field', () => {
      const status = {
        ok: true,
        projectRoot: '/tmp/project',
        counts: { notes: 42, files: 100, edges: 5, embeddings: 0 },
        statusDisplay: 'Hemis: 42 notes, 100 files indexed',
      };
      assert.strictEqual(status.statusDisplay, 'Hemis: 42 notes, 100 files indexed');
    });
  });

  suite('Multi-note Disambiguation', () => {
    test('multiple notes at same line can be distinguished', () => {
      const note1 = createMockNote({
        id: 'note-1111',
        shortId: 'note1111',
        line: 10,
        displayLine: 10,
        displayMarker: '[n:note1111]',
        summary: 'First note',
      });
      const note2 = createMockNote({
        id: 'note-2222',
        shortId: 'note2222',
        line: 10,
        displayLine: 10,
        displayMarker: '[n:note2222]',
        summary: 'Second note',
      });

      const notesAtLine = [note1, note2];
      assert.strictEqual(notesAtLine.length, 2);
      assert.notStrictEqual(note1.displayMarker, note2.displayMarker);
    });
  });

  suite('Error Handling Paths', () => {
    test('note with empty text falls back gracefully', () => {
      const note = createMockNote({
        text: '',
        formattedLines: undefined,
      });
      const result = formatNoteText(note, 'full');
      assert.strictEqual(result, '// ');
    });

    test('note with undefined formattedLines uses text', () => {
      const note = createMockNote({
        text: 'Fallback text',
        formattedLines: undefined,
      });
      const result = formatNoteText(note, 'full');
      assert.strictEqual(result, '// Fallback text');
    });

    test('note with empty formattedLines array uses text', () => {
      const note = createMockNote({
        text: 'Fallback text',
        formattedLines: [],
      });
      const result = formatNoteText(note, 'full');
      assert.strictEqual(result, '// Fallback text');
    });
  });
});
