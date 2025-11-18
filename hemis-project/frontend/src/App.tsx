import React from 'react';

export const App: React.FC = () => {
  return (
    <div style={{ display: 'flex', height: '100vh', fontFamily: 'system-ui, sans-serif' }}>
      <aside style={{ width: '260px', borderRight: '1px solid #ddd', padding: '0.75rem' }}>
        <h1 style={{ fontSize: '1rem', fontWeight: 600, marginBottom: '0.75rem' }}>Hemis</h1>
        <p style={{ fontSize: '0.85rem', opacity: 0.7 }}>Second brain for your codebase.</p>
        <hr style={{ margin: '0.75rem 0' }} />
        <p style={{ fontSize: '0.85rem', opacity: 0.8 }}>File tree placeholder</p>
      </aside>

      <main style={{ flex: 1, display: 'flex', flexDirection: 'column' }}>
        <header style={{ borderBottom: '1px solid #ddd', padding: '0.5rem 0.75rem' }}>
          <span style={{ fontSize: '0.9rem', opacity: 0.8 }}>Code / Notes / Backlinks layout skeleton</span>
        </header>

        <div style={{ flex: 1, display: 'flex' }}>
          <section style={{ flex: 2, borderRight: '1px solid #eee', padding: '0.75rem' }}>
            <h2 style={{ fontSize: '0.9rem', marginBottom: '0.5rem' }}>Code pane</h2>
            <div style={{ border: '1px solid #eee', borderRadius: 4, padding: '0.5rem', fontFamily: 'monospace', fontSize: 12 }}>
              ; CodeMirror / Monaco will live here
            </div>
          </section>

          <section style={{ flex: 1, padding: '0.75rem', display: 'flex', flexDirection: 'column', gap: '0.75rem' }}>
            <div>
              <h2 style={{ fontSize: '0.9rem', marginBottom: '0.25rem' }}>Notes</h2>
              <div style={{ border: '1px solid #eee', borderRadius: 4, padding: '0.5rem', fontSize: 12 }}>
                No notes yet. Select a symbol in the code pane to attach one.
              </div>
            </div>
            <div>
              <h2 style={{ fontSize: '0.9rem', marginBottom: '0.25rem' }}>Backlinks</h2>
              <div style={{ border: '1px solid #eee', borderRadius: 4, padding: '0.5rem', fontSize: 12 }}>
                Backlinks will show here.
              </div>
            </div>
          </section>
        </div>
      </main>
    </div>
  );
};
