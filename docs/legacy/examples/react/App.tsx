import React, { useState } from "react";
import { MnemosClient } from "./MnemosClient";

export function App() {
  const [client] = useState(() => new MnemosClient());

  return (
    <div className="mnemos-app">
      <header>Mnemos — a second brain for your code</header>
      <main>
        {/* TODO: code view, notes panel, conversation panel */}
        <p>UI skeleton. Wire this to Lisp via MnemosClient.</p>
      </main>
    </div>
  );
}
