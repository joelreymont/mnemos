import React, { useState } from "react";
import { HemisClient } from "./HemisClient";

export function App() {
  const [client] = useState(() => new HemisClient());

  return (
    <div className="hemis-app">
      <header>Hemis — a second brain for your code</header>
      <main>
        {/* TODO: code view, notes panel, conversation panel */}
        <p>UI skeleton. Wire this to Lisp via HemisClient.</p>
      </main>
    </div>
  );
}
