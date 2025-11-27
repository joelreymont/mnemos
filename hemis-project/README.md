# Hemis – Second Brain for Your Codebase

This is a starter skeleton for the Hemis project:

- `backend/`: Common Lisp JSON-RPC backend over stdio (notes protocol v2)
- `../emacs/`: Emacs UI client (`hemis.el`) plus Doom module that talks to the backend

Backend entrypoint: `sbcl --script backend/hemis.lisp`  
Protocol details: see `../docs/PROTOCOL.md`  
This is NOT a complete app, just a structured starting point you can evolve.
