#!/bin/bash
# Stop the development environment

echo "Stopping development servers..."
pkill -f "lein.*repl" 2>/dev/null || true
pkill -f "shadow-cljs" 2>/dev/null || true
pkill -f "lein.*run" 2>/dev/null || true
rm -f .nrepl-port
echo "Done."
