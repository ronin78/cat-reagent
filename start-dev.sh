#!/bin/bash
# Start full development environment: REPL + shadow-cljs

set -e
cd "$(dirname "$0")"

echo "=== Cat-Reagent Development Server ==="
echo ""

# Kill any existing processes
echo "Stopping existing processes..."
pkill -f "lein.*repl" 2>/dev/null || true
pkill -f "shadow-cljs.*watch" 2>/dev/null || true
pkill -f "lein.*run" 2>/dev/null || true
sleep 2
rm -f .nrepl-port

# Start REPL in background
echo "Starting REPL..."
lein repl :headless > /tmp/cat-repl.log 2>&1 &
REPL_PID=$!

# Wait for REPL to be ready
echo "Waiting for REPL to start (this may take a minute)..."
for i in {1..120}; do
    if [ -f .nrepl-port ]; then
        NREPL_PORT=$(cat .nrepl-port)
        echo "REPL ready on port $NREPL_PORT"
        break
    fi
    sleep 1
    # Show progress every 10 seconds
    if [ $((i % 10)) -eq 0 ]; then
        echo "  ...still starting ($i seconds)"
    fi
done

if [ ! -f .nrepl-port ]; then
    echo "ERROR: REPL failed to start. Check /tmp/cat-repl.log"
    exit 1
fi

# Start the game server via REPL
echo "Starting game server on port 3000..."
echo "(start-server)" | lein repl :connect $NREPL_PORT > /tmp/cat-server.log 2>&1

# Verify server is running
sleep 2
if curl -s http://localhost:3000/ > /dev/null 2>&1; then
    echo "Game server running at http://localhost:3000"
else
    echo "WARNING: Server may not have started correctly"
fi

# Start shadow-cljs in background
echo ""
echo "Starting shadow-cljs frontend..."
npx shadow-cljs watch app > /tmp/cat-shadow.log 2>&1 &
SHADOW_PID=$!

# Wait for shadow-cljs to compile
echo "Waiting for frontend to compile..."
for i in {1..90}; do
    if grep -q "Build completed" /tmp/cat-shadow.log 2>/dev/null; then
        echo "Frontend compiled successfully"
        break
    fi
    if grep -q "failed" /tmp/cat-shadow.log 2>/dev/null; then
        echo "WARNING: Frontend compilation had errors. Check /tmp/cat-shadow.log"
        break
    fi
    sleep 1
    if [ $((i % 10)) -eq 0 ]; then
        echo "  ...still compiling ($i seconds)"
    fi
done

echo ""
echo "=== Development environment ready ==="
echo ""
echo "  Game:     http://localhost:3000"
echo "  Frontend: http://localhost:3449 (with hot reload)"
echo "  nREPL:    port $NREPL_PORT"
echo ""
echo "To connect to REPL:"
echo "  lein repl :connect \$(cat .nrepl-port)"
echo ""
echo "To reload backend code:"
echo "  (require '[cat-reagent.game :as g] :reload)"
echo ""
echo "Logs:"
echo "  REPL:     /tmp/cat-repl.log"
echo "  Server:   /tmp/cat-server.log"
echo "  Frontend: /tmp/cat-shadow.log"
echo ""
echo "To stop: pkill -f 'lein.*repl' && pkill -f shadow-cljs"
