#!/bin/bash
# Start shadow-cljs frontend with hot reload

echo "Starting shadow-cljs watch on port 3449..."
echo "(Frontend proxies API requests to backend on port 3000)"

# Kill any existing watch processes
pkill -f "shadow-cljs.*watch" 2>/dev/null

# Start the watch - this will block and show compilation output
npx shadow-cljs watch app
