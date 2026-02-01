#!/bin/bash
# Restart the backend server

echo "Stopping existing server..."
pkill -f "lein.*run" 2>/dev/null
sleep 2

echo "Starting backend on port 3000..."
lein run &

echo "Waiting for server to start..."
for i in {1..60}; do
    if lsof -i :3000 | grep -q LISTEN 2>/dev/null; then
        echo "Backend running at http://localhost:3000"
        exit 0
    fi
    sleep 1
done

echo "Server failed to start within 60 seconds"
exit 1
