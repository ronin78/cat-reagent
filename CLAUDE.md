# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Cat-reagent is a 2-player browser-based stealth game where one player controls a Cat (burglar) and the other controls a Caretaker trying to catch them. Built with Clojure (backend) and ClojureScript/Reagent (frontend).

## Development Commands

```bash
# Development with hot reload (Figwheel)
lein figwheel
# Server at http://localhost:3449, nREPL at localhost:7002

# Development standalone server (better for websockets)
lein do clean, run
# Server at http://localhost:3000

# REPL development
lein repl
# Then: (start-server) / (stop-server)
# ClojureScript REPL: (cljs)

# Production build
lein do clean, uberjar
```

## Architecture

**Backend (Clojure):** `src/clj/cat_reagent/`
- `server.clj` - Entry point, starts Jetty
- `handler.clj` - Reitit routes and HTTP handlers
- `game.clj` - Core game logic, state management (939 lines)
- `ascii.clj` - Board visualization utilities

**Frontend (ClojureScript):** `src/cljs/cat_reagent/`
- `core.cljs` - Reagent UI components, Quil rendering, keyboard handling

**Data Flow:**
1. Browser sends keypress via HTTP GET to `/play?command={cmd}&player={:cat|:caretaker}`
2. `handler.clj` calls `game/play`
3. `game.clj` modifies `@s` atom (global game state)
4. JSON response rendered by Quil/Reagent

## Key Game Concepts

**State atom:** `game.clj` uses `@g/s` for mutable game state containing:
- `:turn` - Current player (`:cat` or `:caretaker`)
- `:move-edges` / `:sound-edges` - Loom graphs for pathfinding and sound propagation
- `:characters` - Map with `:cat` and `:caretaker` submaps (location, facing, bound limbs, etc.)

**Board as Graph:** Uses Loom library for Dijkstra pathfinding and line-of-sight calculations.

**Combat System:** Dice rolls, body part binding (arms/legs), gagging mechanics.

## API Endpoints

- `GET /play?command={cmd}&player={player}` - Process game commands
- `GET /get-state?player={player}` - Fetch current state
- `GET /restart?player={player}` - Reset game

## Key Libraries

- **Ring/Reitit** - HTTP server and routing
- **Reagent** - React wrapper for ClojureScript
- **Quil** - Graphics rendering
- **Loom** - Graph algorithms (pathfinding, line-of-sight)
- **Transit** - JSON serialization
