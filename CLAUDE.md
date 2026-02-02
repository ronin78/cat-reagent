# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Cat-reagent is a 2-player browser-based stealth game where one player controls a Cat (burglar) and the other controls a Caretaker trying to catch them. Built with Clojure (backend) and ClojureScript/Reagent (frontend).

## Development Workflow

### Starting Development

```bash
./start-dev.sh
```

This starts everything:
- REPL with nREPL server (port in `.nrepl-port`)
- Game server at http://localhost:3000
- Shadow-cljs with hot reload at http://localhost:3449

Logs are in `/tmp/cat-*.log`

### Making Backend Changes

**Prefer REPL over server restarts.** If server started with `lein repl`:

```clojure
;; Reload changed namespaces
(require '[cat-reagent.game :as g] :reload)
(require '[cat-reagent.game.board :as b] :reload)
(require '[cat-reagent.game.combat :as combat] :reload)
```

Connect to the running REPL from another terminal:
```bash
lein repl :connect $(cat .nrepl-port)
```

Only restart the full server for:
- Changes to `server.clj` or `handler.clj` routes
- Dependency changes in `project.clj`

### Making Frontend Changes

Shadow-cljs handles hot reload automatically. Just save the file and changes appear in the browser.

### Helper Scripts

- `./start-dev.sh` - Start full dev environment (REPL + server + frontend)
- `./stop-dev.sh` - Stop all dev processes
- `./start-frontend.sh` - Start shadow-cljs watch only (if running REPL manually)

## Architecture

**Backend (Clojure):** `src/clj/cat_reagent/`
- `server.clj` - Entry point, starts Jetty on port 3000
- `handler.clj` - Reitit routes and HTTP handlers, transforms game state for frontend
- `game.clj` - Core game logic, turn processing, command dispatch
- `game/` - Submodules: `board.clj`, `combat.clj`, `movement.clj`, `sound.clj`, `state.clj`, `constants.clj`

**Frontend (ClojureScript):** `src/cljs/cat_reagent/`
- `core.cljs` - Reagent UI components, HTML/CSS rendering, keyboard handling

**Data Flow:**
1. Browser sends keypress via HTTP GET to `/play?command={cmd}&player={:cat|:caretaker}`
2. `handler.clj` calls `game/play`
3. `game.clj` modifies `@s` atom (global game state)
4. `handler.clj` transforms state via `pass-state` for the specific player's view
5. JSON response rendered by Reagent components

## Key Game Concepts

**State atom:** `game.clj` uses `@g/s` for mutable game state containing:
- `:turn` - Current player (`:cat` or `:caretaker`)
- `:move-edges` / `:sound-edges` - Loom graphs for pathfinding and sound propagation
- `:characters` - Map with `:cat` and `:caretaker` submaps (location, facing, bound limbs, etc.)
- `:treasure-map` - Position -> value map (concentrated treasure, color-coded by value)
- `:neighbors` - Noise counters for win condition (`:n1`, `:n2`)
- `:patrol-tasks` - Caretaker's current objectives

**Board as Graph:** Uses Loom library for Dijkstra pathfinding and line-of-sight calculations.

**Combat System:** Dice rolls, body part binding (arms/legs at 9+ = immobilized), gagging mechanics (in-mouth + over-mouth layers).

**Victory Conditions:**
- Cat: Collect 50 treasure and escape through front door
- Caretaker: Call 911 (must be aware), or neighbors call police (noise > 3)

## API Endpoints

- `GET /play?command={cmd}&player={player}` - Process game commands
- `GET /get-state?player={player}` - Fetch current state (filtered by player visibility)
- `GET /restart?player={player}` - Reset game

## Key Libraries

- **Ring/Reitit** - HTTP server and routing
- **Reagent** - React wrapper for ClojureScript
- **Shadow-cljs** - ClojureScript compiler with hot reload
- **Loom** - Graph algorithms (pathfinding, line-of-sight)
- **Transit** - JSON serialization

## Linting

Use clj-kondo for syntax checking:
```bash
clj-kondo --lint src/clj/cat_reagent/
clj-kondo --lint src/cljs/cat_reagent/
```

