# Maxwell's Puzzling Demon

A GUI version of the puzzle in `../sample` (same rules, all 57 levels), built
on ml-messenger with the desktop (SDL3/OpenGL) backend.

Push blocks onto every goal without letting heat reach the demon. Blocks are
hot, cold or neutral; touching objects share heat unless a side is
insulated, hot and cold cancel one for one, and heat that reaches a
wall through a bare side is drained. Edgeless sides are sticky: glued objects move together.

## Build, run, test

```sh
dune build
dune exec ./bin/main.exe          # run from this directory: asset paths are relative
dune test                         # headless end-to-end test (check/)
python3 tools/difftest.py 1 20    # diff the rules port against ../sample/maxwell
```

`MAXWELL_LEVEL=2-9 dune exec ./bin/main.exe` starts straight in a level.
`./run_mcp.sh` runs it connected to the ml-regl MCP server.

### In the browser

```sh
./web/serve.sh                    # builds, assembles _web/, serves http://localhost:8000/
PORT=8080 PROFILE=release ./web/serve.sh
./web/serve.sh --no-serve         # only assemble _web/ (a static site: index.html,
                                  # main.bc.js, regl.js, assets/)
```

Open the URL in a browser; `file://` does not work, the assets are fetched
over HTTP. The page is `web/index.html`; the ml-regl-js host bundle is copied
from `$REGL_JS` (default `../../ml-regl/ml-regl-js/build/regl.js`). Append
`#mcp=ws://127.0.0.1:8765` to the URL to connect it to the MCP server.
Progress is kept in the browser's `localStorage`.

## Controls

| Key | Action |
|---|---|
| WASD / arrows | move (hold to repeat) |
| Z, U, Backspace | undo |
| R | reset the level (undoable) |
| T | show heat links: a dot on every conducting contact |
| Enter | next level (after solving) |
| Esc | back to the level select / title; on the title, settings |
| O | settings (also the gear in the top-right corner) |
| F11 | toggle fullscreen (desktop) |

The mouse works on every button and tile.

### Settings

The settings panel opens over any screen and pauses input to it. It has the
master volume (Left/Right in tenths, or drag the slider), fullscreen, erasing
the saved progress (press twice), Resume and Quit. Fullscreen and Quit are
desktop only: the browser host ignores fullscreen, and quitting would only
freeze the page. The choices are saved under `maxwell.settings` and applied
at start.

## How the board reads

- **Copper strip**: conducting edge. **Cream stitched felt**: insulating edge.
- **Lime teeth**: an exposed sticky side (no edge). Two edgeless sides that
  touch are drawn flush, as one piece.
- **Crates with a raised panel**: movable blocks. **Bolted corners**: fixed
  hot/cold blocks and fixed glass (they never move).
- **Dark stone**: walls (fixed neutral blocks, the CLI's `#`). A wall side
  without felt drains heat: in every shipped level, each such wall is
  connected to an outer cell. The outer marker itself is not drawn, as in the
  CLI's normal view.
- **Flame / snowflake**: hot / cold. **Glass** is translucent and takes the
  tint of whatever heat flows through it. **Gold ring**: goal.
- The demon turns frosty when it conducts with a cold group and scorched
  (dead) when heat reaches it.

## Layout

```
rules/                 the game rules, framework-free (port of sample/maxwell.cpp)
tools/trace.ml         prints the CLI's trace format, for tools/difftest.py
src/app.ml             config, resources, scenes, global components
src/board_view.ml      draws a board (also used by the select-screen preview)
src/progress.ml        global component: loads saved progress at start
src/settings.ml        global component: the settings panel and its gear
src/clock.ml           forward-only clock for animations
src/scenes/title       title screen
src/scenes/select      level select; tile/ is one level's button
src/scenes/play        a level: board/, hud/, banner/ components
check/                 headless test driving Ui.update
web/                   browser executable, index.html, serve.sh
art/                   SVG sources (demon.py), build.sh, sfx.sh (sox)
assets/                fonts (MSDF), images, sounds, levels
```

Progress is saved under the key `maxwell.progress` (desktop:
`~/.local/share/Maxwells Puzzling Demon/kv_store.json`).

## Credits

Fonts: DM Serif Display and Space Mono, SIL Open Font License, converted to
MSDF atlases with msdf-bmfont-xml. Art and sounds were made for this project
(`art/`). Rules and levels: `../sample`.
