#!/bin/sh
# Runs the game connected to the ml-regl MCP server (from the project root, so
# asset paths resolve). MAXWELL_LEVEL=<id> starts in that level.
cd "$(dirname "$0")" && dune build ./bin/main.exe && \
  DECLGL_DEBUG=1 DECLGL_CONTROL_URL=ws://127.0.0.1:8765 exec ./_build/default/bin/main.exe
