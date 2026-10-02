#!/bin/sh
# Builds the browser version into _web/ and serves it on http://localhost:${PORT:-8000}/.
# The ml-regl-js host bundle comes from $REGL_JS (default: the ml-regl checkout
# next to this repository; build it with `make build` in ml-regl/ml-regl-js).
# Pass --no-serve to only assemble _web/.
set -e
cd "$(dirname "$0")/.."
REGL_JS=${REGL_JS:-../../ml-regl/ml-regl-js/build/regl.js}
[ -f "$REGL_JS" ] || { echo "regl.js not found at $REGL_JS (set REGL_JS)"; exit 1; }
dune build ${PROFILE:+--profile $PROFILE} ./web/main.bc.js
rm -rf _web && mkdir _web
cp web/index.html _web/
cp _build/default/web/main.bc.js _web/
cp "$REGL_JS" _web/regl.js
cp -r assets _web/assets
[ "$1" = "--no-serve" ] && exit 0
echo "serving http://localhost:${PORT:-8000}/"
exec python3 -m http.server -d _web "${PORT:-8000}"
