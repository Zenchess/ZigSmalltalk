#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
SRC="$ROOT_DIR/native/mac/zs_cocoa_bridge.m"
OUT="$ROOT_DIR/native/mac/libzscocoa_bridge.dylib"

clang -fobjc-arc -dynamiclib \
  -framework Cocoa \
  -o "$OUT" "$SRC"

echo "Built $OUT"
