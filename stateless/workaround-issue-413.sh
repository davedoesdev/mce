#!/bin/bash
cd "$(dirname "$0")"
for pkg in canvas utf-8-validate bufferutil
do
  mkdir -p "node_modules/$pkg"
  touch "node_modules/$pkg/index.js"
done
