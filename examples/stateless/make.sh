#!/bin/bash
cd "$(dirname "$0")"
for pkg in canvas utf-8-validate bufferutil
do
  mkdir -p "../../stateless/node_modules/$pkg"
  touch "../../stateless/node_modules/$pkg/index.js"
done
../../scm/expand < counter.scm | ../../scm/scan | STATELESS_PRIV_PEM="$(cat ../../stateless/certs/stateless_priv.pem)" node --experimental-modules ../../stateless/initial.mjs --config url=http://localhost:3000/stateless > counter.html
