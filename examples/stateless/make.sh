#!/bin/bash
cd "$(dirname "$0")"
for pkg in canvas utf-8-validate bufferutil
do
  mkdir -p "../../js/node_modules/$pkg"
  touch "../../js/node_modules/$pkg/index.js"
done
../../scm/expand < counter.scm | ../../scm/scan | STATELESS_PRIV_PEM="$(cat ../../js/stateless/certs/stateless_priv.pem)" node --experimental-modules ../../js/stateless/initial.mjs --config url=http://localhost:3000/stateless > counter.html
