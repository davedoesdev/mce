#!/bin/bash
cd "$(dirname "$0")"
../../../scm/expand < ../../../examples/stateless/counter.scm | ../../../scm/scan | STATELESS_PRIV_PEM="$(cat certs/stateless_priv.pem)" node --experimental-modules ../initial.mjs --config url=https://localhost:8000 > counter.html
