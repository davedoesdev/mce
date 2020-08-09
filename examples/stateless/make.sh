#!/bin/bash
cd "$(dirname "$0")"
../../scm/expand < counter.scm | ../../scm/scan | STATELESS_KEY="$(cat ../../stateless/key)" node ../../stateless/initial.mjs --config url=/ > ../../stateless/counter.html
