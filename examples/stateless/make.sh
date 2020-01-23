#!/bin/bash
cd "$(dirname "$0")"
../../scm/expand < counter.scm | ../../scm/scan | STATELESS_PRIV_PEM="$(cat ../../certs/stateless_priv.pem)" node ../../stateless/initial.js --config url=/ > ../../stateless/counter.html
