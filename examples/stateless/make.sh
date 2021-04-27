#!/bin/bash
cd "$(dirname "$0")"
url="$(echo '(display-binary (save "/.netlify/functions/stateless"))' | ../../scm/expand | ../../scm/scan | ../../scm/mce)"
../../scm/expand < counter.scm | ../../scm/scan | STATELESS_KEY="$(cat ../../stateless/key)" node ../../stateless/initial.mjs --config "url=$url" > ../../stateless/counter.html
