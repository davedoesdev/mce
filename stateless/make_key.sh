#!/bin/bash
cd "$(dirname "$0")"
node > key <<EOF
(async () => {
    const { SodiumPlus } = require('sodium-plus');
    const sodium = await SodiumPlus.auto();
    const key = await sodium.crypto_auth_keygen();
    console.log(key.getBuffer().toString('base64'));
})();
EOF
