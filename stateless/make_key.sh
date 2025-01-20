#!/bin/bash
cd "$(dirname "$0")"
node > key <<EOF
const sodium = require('sodium-native');
const key = Buffer.alloc(sodium.crypto_auth_KEYBYTES);
sodium.randombytes_buf(key);
console.log(key.toString('base64'));
EOF
