#!/bin/bash
cd "$(dirname "$0")"
openssl genrsa -out stateless_priv.pem 2048
openssl rsa -in stateless_priv.pem -outform PEM -pubout -out stateless_pub.pem
cat > ../.env << EOF
STATELESS_PRIV_PEM=$(cat stateless_priv.pem | sed -z 's/\n/\\n/g')
STATELESS_PUB_PEM=$(cat stateless_pub.pem | sed -z 's/\n/\\n/g')
EOF
