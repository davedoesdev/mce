#!/bin/bash
cd "$(dirname "$0")"
openssl genrsa -out stateless_priv.pem 2048
openssl rsa -in stateless_priv.pem -outform PEM -pubout -out stateless_pub.pem
