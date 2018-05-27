#!/bin/bash

openssl req \
  -key server.local.key \
  -x509 \
  -nodes \
  -new \
  -out server.local.crt \
  -subj /CN=server.local \
  -reqexts SAN \
  -extensions SAN \
  -config <(cat /usr/lib/ssl/openssl.cnf \
          <(printf '[SAN]\nsubjectAltName=DNS:server.local')) \
  -sha256 \
  -days 3650
