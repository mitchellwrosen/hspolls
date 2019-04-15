#!/bin/sh

# Crappy "makefile", replace with shake in due time...

set -ex

dhall-to-yaml --explain <<< ./docker-compose.dhall > ./docker-compose.yaml
dhall-to-yaml --explain <<< ./etc/prometheus.dhall > ./etc/prometheus.yaml
