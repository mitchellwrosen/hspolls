#!/bin/sh

# Crappy "makefile", replace with shake in due time...

set -ex

dhall-to-yaml <<< ./docker-compose.dhall > ./docker-compose.yaml
dhall-to-yaml <<< ./etc/prometheus.dhall > ./etc/prometheus.yaml
