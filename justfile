#! /usr/bin/env just

validate-config:
  cat config.yaml | yq | json-schema-validate config-schema.json

