#!/usr/bin/env bash
op get item 'fastmail' | jq '.details.fields[] | select(.designation=="password").value'
