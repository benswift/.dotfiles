#!/usr/bin/env bash
op get item 'ANU Identity' | jq '.details.fields[] | select(.designation=="password").value'
