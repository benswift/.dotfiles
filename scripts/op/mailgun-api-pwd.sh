#!/usr/bin/env bash
op get item 'Mailgun' | jq '.details.sections[] | select(.title=="API").fields[] | select(.t=="key").v'
