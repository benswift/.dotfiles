#!/bin/bash

if [ -z "${INSIDE_EMACS}" ]; then
    env | sort > emacs-env.txt
else
    env | sort > shell-env.txt
fi
