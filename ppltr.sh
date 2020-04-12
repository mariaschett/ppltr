#!/bin/bash

PPLTRDIR=$(dirname "$(realpath "$0")")

"$PPLTRDIR"/_build/default/main.exe "$@"
