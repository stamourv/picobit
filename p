#!/bin/bash

set -eu

base="$(dirname "$1")/$(basename "$1" .scm)"

./picobit "$base".scm
./picobit-vm "$base".hex
