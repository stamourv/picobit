#!/bin/bash
set -eu
./picobit "$1".scm && ./picobit-vm "$1".hex
