#! /usr/bin/env bash

set -e

SCRIPT_DIR=$(dirname "${BASH_SOURCE[0]}")

echo "***** COMPILING *****"
cd "$SCRIPT_DIR"
make -j1 clean all-no-docs

echo "****** DEF-USE ******"
cd "$SCRIPT_DIR/mlton"
make -j1 def-use
