#!/usr/bin/env bash
# Load cabal repl with all libraries and tests available
cd "$(dirname "$0")"
cabal repl leetcode:test:leetcode-test
