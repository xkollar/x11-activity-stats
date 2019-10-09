#!/usr/bin/env bash
stack \
    --no-nix \
    --docker \
    build \
    --keep-going \
    --force-dirty \
    --fast \
    --test \
    --bench \
    --no-run-benchmarks \
    --ghc-options "-ddump-to-file -ddump-hi"
