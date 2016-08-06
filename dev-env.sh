#!/usr/bin/env bash

if test ! -e ./papstehrenwort.cabal; then
    echo "please run from project directory"
    exit 1
fi

if (type nix-shell && \
    type cabal2nix) >/dev/null; then

    cabal2nix . > papstehrenwort.nix \
        && nix-shell -A env

else
    exit 1
fi
