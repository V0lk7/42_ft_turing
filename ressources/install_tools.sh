#!/bin/sh

# --- Check if ghcup already installed 
if command -v ghcup >/dev/null 2>&1; then
  echo "GHCup installed : $(ghcup --version)"
  return
fi

export GHCUP_USE_XDG_DIRS=1
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash

echo "Put this line in your .zshrc, .bashrc or other shell config file.\nexport GHCUP_USE_XDG_DIRS=1"
