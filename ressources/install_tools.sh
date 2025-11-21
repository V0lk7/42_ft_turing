#!/bin/sh

# --- Check if ghcup already installed 
if command -v ghcup >/dev/null 2>&1; then
    echo "GHCup installed : $(ghcup --version)"
    return
fi

export GHCUP_USE_XDG_DIRS=1
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash

# Source ghcup environment to make it available in this script
export PATH="$HOME/.ghcup/bin:$PATH"

# Install recommended versions of GHC, Cabal, and HLS
echo "Installing GHC (Haskell compiler)..."
ghcup install ghc recommended
ghcup set ghc recommended

echo "Installing Cabal (build tool)..."
ghcup install cabal recommended

echo "\n=== Installation Complete ==="
echo "Put this line in your .zshrc, .bashrc or other shell config file:"
echo "export GHCUP_USE_XDG_DIRS=1"
echo "\nInstalled versions:"
ghcup list -c installed