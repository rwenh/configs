#!/bin/bash
# update.sh - Update Emacs IDE packages and configuration
# Version: 2.0.0
set -e

echo "🔄 Updating Emacs IDE..."
cd ~/.emacs.d

# Pull latest config
echo "📥 Pulling latest configuration..."
git pull origin main

# Update packages via Emacs
echo "📦 Updating packages..."
emacs --batch --eval "(progn (require 'straight) (straight-pull-all))" 2>&1 | grep -v "^Loading"

echo "✓ Update complete. Restart Emacs to apply changes."
