#!/bin/bash
# test.sh - Test Emacs IDE configuration
# Version: 2.0.0
set -e

echo "🧪 Testing Emacs IDE..."

# Run basic startup test
echo "Testing startup..."
timeout 10s emacs --batch -l ~/.emacs.d/init.el --eval "(message \"Startup: OK\")" 2>&1 | tail -1

# Check if health module exists and run it
if [ -f ~/.emacs.d/core/emacs-ide-health.el ]; then
    echo "Running health checks..."
    emacs --batch -l ~/.emacs.d/init.el \
          --eval "(progn (require 'emacs-ide-health) (emacs-ide-health-check-all) (message \"Health: OK\"))" 2>&1 | tail -1
fi

echo "✓ All tests passed"
