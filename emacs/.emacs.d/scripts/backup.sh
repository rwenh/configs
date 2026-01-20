#!/bin/bash
# backup.sh - Backup Emacs IDE configuration
# Version: 2.0.0
set -e

BACKUP_DIR=~/emacs-backups
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
BACKUP_FILE="$BACKUP_DIR/emacs-ide-$TIMESTAMP.tar.gz"

echo "💾 Creating backup..."
mkdir -p "$BACKUP_DIR"

tar -czf "$BACKUP_FILE" \
    -C ~ \
    --exclude='.emacs.d/straight' \
    --exclude='.emacs.d/var' \
    --exclude='.emacs.d/.cache' \
    .emacs.d

echo "✓ Backup created: $BACKUP_FILE"

# Keep only last 10 backups
ls -t "$BACKUP_DIR"/emacs-ide-*.tar.gz | tail -n +11 | xargs -r rm

echo "📊 Backup complete ($(du -h "$BACKUP_FILE" | cut -f1))"
