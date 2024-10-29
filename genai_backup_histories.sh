#!/bin/bash
# /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai_backup_histories.sh
# Author: ywatanabe (ywatanabe@alumni.u-tokyo.ac.jp)
# Date: $(date +"%Y-%m-%d-%H-%M")

LOG_FILE="${0%.sh}.log"

DIR="$HOME/.dotfiles/.emacs.d/lisp/genai"
BACKUP_DIR="$DIR/histories"

HISTORY_FILES=(
    "history-ai-secret.json"
    "history-human-secret.json"
)

timestamp() {
    date +"%Y%m%d_%H%M%S"
}

backup_histories() {
    mkdir -p "$BACKUP_DIR"
    
    for history_file in "${HISTORY_FILES[@]}"; do
        if [ -f "$DIR/$history_file" ]; then
            mv "$DIR/$history_file" "$BACKUP_DIR/${history_file%.*}_$(timestamp).json"
        else
            echo "Warning: $history_file not found in $DIR"
        fi
    done
}

main() {
    backup_histories
}

main "$@" 2>&1 # | tee "$LOG_FILE"

# notify -s "$0 finished" -m "History files backup completed"

# EOF
