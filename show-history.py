#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "2024-10-30 11:12:38 (ywatanabe)"
# File: genai/show-history.py

import json
import argparse
from mngs.path import split as mngs_path_split

# __file__ = "/home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/show-history.py"

def convert_json_to_text(input_path, n_interactions=-1):
    try:
        with open(input_path, 'r') as f:
            history = json.load(f)
    except Exception as e:
        print(e)
        print(input_path)
        print(n_interactions)
        raise ValueError(f"Not loaded {input_path}")

    if not n_interactions:
        history = history
    else:
        history = history[-int(n_interactions):]

    for entry in history:
        role = entry["role"].replace("user", "YOU").replace("assistant", "GENAI")
        print(f"\n\n{'='*60}\n\n{role}\n\n{entry['content']}\n")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="")
    parser.add_argument(
        "--human_history_path",
        type=str,
        default=mngs_path_split(__file__)[0] + "./history-human-secret.json",
        help="(default: %(default)s)",
    )
    parser.add_argument(
        "--n_interactions",
        type=int,
        required=False,
        help="Number of interactions to show"
    )
    args = parser.parse_args()

    convert_json_to_text(args.human_history_path, args.n_interactions)


# EOF
