#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2025-04-27 10:41:38 (ywatanabe)"
# File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/show-history.py
# ----------------------------------------
import os
__FILE__ = (
    "./show-history.py"
)
__DIR__ = os.path.dirname(__FILE__)
# ----------------------------------------

import argparse
import json

from mngs.path import split as mngs_path_split

# def convert_json_to_text(input_path, n_interactions=-1):
#     try:
#         with open(input_path, "r") as f:
#             history = json.load(f)
#     except Exception as e:
#         print(e)
#         print(input_path)
#         print(n_interactions)
#         raise ValueError(f"Not loaded {input_path}")

#     if not n_interactions:
#         history = history
#     else:
#         history = history[-int(n_interactions) :]

#     for entry in history:
#         role = (
#             entry["role"].replace("user", "YOU").replace("assistant", "GENAI")
#         )
#         print(f"\n\n{'='*60}\n\n{role}\n\n{entry['content']}\n")


def convert_json_to_text(input_path, n_interactions=None):
    # load JSON history
    with open(input_path, "r") as f:
        history = json.load(f)

    # select last n_interactions if specified
    history = (
        history[-n_interactions:]
        if n_interactions and n_interactions > 0
        else history
    )

    # prepare separator
    separator = "=" * 60

    # build output parts
    output_parts = [
        f"\n\n{separator}\n\n"
        + entry["role"].replace("user", "YOU").replace("assistant", "GENAI")
        + "\n\n"
        + entry["content"]
        + "\n"
        for entry in history
    ]

    # print all at once
    print("".join(output_parts))


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
        help="Number of interactions to show",
    )
    args = parser.parse_args()

    convert_json_to_text(args.human_history_path, args.n_interactions)

# EOF