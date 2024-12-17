#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "2024-12-18 08:31:38 (ywatanabe)"
# File: ./genai/genai.py

__file__ = "/home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/genai.py"

"""
Provides an interface for interacting with GenAI APIs (e.g., Gemini).

This script handles:
- Loading and formatting chat histories
- Loading prompt templates
- Making API calls with configurable parameters
- Updating and saving conversation histories

Key features:
- Supports multiple API keys and engines
- Templated prompts system
- Conversation history management
- Streaming responses
- Command line interface
"""

import argparse
import warnings

from mngs.ai import GenAI as mngs_ai_GenAI
from mngs.io import glob as mngs_io_glob
from mngs.io import load as mngs_io_load
from mngs.io import save as mngs_io_save
from mngs.path import split as mngs_path_split
from mngs.str import printc

## Parameters
GENERAL_INSTRUCTION = f"""
########################################
## General Instruction
########################################
I am busy. So,

- Please avoid unnecessary messages.
- Keep your output minimal.
- When programming code is provided, please concentrate on differences between my input and your output; always be concise and stick to the point.
- However, do not skip any lines of code as I will use your output as they are, even when your code is long, do not care about it. In such a case I will request you to continue afterwards.
########################################
"""


## Functions
def load_histories(human_history_path, ai_history_path):
    try:
        human_history = mngs_io_load(human_history_path)
    except Exception as e:
        warnings.warn(
            str(e) + f"\nCreating new history file: {human_history_path}"
        )
        human_history = []

    try:
        ai_history = mngs_io_load(ai_history_path)
    except Exception as e:
        warnings.warn(
            str(e) + f"\nCreating new history file: {ai_history_path}"
        )
        ai_history = []

    human_history = format_history(human_history)
    ai_history = format_history(ai_history)
    return human_history, ai_history


def format_history(ai_history):
    formatted = []
    for item in ai_history:
        role = item["role"]
        text = item.get("content")
        parts = item.get("parts")
        if parts:
            text = text or parts[0].get("text")
        formatted.append({"role": role, "content": text})
    return formatted


def load_templates():
    TEMPLATE_DIR = mngs_path_split(__file__)[0] + "./templates/"
    TEMPLATE_PATHS = mngs_io_glob(TEMPLATE_DIR.replace("/./", "/") + "*")

    TEMPLATE_NAMES = [
        "".join(mngs_path_split(tp)[1:]) for tp in TEMPLATE_PATHS
    ]

    TEMPLATES = {}
    for lpath, fname in zip(TEMPLATE_PATHS, TEMPLATE_NAMES):
        template_type = fname.split(".")[0]
        prompt = mngs_io_load(lpath, verbose=False)
        TEMPLATES[template_type] = prompt

    return TEMPLATES


def update_human_history(
    human_history, human_history_path, template_type, prompt, model_out
):
    human_history.append(
        {
            "role": f"user (Template: {str(template_type)})",
            "content": prompt,
        }
    )
    human_history.append({"role": "assistant", "content": model_out})
    human_history = format_history(human_history)
    mngs_io_save(human_history, human_history_path, verbose=False)


def update_ai_history(ai_history, ai_history_path, model):
    n_new_history = 2
    for history in model.history[-n_new_history:]:
        ai_history.append(history)
    ai_history = format_history(ai_history)
    mngs_io_save(ai_history, ai_history_path, verbose=False)


def determine_template(template_type):
    TEMPLATES = load_templates()
    if str(template_type) == "None":
        template_type = ""
    return TEMPLATES.get(template_type, f"{template_type}\nPLACEHOLDER")


def run_genai(
    api_keys,
    engine,
    max_tokens,
    temperature,
    human_history_path,
    template_type,
    n_history,
    prompt,
    prompt_file,
):

    if (not prompt) and (not prompt_file):
        print("Please input prompt\n")
        return

    if prompt_file:
        prompt = (
            str(prompt).strip()
            + "\n\n"
            + str("\n".join(mngs_io_load(prompt_file))).strip()
        )

    if prompt.strip() == "":
        print("Please input prompt\n")
        return

    else:
        # Handle histories
        ai_history_path = human_history_path.replace("human", "ai")
        human_history, ai_history = load_histories(
            human_history_path, ai_history_path
        )

        # Model initialization
        model = mngs_ai_GenAI(
            model=engine,
            api_key=api_keys,
            stream=True,
            n_keep=n_history,
            max_tokens=max_tokens,
            temperature=temperature,
        )
        [
            model.update_history(**_history)
            for _history in ai_history[-n_history:]
        ]

        # AI prompt = general_instruction + template + prompt
        ai_prompt = GENERAL_INSTRUCTION + determine_template(template_type).replace("PLACEHOLDER", prompt)

        # print("----------------------------------------")
        # print(f"AI Prompt: {ai_prompt}")
        # print("----------------------------------------")

        # Main
        model_out = model(ai_prompt)

        # print("----------------------------------------")
        # print(f"Model OUTPUT: {model_out}")
        # print("----------------------------------------")


        # Update chat histories
        update_human_history(
            human_history, human_history_path, template_type, prompt, model_out
        )
        update_ai_history(ai_history, ai_history_path, model)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="")
    parser.add_argument(
        "--api_key",
        type=str,
        action="append",
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--engine",
        type=str,
        default="gemini-2.0-flash-exp",
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--max_tokens",
        type=int,
        default=4096,
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--temperature",
        type=int,
        default=0,
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--human_history_path",
        type=str,
        default=mngs_path_split(__file__)[0] + "./history-human-secret.json",
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--n_history",
        type=int,
        default=5,
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--template_type",
        type=str,
        default="",
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--prompt",
        type=str,
        default="",
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--prompt_file",
        type=str,
        default="",
        help="(default: %(default)s)",
    )

    args = parser.parse_args()

    run_genai(
        api_keys=args.api_key,
        engine=args.engine,
        max_tokens=args.max_tokens,
        temperature=args.temperature,
        human_history_path=args.human_history_path,
        template_type=args.template_type,
        n_history=args.n_history,
        prompt=args.prompt,
        prompt_file=args.prompt_file,
    )


"""
python ./genai/genai.py
python -m genai
"""

# EOF
