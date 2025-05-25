#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-26 08:07:52 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/genai/genai_claude.py
# ----------------------------------------
import os
__FILE__ = (
    "./genai_claude.py"
)
__DIR__ = os.path.dirname(__FILE__)
# ----------------------------------------

import subprocess
import sys

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

## Parameters
TEMPLATE_DIR = mngs_path_split(__file__)[0] + "./templates/"

GENERAL_INSTRUCTION = mngs_io_load(os.path.join(TEMPLATE_DIR, "General.md"))


# ------------------------------
# Main Function
# ------------------------------
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
    # Prompt
    prompt = _handle_prompt_and_prompt_file(prompt, prompt_file)

    # Params
    ai_history_path = human_history_path.replace("human", "ai")

    # Load histories
    human_history, ai_history = _load_histories(
        human_history_path, ai_history_path
    )

    # Model initialization
    llm = mngs_ai_GenAI(
        model=engine,
        api_key=api_keys,
        stream=True,
        n_keep=n_history,
        max_tokens=max_tokens,
        temperature=temperature,
    )

    # Trim histories
    [llm.update_history(**_history) for _history in ai_history[-n_history:]]

    # AI prompt = general_instruction + template + prompt
    _prompt_template = _get_template(template_type)
    _prompt_embedded = _prompt_template.replace("PLACEHOLDER", prompt)
    ai_prompt = GENERAL_INSTRUCTION + _prompt_embedded

    # Main
    # # Replace this line in run_genai function:
    # llm_out = subprocess.run(
    #     ["claude", "-p", ai_prompt], capture_output=True, text=True
    # ).stdout.strip()

    # Replace the subprocess call:
    process = subprocess.Popen(
        ["claude", "-p", ai_prompt],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    llm_out = ""
    for line in iter(process.stdout.readline, ""):
        print(line, end="")
        sys.stdout.flush()
        llm_out += line

    process.wait()

    # llm_out = llm(ai_prompt)

    # Update chat histories
    _save_updated_human_history(
        human_history, human_history_path, template_type, prompt, llm_out
    )
    _save_human_readable_history(human_history, human_history_path)
    # _save_updated_ai_history(ai_history, ai_history_path, llm)
    _save_updated_ai_history_claude(
        ai_history, ai_history_path, prompt, llm_out
    )


# ------------------------------
# Helper Functions
# ------------------------------
def _handle_prompt_and_prompt_file(prompt, prompt_file):
    if (not prompt) and (not prompt_file):
        prompt = ""

    if prompt_file:
        prompt = str(prompt) + "\n\n" + "\n".join(mngs_io_load(prompt_file))

    return prompt


def _load_histories(human_history_path, ai_history_path):
    def _load_or_create_history(history_path):
        try:
            history = mngs_io_load(history_path)
        except Exception as e:
            warnings.warn(
                str(e) + f"\nCreating new history file: {history_path}"
            )
            history = []
        return _format_history(history)

    human_history = _load_or_create_history(human_history_path)
    ai_history = _load_or_create_history(ai_history_path)
    return human_history, ai_history


def _format_history(ai_history):
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


def _save_updated_human_history(
    human_history, human_history_path, template_type, prompt, llm_out
):
    human_history.append(
        {
            "role": f"user (Template: {str(template_type)})",
            "content": prompt,
        }
    )
    human_history.append({"role": "assistant", "content": llm_out})
    human_history = _format_history(human_history)
    mngs_io_save(human_history, human_history_path, verbose=False)


# def _save_updated_ai_history(ai_history, ai_history_path, model):
#     n_new_history = 2
#     for history in model.history[-n_new_history:]:
#         ai_history.append(history)
#     ai_history = _format_history(ai_history)
#     mngs_io_save(ai_history, ai_history_path, verbose=False)
# Add this new function:
def _save_updated_ai_history_claude(
    ai_history, ai_history_path, user_prompt, assistant_response
):
    ai_history.append({"role": "user", "content": user_prompt})
    ai_history.append({"role": "assistant", "content": assistant_response})
    ai_history = _format_history(ai_history)
    mngs_io_save(ai_history, ai_history_path, verbose=False)


def _get_template(template_type):
    TEMPLATES = load_templates()
    if str(template_type) == "None":
        template_type = ""
    return TEMPLATES.get(template_type, f"{template_type}\nPLACEHOLDER")


def _save_human_readable_history(
    human_history, human_history_path, n_interactions=None
):
    # select last n_interactions if specified
    human_history = (
        human_history[-n_interactions:]
        if n_interactions and n_interactions > 0
        else human_history
    )

    # prepare separator
    separator = "=" * 60

    # Format human history in a readable manner
    human_readable_history_str_list = [
        f"\n\n{separator}\n\n"
        + entry["role"].replace("user", "YOU").replace("assistant", "GENAI")
        + "\n\n"
        + entry["content"]
        + "\n"
        for entry in human_history
    ]

    # Saving as markdown
    human_readable_history_str = "".join(human_readable_history_str_list)
    human_readable_history_path = human_history_path.replace(
        "human", "human-readable"
    ).replace(".json", ".md")
    mngs_io_save(
        human_readable_history_str, human_readable_history_path, verbose=False
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="")

    # Prompt
    # --------------------
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

    parser.add_argument(
        "--template_type",
        type=str,
        default="",
        help="(default: %(default)s)",
    )

    # API
    # --------------------
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

    # LLM Parameters
    # --------------------
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

    # History
    # --------------------
    parser.add_argument(
        "--n_history",
        type=int,
        default=5,
        help="(default: %(default)s)",
    )

    parser.add_argument(
        "--human_history_path",
        type=str,
        default=mngs_path_split(__file__)[0] + "./history-human-secret.json",
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
