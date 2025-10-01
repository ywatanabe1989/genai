#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2025-10-02 00:12:02 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/genai/genai_claude.py
# ----------------------------------------
from __future__ import annotations
import os
__FILE__ = (
    "./genai_claude.py"
)
__DIR__ = os.path.dirname(__FILE__)
# ----------------------------------------

import subprocess
import tempfile

"""Provides an interface for interacting with GenAI APIs using genai_claude.sh.

This script handles:
- Loading and formatting chat histories
- Loading prompt templates
- Making API calls via genai_claude.sh
- Updating and saving conversation histories

Key features:
- Uses genai_claude.sh for API calls
- Templated prompts system
- Conversation history management
- Command line interface
"""

import argparse
import warnings

from scitex.io import glob as scitex_io_glob
from scitex.io import load as scitex_io_load
from scitex.io import save as scitex_io_save
from scitex.path import split as scitex_path_split

## Parameters
TEMPLATE_DIR = scitex_path_split(__file__)[0] + "./templates/"
GENERAL_INSTRUCTION = scitex_io_load(os.path.join(TEMPLATE_DIR, "General.md"))


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

    # Prepare conversation context
    conversation_context = _prepare_conversation_context(ai_history, n_history)

    # AI prompt = general_instruction + template + prompt
    prompt_template = _get_template(template_type)
    _prompt_embedded = prompt_template.replace("PLACEHOLDER", prompt)
    ai_prompt = GENERAL_INSTRUCTION + _prompt_embedded

    # Call genai_claude.sh
    llm_out = _call_genai_claude(
        ai_prompt, conversation_context, max_tokens, temperature
    )
    print(llm_out)

    # Update chat histories
    _save_updated_human_history(
        human_history, human_history_path, template_type, prompt, llm_out
    )
    _save_human_readable_history(human_history, human_history_path)
    _save_updated_ai_history(ai_history, ai_history_path, ai_prompt, llm_out)


# ------------------------------
# Helper Functions
# ------------------------------
def _handle_prompt_and_prompt_file(prompt, prompt_file):
    if (not prompt) and (not prompt_file):
        prompt = ""
    if prompt_file:
        prompt = str(prompt) + "\n\n" + "".join(scitex_io_load(prompt_file))
    return prompt


def _load_histories(human_history_path, ai_history_path):
    def _load_or_create_history(history_path):
        try:
            history = scitex_io_load(history_path)
        except Exception as e_:
            warnings.warn(
                str(e_) + f"\nCreating new history file: {history_path}"
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


def _prepare_conversation_context(ai_history, n_history):
    # Get last n_history entries
    recent_history = ai_history[-n_history:] if ai_history else []
    return recent_history


def _call_genai_claude(prompt, conversation_context, max_tokens, temperature):
    """Call genai_claude.sh with conversation history formatted in prompt."""
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".txt", delete=False
    ) as temp_file:
        # Format conversation history as part of the prompt
        for entry in conversation_context:
            role = "Human" if entry["role"] == "user" else "Assistant"
            temp_file.write(f"{role}: {entry['content']}\n\n")

        # Add current prompt
        temp_file.write(f"Human: {prompt}\n")
        prompt_file = temp_file.name

    try:
        # Use -i flag to specify input file
        cmd = ["genai_claude.sh", "-i", prompt_file]

        result = subprocess.run(
            cmd, capture_output=True, text=True, check=True
        )
        return result.stdout.strip()

    except subprocess.CalledProcessError as e:
        print(f"Error: {e.stderr}")
        return ""
    finally:
        if os.path.exists(prompt_file):
            os.unlink(prompt_file)


def load_templates():
    TEMPLATE_DIR = scitex_path_split(__file__)[0] + "./templates/"
    TEMPLATE_PATHS = scitex_io_glob(TEMPLATE_DIR.replace("/./", "/") + "*")
    TEMPLATE_NAMES = [
        "".join(scitex_path_split(tp_)[1:]) for tp_ in TEMPLATE_PATHS
    ]
    TEMPLATES = {}
    for lpath_, fname_ in zip(TEMPLATE_PATHS, TEMPLATE_NAMES):
        template_type = fname_.split(".")[0]
        prompt = scitex_io_load(lpath_, verbose=False)
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
    scitex_io_save(human_history, human_history_path, verbose=False)


def _save_updated_ai_history(
    ai_history, ai_history_path, user_prompt, assistant_response
):
    ai_history.append({"role": "user", "content": user_prompt})
    ai_history.append({"role": "assistant", "content": assistant_response})
    ai_history = _format_history(ai_history)
    scitex_io_save(ai_history, ai_history_path, verbose=False)


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
    scitex_io_save(
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
        default=scitex_path_split(__file__)[0] + "./history-human-secret.json",
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
python /home/ywatanabe/.emacs.d/lisp/genai/genai_claude.py --prompt hi
"""


# Issue: genai.el does not handle permissions or file path relaying failed

# | CLAUDE-SONNET-4-20250514

# Running /home/ywatanabe/.bin/llm/genai_claude.sh...
#     Full prompt file: /tmp/claude_1759043585_1055192/prompt.txt
# I cannot access the file referenced in the prompt.txt file. The path `/tmp/tmp3e1cnecg.txt` requires permission that hasn't been granted. Could you either:

# 1. Grant permission to read that file, or
# 2. Provide the actual prompt content directly?

# EOF
