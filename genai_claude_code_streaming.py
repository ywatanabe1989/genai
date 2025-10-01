#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2025-10-02 01:27:10 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/genai/genai_claude_code_streaming.py
# ----------------------------------------
from __future__ import annotations
import os
__FILE__ = (
    "./genai_claude_code_streaming.py"
)
__DIR__ = os.path.dirname(__FILE__)
# ----------------------------------------

import json
import subprocess
import sys
import tempfile

"""
Claude Code streaming with history management
Combines streaming capabilities with conversation history tracking
"""

import shlex
import warnings
from logging import getLogger

from scitex.io import glob as scitex_io_glob
from scitex.io import load as scitex_io_load
from scitex.io import save as scitex_io_save
from scitex.path import split as scitex_path_split

logger = getLogger(__name__)

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
    exit_code, llm_out = _call_claude_code_streaming(
        ai_prompt, engine, conversation_context, max_tokens, temperature
    )

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


# ------------------------------
# Streaming Function
# ------------------------------
def _call_claude_code_streaming(
    prompt, model, conversation_context, max_tokens, temperature
):
    """Call claude CLI with conversation history and streaming output."""
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".txt", delete=False
    ) as temp_file:
        for entry in conversation_context:
            role = "Human" if entry["role"] == "user" else "Assistant"
            temp_file.write(f"{role}: {entry['content']}\n\n")
        temp_file.write(f"Human: {prompt}\n")
        prompt_file = temp_file.name

    final_prompt = (
        f"("
        f"Read the prompt written in {prompt_file} and "
        f"purely respond to the contents of the file, "
        f"ignoreing this prompt itself"
        f")"
    )

    cmd = [
        "claude",
        "--dangerously-skip-permissions",
        "--print",
        "--output-format",
        "stream-json",
        "--verbose",
        "--model",
        model,
        final_prompt,
    ]

    try:
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            # bufsize=1, # working but in chunk
            bufsize=0,
            universal_newlines=True,
        )

        full_output = []

        for line in iter(process.stdout.readline, ""):
            if line:
                line = line.rstrip()
                if '"type":"assistant"' in line and '"type":"text"' in line:
                    try:
                        data = json.loads(line)
                        if data.get("type") == "assistant":
                            message = data.get("message", {})
                            content = message.get("content", [])
                            for item in content:
                                if item.get("type") == "text":
                                    text = item.get("text", "")
                                    sys.stdout.write(text)
                                    sys.stdout.flush()
                                    full_output.append(text)
                    except json.JSONDecodeError:
                        print(line)
                        full_output.append(line)

        exit_code = process.wait()
        stderr_output = process.stderr.read()
        if stderr_output:
            logger.error(f"Stderr: {stderr_output}")

        return exit_code, "".join(full_output)

    except Exception as e:
        return 1, str(e)
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
    import argparse

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
        default="sonnet",
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

# EOF
