#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2026-01-08 02:05:49 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/genai/genai.py


import os

import json
import subprocess
import sys
import tempfile
from pathlib import Path

"""
Claude Code streaming with history management
Combines streaming capabilities with conversation history tracking

SECURITY IMPROVEMENTS:
- Uses list format for subprocess calls (shell=False)
- Validates model names against whitelist
- Sanitizes file paths to prevent path traversal
- Adds timeout to prevent hanging processes
- Validates JSON input
"""

import warnings
from logging import getLogger
from scitex.io import load as scitex_io_load
from scitex.io import save as scitex_io_save
from scitex.path import split as scitex_path_split

logger = getLogger(__name__)

## Parameters
TEMPLATE_DIR = scitex_path_split(__file__)[0] / "templates"
GENERAL_INSTRUCTION = scitex_io_load(TEMPLATE_DIR / "General.md")

# Security: Whitelist of allowed model names
ALLOWED_MODELS = {
    "sonnet",
    "claude-sonnet-4-5-20250929",
    "claude-sonnet-4-20250514",
    "claude-opus-4-20250514",
    "opus",
}

# Security: Maximum timeout for subprocess (in seconds)
SUBPROCESS_TIMEOUT = 300


# ------------------------------
# Security Helper Functions
# ------------------------------
def _validate_model_name(model: str) -> str:
    """
    Validate model name against whitelist.

    Raises:
        ValueError: If model name is not in whitelist
    """
    if model not in ALLOWED_MODELS:
        raise ValueError(
            f"Invalid model name: {model}. Allowed models: {', '.join(ALLOWED_MODELS)}"
        )
    return model


ALLOWED_DIRECTORIES = [
    "/tmp",
    "/var/tmp",
    os.path.expanduser("~/.cache/genai"),
    os.getcwd(),
]


def _sanitize_path(path: str) -> str:
    """
    Sanitize file path to prevent path traversal attacks.

    Allows:
    - Temporary files (/tmp, /var/tmp)
    - Files in ~/.cache/genai
    - Relative paths without '..'

    Blocks:
    - Path traversal with '..'
    - Absolute paths outside allowed directories

    Raises:
        ValueError: If path is potentially dangerous
    """
    normalized = os.path.normpath(os.path.expanduser(path))

    # 許可されたディレクトリ
    allowed_prefixes = [
        tempfile.gettempdir(),
        "/var/tmp",
        os.path.expanduser("~/.cache/genai"),
        os.getcwd(),
    ]

    # 絶対パスの場合、許可されたディレクトリ内かチェック
    if os.path.isabs(normalized):
        for prefix in allowed_prefixes:
            if normalized.startswith(os.path.abspath(prefix)):
                return normalized
        raise ValueError(
            f"Absolute path outside allowed directories: {path}\n"
            f"Allowed: {', '.join(allowed_prefixes)}"
        )

    # 相対パスの場合、'..'をチェック
    if ".." in Path(path).parts:
        raise ValueError(f"Path traversal detected: {path}")

    return normalized


def _validate_temperature(temperature: float) -> float:
    """Validate temperature parameter."""
    if not 0 <= temperature <= 2:
        raise ValueError(f"Temperature must be between 0 and 2, got {temperature}")
    return temperature


def _validate_max_tokens(max_tokens: int) -> int:
    """Validate max_tokens parameter."""
    if not 1 <= max_tokens <= 200000:
        raise ValueError(f"max_tokens must be between 1 and 200000, got {max_tokens}")
    return max_tokens


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
    # Validate inputs
    engine = _validate_model_name(engine)
    max_tokens = _validate_max_tokens(max_tokens)
    temperature = _validate_temperature(temperature)

    # Prompt
    prompt = _handle_prompt_and_prompt_file(prompt, prompt_file)

    # Params - ensure string for .replace() compatibility
    human_history_path = str(human_history_path)
    ai_history_path = human_history_path.replace("human", "ai")

    # Load histories
    human_history, ai_history = _load_histories(human_history_path, ai_history_path)

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
def _prepare_temp_settings_json(tmp_settings_path):
    permissions = {
        "permissions": {
            "allow": [
                "Read(**)",
                "Write(**)",
                "Bash(*)",
            ],
            "deny": [],
        },
    }
    scitex_io_save(permissions, tmp_settings_path, verbose=False)
    return tmp_settings_path


def _handle_prompt_and_prompt_file(prompt, prompt_file):
    if (not prompt) and (not prompt_file):
        prompt = ""
    if prompt_file:
        # Security: Sanitize prompt_file path
        try:
            prompt_file = _sanitize_path(prompt_file)
        except ValueError as e:
            logger.error(f"Invalid prompt file path: {e}")
            raise
        prompt = str(prompt) + "\n\n" + "".join(scitex_io_load(prompt_file))
    return prompt


def _load_histories(human_history_path, ai_history_path):
    def _load_or_create_history(history_path):
        try:
            history = scitex_io_load(history_path)
            # Security: Validate JSON structure
            if not isinstance(history, list):
                raise ValueError("History must be a list")
            for item in history:
                if not isinstance(item, dict):
                    raise ValueError("History items must be dictionaries")
                if "role" not in item:
                    raise ValueError("History items must have 'role' key")
        except Exception as e_:
            warnings.warn(str(e_) + f"\nCreating new history file: {history_path}")
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
# Streaming Function (SECURITY HARDENED)
# ------------------------------
def _call_claude_code_streaming(
    prompt, model, conversation_context, max_tokens, temperature
):
    """
    Call claude CLI with conversation history and streaming output.

    SECURITY IMPROVEMENTS:
    - Uses list format for subprocess (shell=False)
    - Adds timeout to prevent hanging
    - Validates model name
    - Proper cleanup of temp files
    """
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".txt", delete=False
    ) as temp_file:
        for entry in conversation_context:
            role = "Human" if entry["role"] == "user" else "Assistant"
            temp_file.write(f"{role}: {entry['content']}\n\n")
        temp_file.write(f"Human: {prompt}\n")
        prompt_file = temp_file.name

    final_prompt = (
        f"--- THIS IS META PROMPT ---\n"
        f"1. The file {prompt_file} contains conversation history.\n"
        f"2. Read and understand the context.\n"
        f"3. Respond to the last Human message in that file.\n"
        f"4. Starts from `I'll read the conversation history file to understand the context.`\n"
        f"5. You have both read/write permissions and expected to work as an agent.\n"
        f"6. No need for permission check to read/write and rm for this temporal prompt file for this request.\n"
        f"--- META PROMPT ENDS ---\n"
    )

    logger.debug(os.getcwd())

    # Remove ANTHROPIC_API_KEY to force Claude Code to use its own auth
    os.environ.pop("ANTHROPIC_API_KEY", None)

    # Temporal settings.json
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".json", delete=False
    ) as settings_file:
        tmp_settings_path = settings_file.name

    _prepare_temp_settings_json(tmp_settings_path)

    # SECURITY: Use list format instead of string
    cmd = [
        "claude",
        "--print",
        "--verbose",
        "--output-format",
        "stream-json",
        "--dangerously-skip-permissions",
        "--settings",
        tmp_settings_path,
        "--model",
        model,
        "--",
        final_prompt,
    ]

    try:
        # SECURITY: shell=False (implicit default, but being explicit)
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=0,
            universal_newlines=True,
            # Note: timeout is handled via process.wait(timeout=...)
        )

        full_output = []

        # Read output with timeout awareness
        for line in iter(process.stdout.readline, ""):
            if line:
                if '"type":"assistant"' in line and '"type":"text"' in line:
                    try:
                        data = json.loads(line)
                        if data.get("type") == "assistant":
                            message = data.get("message", {})
                            content = message.get("content", [])
                            for item in content:
                                if item.get("type") == "text":
                                    text = item.get("text", "")
                                    if (
                                        text
                                        == "I'll read the conversation history file to understand the context."
                                    ):
                                        continue
                                    sys.stdout.write(text)
                                    sys.stdout.flush()
                                    full_output.append(text)
                    except json.JSONDecodeError as e:
                        logger.warning(f"JSON decode error: {e}")
                        print(line)
                        full_output.append(line)

        # SECURITY: Add timeout to wait
        try:
            exit_code = process.wait(timeout=SUBPROCESS_TIMEOUT)
        except subprocess.TimeoutExpired:
            logger.error(f"Process timed out after {SUBPROCESS_TIMEOUT} seconds")
            process.kill()
            process.wait()
            return 1, "Error: Process timed out"

        stderr_output = process.stderr.read()
        if stderr_output:
            logger.error(f"Stderr: {stderr_output}")

        return exit_code, "".join(full_output)

    except Exception as e:
        logger.error(f"Exception in _call_claude_code_streaming: {e}")
        return 1, str(e)
    finally:
        # SECURITY: Always cleanup temp files
        if os.path.exists(prompt_file):
            try:
                os.unlink(prompt_file)
            except Exception as e:
                logger.warning(f"Failed to delete {prompt_file}: {e}")
        if os.path.exists(tmp_settings_path):
            try:
                os.unlink(tmp_settings_path)
            except Exception as e:
                logger.warning(f"Failed to delete {tmp_settings_path}: {e}")


def load_templates():
    TEMPLATE_DIR = scitex_path_split(__file__)[0] / "templates"
    # Only load .md files, excluding directories and backup files
    TEMPLATE_PATHS = [
        p
        for p in TEMPLATE_DIR.glob("*.md")
        if p.is_file() and not p.name.startswith(".")
    ]
    TEMPLATES = {}
    for lpath_ in TEMPLATE_PATHS:
        template_type = lpath_.stem  # Get filename without extension
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
    human_readable_history_path = (
        str(human_history_path)
        .replace("human", "human-readable")
        .replace(".json", ".md")
    )
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
        type=float,
        default=0.0,
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
        default=str(scitex_path_split(__file__)[0] / "history-human-secret.json"),
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
