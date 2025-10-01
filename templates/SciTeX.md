<!-- ---
!-- Timestamp: 2025-09-21 22:40:50
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/genai/templates/SciTeX.md
!-- --- -->

----------
Background
----------
# Your Role
You are an experienced programmer.

# My Request
- Make the scripts to follow the SciTeX Template below.
- Implement docstrings baed on actual implementations
- Implement `main()` to demonstrate actual implementations of the file
- DO NOT MODIFY THE `run_main()` FUNCTION
  - This handles:
    - 1. stdout/stderr logging
    - 2. configuration using `CONFIG = stx.io.load_configs()`
    - 3. Link produced outputs to a traciable place and add symlink for centralized navigation and reference
  - For example, if the script is `/path/to/script.py` outputs will be saved to `/path/to/script_out/` directory automatically
  - Especially, `stx.io.save(obj, ./relative/path.ext, symlink_from_cwd=True)` will:
    - 1. Save the object to `/path/to/script_out/relative/path.ext`
    - 2. Symlink to `<current-working-directory>/relative/path.ext` -> `/path/to/script_out/relative/path.ext`
- Even when no argparse needed, keep the format and just fill the description
  - This enables always check by `-h|--help` option to get the large picture
- Always add type hints and docstrings

## Script Template

SCITEX PYTHON SCRIPT MUST STRICTLY FOLLOW THIS STANDARD FORMAT:

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2024-11-03 10:33:13 (ywatanabe)"
# File: placeholder.py

__FILE__ = "placeholder.py"

"""
Functionalities:
  - Does XYZ
  - Does XYZ
  - Does XYZ
  - Saves XYZ

Dependencies:
  - scripts:
    - /path/to/script1
    - /path/to/script2
  - packages:
    - package1
    - package2
IO:
  - input-files:
    - /path/to/input/file.xxx
    - /path/to/input/file.xxx

  - output-files:
    - /path/to/input/file.xxx
    - /path/to/input/file.xxx

(Remove me: Please fill docstrings above, while keeping the bulette point style, and remove this instruction line)
"""

"""Imports"""
import os
import sys
import argparse
import scitex as stx
from scitex import logging

logger = logging.getLogger(__name__)

# (Remove this Warnings section if not needed)
"""Warnings"""
# stx.pd.ignore_SettingWithCopyWarning()
# warnings.simplefilter("ignore", UserWarning)
# with warnings.catch_warnings():
#     warnings.simplefilter("ignore", UserWarning)

# (Remove this Parameters section if not needed)
"""Parameters"""
# CONFIG = stx.io.load_configs()

"""Functions & Classes"""
def main(args):
    # IMPLEMENT this function to demonstrate this script
    return 0

import argparse
def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    import scitex as stx
    parser = argparse.ArgumentParser(description='')
    # (Remove the following variables as they are just examples)
    # parser.add_argument(
    #     "--var",
    #     "-v",
    #     type=int,
    #     choices=None,
    #     default=1,
    #     help="(default: %(default)s)",
    # )
    # parser.add_argument(
    #     "--flag",
    #     "-f",
    #     action="store_true",
    #     default=False,
    #     help="(default: %%(default)s)",
    # )
    args = parser.parse_args()
    return args

def run_main() -> None:
    """Initialize scitex framework, run main function, and cleanup."""
    global CONFIG, CC, sys, plt, rng

    import sys
    import matplotlib.pyplot as plt
    import scitex as stx

    args = parse_args()

    CONFIG, sys.stdout, sys.stderr, plt, CC, rng = stx.session.start(
        sys,
        plt,
        args=args,
        file=__FILE__,
        sdir_suffix=None,
        verbose=False,
        agg=True,
    )

    exit_status = main(args)

    stx.session.close(
        CONFIG,
        verbose=False,
        notify=False,
        message="",
        exit_status=exit_status,
    )

if __name__ == '__main__':
    run_main()

# EOF
```

----------
Now, the scripts to revise are as follows:
----------
PLACEHOLDER

<!-- EOF -->