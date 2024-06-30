#!./env/bin/python3
# -*- coding: utf-8 -*-
# Time-stamp: "2024-06-15 16:19:20 (ywatanabe)"
# /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-genai/organize-and-show-history.py


"""
This script does XYZ.
"""


"""
Imports
"""
import os
import re
import sys
import warnings
from glob import glob
from pprint import pprint

import matplotlib
import matplotlib.pyplot as plt
import mngs
import numpy as np
import pandas as pd
import seaborn as sns
import torch
import torch.nn as nn
import torch.nn.functional as F
import xarray as xr
from icecream import ic
from mngs.io import load as mngs_io_load
from natsort import natsorted
from tqdm import tqdm

# sys.path = ["."] + sys.path
# from scripts import utils, load

"""
Warnings
"""
# warnings.simplefilter("ignore", UserWarning)


"""
Config
"""
# CONFIG = mngs.gen.load_configs()


"""
Functions & Classes
"""
# PATHs
__file__ = "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-genai/emacs-genai.py"


def print_splitter(role=None):
    print(f"\n\n{'='* 60}\n")
    if role:
        print(f"{role}\n")


def main(human_history_path):
    history = mngs_io_load(human_history_path)

    for entry in history:
        print_splitter(
            entry["role"].replace("user", "YOU").replace("assistant", "GENAI")
        )
        print(entry["content"])


if __name__ == "__main__":
    # Argument Parser
    import argparse

    parser = argparse.ArgumentParser(description="")
    parser.add_argument(
        "--human_history_path",
        type=str,
        default=mngs.path.split(__file__)[0] + "./history-human-secret.json",
        help="(default: %(default)s)",
    )

    # parser.add_argument('--var', '-v', type=int, default=1, help='')
    # parser.add_argument('--flag', '-f', action='store_true', default=False, help='')
    args = parser.parse_args()

    # Main
    # CONFIG, sys.stdout, sys.stderr, plt, CC = mngs.gen.start(
    #     sys, plt, verbose=False
    # )
    main(human_history_path=args.human_history_path)
    # mngs.gen.close(CONFIG, verbose=False, notify=False)

# EOF
