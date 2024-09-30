#!/usr/bin/env python3
import json
import argparse
import mngs

def convert_json_to_text(input_path, output_path, n_interactions):
    with open(input_path, 'r') as f:
        history = json.load(f)

    if n_interactions:
        if n_interactions == -1:
            history = history
        else:
            history = history[-n_interactions:]

    with open(output_path, 'w') as f:
        for entry in history:
            role = entry["role"].replace("user", "YOU").replace("assistant", "GENAI")
            f.write(f"\n\n{'='*60}\n\n{role}\n\n{entry['content']}\n")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="")
    parser.add_argument(
        "--human_history_path",
        type=str,
        default=mngs.path.split(__file__)[0] + "./history-human-secret.json",
        help="(default: %(default)s)",
    )
    parser.add_argument(
        "--output",
        type=str,
        required=True,
        help="Output text file path"
    )
    parser.add_argument(
        "--n_interactions",
        type=int,
        required=False,
        help="Number of interactions to show"
    )
    args = parser.parse_args()

    convert_json_to_text(args.human_history_path, args.output, args.n_interactions)
