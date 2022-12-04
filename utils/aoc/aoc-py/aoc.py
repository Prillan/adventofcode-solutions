import argparse
import requests
import os
from pathlib import Path
import subprocess


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        # TODO: Get choices from somewhere else.
        "--language",
        "-l",
        choices=["hs", "asm", "koka", "nix"],
        default="hs",
    )
    parser.add_argument("day", type=int)
    parser.add_argument("year", type=int, default=2022, nargs="?")

    args = parser.parse_args()

    exec_shell(
        language=args.language,
        day=args.day,
        year=args.year,
    )


def exec_shell(language: str, day: int, year: int):
    print(f"Setting up day {day} ({year}, {language})")
    extension = subprocess.run(
        ["nix", "eval", "--raw", f".#lib.x86_64-linux.langs.{language}.extension"],
        capture_output=True,
    ).stdout.decode()

    target_dir = Path(f"{year}/day{day}")
    target_dir.mkdir(parents=True, exist_ok=True)

    run = target_dir / f"run.{extension}"
    if not run.exists():
        template = Path("template") / run.name
        run.write_text(template.read_text())

    os.chdir(target_dir)

    os.execvp(
        "nix", ["nix", "develop", "--quiet", "--no-warn-dirty", f".#langs.{language}"]
    )


if __name__ == "__main__":
    main()
