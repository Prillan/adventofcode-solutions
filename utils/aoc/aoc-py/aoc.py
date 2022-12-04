import argparse
from datetime import datetime, timedelta
from dataclasses import dataclass
import json
import requests
import os
from pathlib import Path
import subprocess
from typing import Optional
import sys


class Missing:
    def __repr__(self):
        return "<missing>"


missing = Missing()


def identity_(self, x):
    return x


def optional(f=lambda x: x):
    def g(self, value):
        if value is not None:
            return f(value)
        else:
            return None

    return g


@dataclass
class Config:
    token: Optional[str]
    _token = optional()

    token_added_at: Optional[datetime]
    _token_added_at = optional(datetime.fromisoformat)

    version: str = "1"
    _version = identity_

    @property
    def token_age(self) -> timedelta:
        return datetime.now() - self.token_added_at

    def set(self, key, value):
        setattr(self, key, getattr(self, f"_{key}")(value))

    @classmethod
    def from_dict(cls, data: dict) -> "Config":
        c = cls.default()
        for k, v in data.items():
            c.set(k, v)
        return c

    def to_dict(self) -> dict:
        return {
            "version": self.version,
            "token": self.token,
            "token_added_at": dt.isoformat() if (dt := self.token_added_at) else None,
        }

    @staticmethod
    def default():
        return Config(
            version=Config.version,
            token=None,
            token_added_at=None,
        )


def config_file():
    home = Path("~").expanduser()
    return home / ".config" / "advent.json"


def load_config():
    c = config_file()
    if c.exists():
        with c.open() as f:
            return Config.from_dict(json.load(f))

    return Config.default()


def write_config(c):
    with config_file().open("w") as f:
        json.dump(c.to_dict(), f, indent=2)


class Client:
    def __init__(self, token: str):
        self.session = requests.Session()
        self.session.cookies["session"] = token

    def input(self, year, day) -> str:
        return self.session.get(f"https://adventofcode.com/{year}/day/{day}/input").text


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--no-fetch-input", default=False, action="store_true")

    subparsers = parser.add_subparsers(dest="command")
    shell_parser = subparsers.add_parser("shell")
    shell_parser.add_argument(
        # TODO: Get choices from somewhere else.
        "--language",
        "-l",
        choices=["hs", "asm", "koka", "nix"],
        default="hs",
    )
    shell_parser.add_argument("day", type=int)
    shell_parser.add_argument("year", type=int, default=2022, nargs="?")

    config_parser = subparsers.add_parser("config")
    config_parser.add_argument("--token", type=str, default=missing)

    args = parser.parse_args()

    if args.command == "shell":
        exec_shell(
            language=args.language,
            day=args.day,
            year=args.year,
        )
    elif args.command == "config":
        set_config(actions=args)


def exec_shell(language: str, day: int, year: int):
    config = load_config()
    if config.token_age >= timedelta(days=30):
        print("WARN: Old token, requests might fail", file=sys.stderr)
    client = Client(config.token)

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

    input_file = target_dir / "input.txt"
    if not input_file.exists():
        print("INFO: Fetching missing input... ", file=sys.stderr, end="")
        input_file.write_text(client.input(year, day))
        print("Done!", file=sys.stderr)

    # EXEC
    os.chdir(target_dir)
    os.execvp(
        "nix", ["nix", "develop", "--quiet", "--no-warn-dirty", f".#langs.{language}"]
    )


def set_config(actions):
    c = load_config()

    if actions.token is not missing:
        c.token = actions.token
        c.token_added_at = datetime.now()

    write_config(c)


if __name__ == "__main__":
    main()
