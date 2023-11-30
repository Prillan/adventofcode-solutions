import argparse
from collections import defaultdict
from datetime import datetime, timedelta, timezone
from dataclasses import dataclass
import json
import requests
import os
from pathlib import Path
import subprocess
from typing import Optional
import sys
import zoneinfo


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

    def leaderboard(self, year, id) -> dict:
        return self.session.get(
            f"https://adventofcode.com/{year}/leaderboard/private/view/{id}.json"
        ).json()

    @classmethod
    def from_config(cls, config: Config) -> "Client":
        if config.token_age >= timedelta(days=30):
            print("WARN: Old token, requests might fail", file=sys.stderr)
        return cls(config.token)


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
    shell_parser.add_argument("year", type=int, default=2023, nargs="?")

    config_parser = subparsers.add_parser("config")
    config_parser.add_argument("--token", type=str, default=missing)

    leaderboard_parser = subparsers.add_parser("leaderboard")
    leaderboard_parser.add_argument("id", type=int)

    args = parser.parse_args()

    if args.command == "shell":
        exec_shell(
            language=args.language,
            day=args.day,
            year=args.year,
        )
    elif args.command == "config":
        set_config(actions=args)
    elif args.command in ("leaderboard", "lb"):
        leaderboard(leaderboard_id=args.id)


def exec_shell(language: str, day: int, year: int):
    client = Client.from_config(load_config())

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


def age(p: Path) -> timedelta:
    return datetime.now() - datetime.fromtimestamp(p.stat().st_mtime)


def leaderboard(leaderboard_id: int):
    cache_dir = Path("~/.cache/aoc/").expanduser()
    cache_dir.mkdir(parents=True, exist_ok=True)

    cache_file = cache_dir / f"{leaderboard_id}.json"
    if not cache_file.exists() or age(cache_file) >= timedelta(minutes=15):
        client = Client.from_config(load_config())
        lbdata = client.leaderboard(2023, leaderboard_id)
        with cache_file.open("w") as f:
            json.dump(lbdata, f)
    else:
        with cache_file.open() as f:
            lbdata = json.load(f)

    year = int(lbdata["event"])

    def day_start(day: int):
        return datetime(year, 12, day, 0, 0, 0, tzinfo=zoneinfo.ZoneInfo("EST"))

    def solve_time(day, ts: int) -> timedelta:
        return datetime.fromtimestamp(ts, tz=timezone.utc) - day_start(day)

    days = defaultdict(list)
    times = defaultdict(list)
    longest_name = 0
    for id, data in lbdata["members"].items():
        name = data["name"]
        if name is None:
            name = f"Anon #{id}"
        longest_name = max(longest_name, len(name))
        for day, parts in data["completion_day_level"].items():
            for part, pdata in parts.items():
                if part == "2":
                    t = solve_time(int(day), pdata["get_star_ts"])
                    days[day].append((solve_time(int(day), pdata["get_star_ts"]), name))
                    times[name].append(t)

    def truncate(d: timedelta) -> timedelta:
        return timedelta(seconds=round(d.total_seconds()))

    averages = [
        (truncate(sum(times_, start=timedelta(0)) / len(times_)), name)
        for name, times_ in times.items()
        if len(times_) == len(days)
    ]
    averages.sort()

    fastest = sorted(
        (
            time,
            day,
            name,
        )
        for day, solvers in days.items()
        for time, name in solvers
    )

    n = 10
    for day, solvers in sorted(days.items()):
        solvers.sort()
        print(f"-- DAY {day} TOP {n} --")
        for i, (time, name) in enumerate(solvers[:n], start=1):
            print(f"{i:>4}. {name:<{longest_name}} {time}")

    print(f"--- AVG. FASTEST ({len(days)} days) ---")
    for i, (time, name) in enumerate(averages[:n], start=1):
        print(f"{i:>4}. {name:<{longest_name}} {time}")

    cutoff = 10

    def key(solvers):
        if len(solvers) >= cutoff:
            t, _ = solvers[cutoff - 1]
            return t
        else:
            return timedelta(0)

    print(f"--- TOP {n} FASTEST SOLVES ---")
    for i, (time, day, name) in enumerate(fastest[:n], start=1):
        print(f"{i:>4}. {str(time):>8}  {name:<{longest_name}} (day {day})")

    hardest_days = sorted(days.items(), key=lambda t: key(t[1]), reverse=True)
    print(f"--- HARDEST DAYS (Time to top {cutoff}) ---")
    for i, (day, solvers) in enumerate(hardest_days[:5], start=1):
        if len(solvers) < cutoff:
            continue
        t, _ = solvers[cutoff]
        print(f"{i:>3}. Day {day}  {t}")


if __name__ == "__main__":
    main()
