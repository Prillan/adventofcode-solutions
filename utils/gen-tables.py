#!/usr/bin/env python3
from collections import defaultdict
import json
import sys
from pathlib import Path
import functools
import itertools
import string

days = defaultdict(lambda: defaultdict(dict))

days_by_lang = defaultdict(lambda: defaultdict(dict))

with Path(sys.argv[1]).open() as f:
    langs = {l["slug"]: l for l in json.load(f)}

template = Path(sys.argv[2]).read_text()

for drv in map(Path, sys.argv[3:]):
    with (drv / "meta.json").open() as f:
        meta = json.load(f)

    y = meta["year"]
    d = meta["day"]
    l = meta["lang"]["slug"]

    status = meta["status"]
    days[y][d][l] = status
    days_by_lang[l][y][d] = status

    days_by_lang["total"][y][d] = status if status == "G" else "?"

_total = {
    "slug": "total",
    "name": "Total",
}

years = sorted(days.keys())


def slug(x):
    return x.lower()


hrule = object()


def table(header, rows):
    def markup_row(cols):
        return "| " + " | ".join(map(str, cols)) + " |"

    def row(cols):
        if cols is hrule:
            return markup_row(["------"] * len(header)).replace(" ", "")

        return markup_row(cols)

    return "\n".join(map(row, itertools.chain([header, hrule], rows)))


def completion_table():
    def lang_cell(l):
        name = l["name"]
        anchor = "".join(c for c in l["name"].lower() if c in string.ascii_lowercase)
        if l["slug"] == "total":
            return name
        else:
            return f"[{name}](#{anchor})"

    def cell(y, l):
        completed = sum(
            1 for d in _year_days(y) if days_by_lang[l["slug"]][y].get(d, "") == "G"
        )
        if completed == len(_year_days(y)):
            return "✓"
        elif completed == 0:
            return ""
        else:
            return f"{completed}/{len(_year_days(y))}"

    def row(l):
        return [lang_cell(l)] + [cell(y, l) for y in years]

    return table(
        [""] + years,
        itertools.chain(
            (row(l) for l in sorted(langs.values(), key=lambda l: l["name"])),
            [hrule],
            [row(_total)],
        ),
    )


def lang_table(l):
    if langs[l]["full"]:
        return lang_table_full(l)
    else:
        return lang_table_simple(l)


@functools.cache
def _year_days(y):
    return list(range(1, 1 + (25 if y < 2025 else 12)))


def lang_table_simple(l):
    ext = langs[l]["extension"]
    return "\n".join(
        ["Solved:"]
        + [
            f" - [{y}, day {d}](./{y}/day{d}/run.{ext})"
            for y in years
            for d in _year_days(y)
            if days[y][d].get(l, "?") == "G"
        ]
    )


def lang_table_full(l):
    ext = langs[l]["extension"]

    def row(d):
        return [d] + [cell(d, y) for y in years]

    def cell(d, y):
        if d not in _year_days(y):
            return "-"
        status = days[y][d].get(l, "?")
        if status == "G":
            return f"[✓](./{y}/day{d}/run.{ext})"
        elif status == "?":
            return ""
        elif status == "B":
            return "❌"

    return table([r"Day \\ Year"] + years, map(row, range(1, 26)))


def lang_tables():
    return "\n".join(
        f"## {langs[l]['name']}\n{lang_table(l)}\n" for l in sorted(langs.keys())
    )


print(
    template.replace("$$$COMPLETION$$$", completion_table()).replace(
        "$$$LANGUAGE_COMPLETION$$$", lang_tables()
    )
)
