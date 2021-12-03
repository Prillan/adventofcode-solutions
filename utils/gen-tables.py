#!/usr/bin/env python3
from collections import defaultdict
import json
import sys
from pathlib import Path

days = defaultdict(
    lambda: defaultdict(dict)
)

days_by_lang = defaultdict(
    lambda: defaultdict(dict)
)

langs = {}

template = Path(sys.argv[1]).read_text()

for drv in map(Path, sys.argv[2:]):
    with (drv / 'meta.json').open() as f:
        meta = json.load(f)

    y = meta['year']
    d = meta['day']
    l = meta['lang']['extension']
        
    days[y][d][l] = meta['status']
    days_by_lang[l][y][d] = meta['status']
    langs[l] = meta['lang']

years = sorted(days.keys())

def slug(x):
    return x.lower()

def table(header, rows):
    def row(cols):
        return '| ' + ' | '.join(map(str, cols)) + ' |'

    return '\n'.join([
        row(header),
        row(['------'] * len(header)).replace(' ', ''),
        *map(row, rows)
    ])
    
def completion_table():
    def lang_cell(l):
        name = l['name']
        anchor = slug(l['name'])
        return f"[{name}](#{anchor})"

    def cell(y, l):
        completed = sum(
            1
            for d in range(1, 26)
            if days_by_lang[l['extension']][y].get(d, '') == 'G'
        )
        if completed == 25:
            return '✓'
        else:
            return f'{completed}/25'

    def row(l):
        return [lang_cell(l)] + [cell(y, l) for y in years]

    return table(
        [""] + years,
        [row(l) for l in sorted(langs.values(), key=lambda l: l['name'])]
    )

def lang_table(l):
    def row(d):
        return [d] + [cell(d, y) for y in years]

    def cell(d, y):
        status = days[y][d].get(l, '?')
        if status == 'G':
            return f'[✓](./{y}/day{d}/run.{l})'
        elif status == '?':
            return ''
        elif status == 'B':
            return '❌'
        
    return table(
        [r"Day \\ Year"] + years,
        map(row, range(1, 26))
    )

def lang_tables():
    return '\n'.join(
        f"## {langs[l]['name']}\n{lang_table(l)}"
        for l in sorted(langs.keys())
    )

print(template
      .replace('$$$COMPLETION$$$', completion_table())
      .replace('$$$LANGUAGE_COMPLETION$$$', lang_tables()))
