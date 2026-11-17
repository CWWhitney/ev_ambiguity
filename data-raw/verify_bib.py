#!/usr/bin/env python3
"""Check vignettes/references.bib against the Crossref REST API.

For entries with a DOI, the registered record is fetched and compared with the
bib entry (title, year, first author, volume, pages). For entries without a
DOI, the best bibliographic match is printed for manual review (books and
older works often have no DOI). Exit status is 1 if any DOI fails to resolve
or any compared field disagrees.

Usage: python3 data-raw/verify_bib.py [vignettes/references.bib]
"""
import difflib
import json
import re
import sys
import time
import urllib.parse
import urllib.request

API = "https://api.crossref.org/works"
HEADERS = {"User-Agent": "evca-bib-check (mailto:cory.whitney@uni-bonn.de)"}


def get(url):
    req = urllib.request.Request(url, headers=HEADERS)
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.load(r)["message"]


def parse_bib(text):
    entries = []
    for m in re.finditer(r"@(\w+)\{([^,]+),(.*?)\n\}", text, re.S):
        fields = {}
        for k, v in re.findall(r"(\w+)\s*=\s*(\{.*?\}|\d+)\s*,?\n", m.group(3) + "\n", re.S):
            fields[k.lower()] = v.strip()[1:-1] if v.startswith("{") else v
        entries.append((m.group(2), fields))
    return entries


def norm(s):
    s = re.sub(r"\\[`'\"^~]?|[{}`']", "", s or "")
    return re.sub(r"[^a-z0-9 ]", " ", s.lower()).split()


def sim(a, b):
    return difflib.SequenceMatcher(None, " ".join(norm(a)), " ".join(norm(b))).ratio()


def family(fields):
    first = re.split(r"\s+and\s+", fields.get("author") or fields.get("editor", ""))[0]
    return norm(first.split(",")[0])


def compare(fields, rec):
    problems = []
    title = (rec.get("title") or [""])[0]
    if sim(fields.get("title", ""), title) < 0.85:
        problems.append(f"title differs: '{title}'")
    year = (rec.get("issued", {}).get("date-parts") or [[None]])[0][0]
    if year and str(year) != fields.get("year"):
        problems.append(f"year {year} vs {fields.get('year')}")
    people = rec.get("author") or rec.get("editor") or []
    if people and family(fields) != norm(people[0].get("family", "")):
        problems.append(f"first author {people[0].get('family')}")
    if "volume" in fields and rec.get("volume") and rec["volume"] != fields["volume"]:
        problems.append(f"volume {rec['volume']} vs {fields['volume']}")
    if "pages" in fields and rec.get("page"):
        want = fields["pages"].replace("--", "-")
        if want.split("-")[0] != rec["page"].replace("\u2013", "-").split("-")[0]:
            problems.append(f"pages {rec['page']} vs {fields['pages']}")
    return problems


def main(path):
    failed = 0
    for key, fields in parse_bib(open(path, encoding="utf-8").read()):
        doi = fields.get("doi")
        try:
            if doi:
                rec = get(f"{API}/{urllib.parse.quote(doi)}")
                problems = compare(fields, rec)
                status = "OK " if not problems else "BAD"
                failed += bool(problems)
                print(f"{status} {key} {doi} {'; '.join(problems)}")
            else:
                q = f"{fields.get('author') or fields.get('editor')} {fields['title']} {fields['year']}"
                items = get(f"{API}?rows=1&query.bibliographic={urllib.parse.quote(q)}")["items"]
                hit = items[0] if items else {}
                print(f"NODOI {key}: best match {hit.get('DOI')} "
                      f"'{(hit.get('title') or [''])[0]}' (review manually)")
        except Exception as e:
            failed += 1
            print(f"ERR {key} {doi}: {e}")
        time.sleep(0.2)
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "vignettes/references.bib")
