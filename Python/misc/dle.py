#!/usr/bin/env python3

# Copyright 2024 8dcc
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/>.

# Dependencies:
#   pip install requests beautifulsoup4 html5lib

import sys
import requests
from bs4 import BeautifulSoup
import textwrap


# ------------------------------------------------------------------------------
# Constant settings.


# User agent when making the request.
USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:111.0) Gecko/20100101 Firefox/111.0"

# If true, remove examples from definitions.
REMOVE_EXAMPLES = True

# Number of spaces used when indenting definitions.
BASE_DEFINITION_INDENT = 2

# Maximum text width.
MAX_WIDTH = 80


# ------------------------------------------------------------------------------
# I/O helpers.


def ftl(text):
    sys.stderr.write(text + "\n")
    exit(1)


def pretty_print(text, init_indent=0, rest_indent=0, width=MAX_WIDTH):
    print(textwrap.fill(text,
                        width=width,
                        initial_indent=(" " * init_indent),
                        subsequent_indent=(" " * rest_indent)))


# ------------------------------------------------------------------------------
# BeautifulSoup helpers.


def get_valid_elem(parent, elem, attrs={}):
    result = parent.find(elem, attrs=attrs)
    if result is None:
        ftl(f"Could not find a '{elem}' element with the specified attributes: {attrs}")
    return result


def get_valid_elems(parent, elem, attrs={}, recursive=True):
    result = parent.find_all(elem, attrs=attrs, recursive=recursive)
    if result is None or len(result) <= 0:
        ftl(f"Could not find any '{elem}' elements with the specified attributes: {attrs}")
    return result


# ------------------------------------------------------------------------------
# Main parsing functions.


def extract_definitions(html_list):
    result = []
    for li in get_valid_elems(html_list, "li", recursive=False):
        if REMOVE_EXAMPLES:
            examples = li.find_all("span", attrs={"class": "h"})
            for example in examples:
                example.extract()

        definition_item = get_valid_elem(li, "div", attrs={"class": "c-definitions__item"})
        definition = get_valid_elem(definition_item, "div", attrs=None)
        result.append(definition.text)

    return result


def print_definitions(definitions):
    if len(definitions) <= 0:
        ftl("No definitions found.")

    definition_counter = 1
    for definition in definitions:
        pretty_print(definition,
                     BASE_DEFINITION_INDENT,
                     BASE_DEFINITION_INDENT + len(str(definition_counter) +
                                                  ". "))
        definition_counter += 1


def print_info_from_url(url):
    headers = {"User-Agent": USER_AGENT}
    r = requests.get(url=url, headers=headers)
    soup = BeautifulSoup(r.content, "html5lib")

    articles = get_valid_elems(soup, "article", {"class": "o-main__article"})
    assert len(articles) > 0

    article_count = 1
    for article in articles:
        for superscript in article.find_all('sup'):
            superscript.extract()

        title = get_valid_elem(article, "h1",
                               {"class": "c-page-header__title"})
        etymology = article.find("div", attrs={"class": "c-text-intro"})
        definition_list = get_valid_elem(article, "ol",
                                         {"class": "c-definitions"})
        extracted_definitions = extract_definitions(definition_list)

        if article_count > 1:
            print()

        title_text = title.text.capitalize()
        if len(articles) > 1:
            title_text += f" ({article_count})"

        pretty_print(title_text)
        print("=" * len(title_text) + "\n")

        if etymology is not None:
            pretty_print(etymology.text)
            print()

        print_definitions(extracted_definitions)

        article_count += 1


# ------------------------------------------------------------------------------
# Entry point.


def main():
    if len(sys.argv) < 2:
        ftl(f"Usage: {sys.argv[0]} WORD")

    target_word = sys.argv[1]
    url = f"https://dle.rae.es/{target_word}"
    print_info_from_url(url)


if __name__ == "__main__":
    main()
