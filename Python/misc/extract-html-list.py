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

# Print elements of an HTML list. Assumes the structure of the input file is
# something like:
#
#   <ul>
#     <li><a href="LINK1">TEXT1</a></li>
#     <li><a href="LINK2">TEXT2</a></li>
#     ...
#   </ul>
#
# Dependencies:
#   pip install beautifulsoup4

import sys
from bs4 import BeautifulSoup

#-------------------------------------------------------------------------------
# I/O helpers.

def ftl(text):
    sys.stderr.write(text + "\n")
    exit(1)

#-------------------------------------------------------------------------------
# BeautifulSoup helpers.

def get_valid_elem(parent, elem, attrs={}):
    result = parent.find(elem, attrs=attrs)
    if (result == None):
        ftl(f"Could not find a '{elem}' element with the specified attributes: {attrs}")
    return result

def get_valid_elems(parent, elem, attrs={}, recursive=True):
    result = parent.find_all(elem, attrs=attrs, recursive=recursive)
    if (result == None):
        ftl(f"Could not find any '{elem}' elements with the specified attributes: {attrs}")
    return result

#-------------------------------------------------------------------------------
# Main program.

def print_list_elements(lst):
    elts = get_valid_elems(lst, "li")
    for elt in elts:
        link = get_valid_elem(elt, "a")
        print(link.text + " | " + link["href"])

def main():
    if len(sys.argv) != 2:
        ftl(f"Usage: {sys.argv[0]} FILE.html")

    with open(sys.argv[1]) as target_file:
        soup = BeautifulSoup(target_file, 'html.parser')
        print_list_elements(soup)

if __name__ == "__main__":
    main()
