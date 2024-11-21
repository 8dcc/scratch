#!/usr/bin/python3

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

# This script extracts plain lists from an Org file. See:
#
#   - https://orgmode.org/org.html#Plain-Lists
#   - https://github.com/8dcc/scratch
#
# Example usage from an Org mode code block. Extracts the lists to another file,
# and then sorts it using `org-sort-list':
#
#   (let ((input "my-input-file.org")
#         (output "exported-lists.org"))
#     (shell-command
#      (concat "/usr/bin/python3 extract-lists.py "
#              (shell-quote-argument input)
#              " > "
#              (shell-quote-argument output)))
#     (with-current-buffer
#         (or (find-buffer-visiting output)
#             (find-file-noselect output))
#       (revert-buffer nil 'noconfirm)
#       (goto-char 0)
#       (org-sort-list nil ?a)
#       (save-buffer)))

import sys, os
import re

RE_EMPTY_LINE    = re.compile(r"^\s*$")
RE_ORG_LIST_ITEM = re.compile(r"^\s*?- .+?$")

# This is Regex is not correct, since it's more permissive than Org, but it's
# not a huge problem. See Info node "(org)Plain Lists".
RE_ORG_LIST_BODY = re.compile(r"^\s{2,}.+?$")

def is_empty_line(string):
    return RE_EMPTY_LINE.search(string) != None

def is_org_list_item(string):
    return RE_ORG_LIST_ITEM.search(string) != None

def is_org_list_body(string):
    return RE_ORG_LIST_BODY.search(string) != None

# ------------------------------------------------------------------------------

def print_newlines(ammount):
    for i in range(ammount):
        sys.stdout.write('\n')

def print_lists(target_file):
    empty_line_count = 0
    in_list_item = False

    for line in target_file:
        # If it's an Org list item, print it and store that a "list item body"
        # might follow.
        if is_org_list_item(line):
            sys.stdout.write(line)
            in_list_item = True
            empty_line_count = 0
            continue

        # This isn't the start of a list item, and we are not inside one. We
        # don't care about newlines or possible list bodies.
        if not in_list_item:
            continue

        # Store count of empty lines, so we can print them *only* if we
        # encounter a list body.
        if is_empty_line(line):
            empty_line_count += 1
            continue

        # If we reached here, we are inside a list, and it's not empty. Check if
        # it's a "list item body". If it is, print the newlines that preceded
        # this body line and reset the counter.
        if is_org_list_body(line):
            print_newlines(empty_line_count)
            sys.stdout.write(line)
            empty_line_count = 0
            continue

# ------------------------------------------------------------------------------

def main():
    if len(sys.argv) < 2:
        sys.stderr.write(f"Usage: {sys.argv[0]} FILE.org\n")
        exit(1)

    target_str = sys.argv[1]

    with open(target_str) as target_file:
        print_lists(target_file)

if __name__ == "__main__":
    main()
