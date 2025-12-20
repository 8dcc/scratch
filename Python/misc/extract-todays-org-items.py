#!/usr/bin/env python3
#
# Copyright 2025 8dcc. All Rights Reserved.
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https://www.gnu.org/licenses/>.
#
# ------------------------------------------------------------------------------
#
# This script extracts, from an Org file, level-2 headings that are below a
# level-1 heading whose format matches the current date.
#
# For example, assuming today is 2025.12.29, from the input:
#
#     * 2025.12.29
#     ** DONE Foo
#     ** TODO Bar
#     * 2025.12.30
#     ** TODO Baz
#     * 2025.12.31
#     ** 12345
#
# This script would extract "Foo" or "Bar", depending on the command-line
# options.

import sys
import argparse
from datetime import datetime


def get_todays_org_tasks(filename, show_mode='todo', current_date=None):
    result_lines = []

    if not current_date:
        current_date = datetime.now().strftime("%Y.%m.%d")

    with open(filename, 'r') as f:
        in_todays_section = False

        for line in f:
            line = line.rstrip()

            # First, check if this line starts a new section.
            if line.startswith('* '):
                # Encountered a new level-1 heading after today's section,
                # we are done processing.
                if in_todays_section:
                    break

                # If the level-1 heading we encountered matches the current
                # date, store it for next iterations.
                if current_date in line:
                    in_todays_section = True

                continue

            # We don't care about lines that aren't nested inside today's
            # section.
            if not in_todays_section:
                continue

            # We don't care about lines that aren't level-2 headings.
            if not line.startswith('** '):
                continue

            # Remove the leading '** '.
            task = line[3:]

            is_done = task.startswith('DONE ')
            is_todo = task.startswith('TODO ')

            # Only keep TODO/DONE keywords if they can be mixed in the output.
            if show_mode != 'all':
                task = task[5:]

            if show_mode == 'all' or \
               (show_mode == 'todo' and is_todo) or \
               (show_mode == 'done' and is_done):
                result_lines.append(task)

        return result_lines


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('file', help='Org file to parse')
    parser.add_argument(
        '--show',
        choices=['todo', 'done', 'all'],
        default='todo',
        help='What to show: todo (default), done, or all',
    )

    args = parser.parse_args()

    try:
        result_lines = get_todays_org_tasks(args.file, args.show)
    except FileNotFoundError:
        sys.stderr.write(f"File not found: {args.file}\n")
        sys.exit(1)

    for line in result_lines:
        print(f"* {line}")


if __name__ == '__main__':
    main()
