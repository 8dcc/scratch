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

import sys
import csv
from datetime import datetime

import plotly.express as px

def read_csv_data(csv_path):
    # The CSV module expects the "raw" newlines from the file, so we need to
    # disable Python's newline translation through the 'newline' argument.
    with open(csv_path, newline='') as f:
        reader = csv.reader(f)

        # Extract the first line of the CSV file, the header, which contains the
        # column names. Iterate it, storing the valid (stripped) names.
        header = []
        for column_name in next(reader):
            stripped = column_name.strip()
            if stripped:
                header.append(stripped)

        # Store the rest of the rows, which contain the actual data, in an
        # array.
        rows = [row for row in reader if any(row)]

    # Create a dictionary with the column names as the keys, and an empty array
    # for the value, which will contain the values.
    result = {}
    for column_name in header:
        result[column_name] = []

    # Fill the dictionary with the row values.
    for row in rows:
        for column_idx, column_name in enumerate(header):
            try:
                value = float(row[column_idx])
            except (ValueError, IndexError):
                value = None
            result[column_name].append(value)

    return result

def transform_csv_data(csv_dict):
    # Divide RPM values to match
    if "RPM" in csv_dict:
        for i, value in enumerate(csv_dict["RPM"]):
            if value is not None:
                csv_dict["RPM"][i] /= 100.0

    # Convert timestamps to from UNIX to human format.
    if "Date" in csv_dict:
        for i, value in enumerate(csv_dict["Date"]):
            if value is not None:
                try:
                    csv_dict["Date"][i] = datetime.fromtimestamp(csv_dict["Date"][i])
                except TypeError:
                    sys.stderr.write("[Warning] Could not convert timestamp to date.")

def main():
    if len(sys.argv) < 2:
        sys.stderr.write(f"Usage: {sys.argv[0]} INPUT.csv\n")
        sys.exit(1)

    csv_path = sys.argv[1]
    csv_dict = read_csv_data(csv_path)
    transform_csv_data(csv_dict)

    x = []
    y = []
    var = []

    # Iterate the CSV dictionary, ignoring the "Date" column, which is used for
    # the X axis.
    #
    # For each column, zip the dates and the data:
    #
    #   ["09:00", "10:00", "11:00"]
    #   [111, 222, 333]
    #
    # Producing:
    #
    #   [
    #     ("09:00", 111),
    #     ("10:00", 222),
    #     ("11:00", 333),
    #   ]
    #
    # Then append it to the 'x', 'y' and 'var' arrays, grouping the data by
    # column.
    for column_name in csv_dict:
        if column_name == "Date":
            continue

        for xi, yi in zip(csv_dict["Date"], csv_dict[column_name]):
            x.append(xi)
            y.append(yi)
            var.append(column_name)

    # Plot the previously-processed data.
    fig = px.line(
        x=x,
        y=y,
        color=var,
        labels={"x": "Date", "y": "Value", "color": "Variable"},
        title="Car Telemetry",
        render_mode="webgl"
    )

    fig.show()

if __name__ == "__main__":
    main()
