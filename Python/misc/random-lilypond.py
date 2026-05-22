#!/usr/bin/env python3

import sys
import random
import argparse

ENGLISH_NOTES = ["c", "d", "e", "f", "g", "a", "b"]
ITALIAN_NOTES = ["do", "re", "mi", "fa", "sol", "la", "si"]

VALID_CLEFS = ["treble", "alto", "tenor", "bass"]
VALID_DURATIONS = [1, 2, 4, 8, 16]

def note_octave_to_pitch(note_idx, octave_idx):
    return note_idx + (7 * octave_idx)

# Parse a pitch in scientific pitch notation, accepting either English (e.g.
# "a3", "c4") or Italian (e.g. "la3", "do4") note names. Returns an integer
# index: note_index + 7 * lilypond_octave. LilyPond's unmarked octave matches
# scientific octave 3 (i.e. middle C, scientific C4, is "c'" in LilyPond).
def parse_pitch(s):
    note_idx = None
    octave_str = None
    for names in (ITALIAN_NOTES, ENGLISH_NOTES):
        for i, name in enumerate(names):
            if s.startswith(name) and s[len(name):].lstrip("-").isdigit():
                note_idx = i
                octave_str = s[len(name):]
                break
        if note_idx is not None:
            break

    if note_idx is None:
        raise argparse.ArgumentTypeError(
            "invalid pitch '%s' (expected note name followed by octave "
            "number, e.g. 'a3' or 'la3')" % s)

    return note_octave_to_pitch(note_idx, int(octave_str) - 3)

# Convert an integer pitch index back into a (note_index, octave) pair.
def split_pitch(idx):
    return idx % 7, idx // 7

def octave_marks(octave):
    if octave > 0:
        return "'" * octave
    else:
        return "," * -octave

def random_note(note_names, min_pitch, max_pitch):
    idx = random.randint(min_pitch, max_pitch)
    note_idx, octave = split_pitch(idx)
    duration = random.choice(VALID_DURATIONS)
    return "%s%s%d" % (note_names[note_idx], octave_marks(octave), duration)

def generate_lilypond(num_notes, language, clef, min_pitch, max_pitch):
    note_names = ITALIAN_NOTES if language == "italian" else ENGLISH_NOTES
    notes = " ".join(random_note(note_names, min_pitch, max_pitch)
                     for _ in range(num_notes))

    return (
        '\\version "2.24.0"\n'
        '\\language "%s"\n'
        '\n'
        '\\score {\n'
        '  \\new Staff {\n'
        '    \\clef %s\n'
        '    %s\n'
        '  }\n'
        '}\n'
    ) % (language, clef, notes)

def main():
    parser = argparse.ArgumentParser(
        description="Generate a LilyPond file with N random notes.")
    parser.add_argument("num_notes", type=int, help="Number of notes to generate")
    parser.add_argument("-l", "--language", choices=["english", "italian"],
                        default="english",
                        help="Note naming convention (default: english)")
    parser.add_argument("-c", "--clef", choices=VALID_CLEFS, default="treble",
                        help="Clef to use (default: treble)")
    parser.add_argument("--min", dest="min_pitch", type=parse_pitch,
                        default=note_octave_to_pitch(5, 0),
                        help="Minimum pitch in scientific notation, e.g. "
                             "'a3', 'la3', 'c4' (default: a3)")
    parser.add_argument("--max", dest="max_pitch", type=parse_pitch,
                        default=note_octave_to_pitch(5, 2),
                        help="Maximum pitch in scientific notation "
                             "(default: a5)")
    args = parser.parse_args()

    if args.num_notes <= 0:
        print("The number of notes must be positive", file=sys.stderr)
        exit(1)

    if args.min_pitch > args.max_pitch:
        print("The minimum pitch must not be higher than the maximum pitch",
              file=sys.stderr)
        exit(1)

    print(generate_lilypond(args.num_notes, args.language, args.clef,
                            args.min_pitch, args.max_pitch))

if __name__ == "__main__":
    main()
