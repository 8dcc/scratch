
import os, sys, re
from pydub import AudioSegment
from pydub.playback import play

#-------------------------------------------------------------------------------

def printError(string):
    sys.stderr.write("[Error] " + string + "\n")

def emptyString(string):
    return string.strip() == ""

def applyVolumeFade(song):
    return song.fade_in(1000)

def reduceVolume(song):
    return song - 20

#-------------------------------------------------------------------------------

def main():
    last_input = ""

    while True:
        user_input = input("Path: ")

        # If the user input is empty
        if emptyString(user_input):
            if not emptyString(last_input):
                # And if there is a previous input, use that one
                user_input = last_input
            else:
                # If there is no previous input, error
                printError("Invalid input")
                continue

        # The in-line arguments will be removed from this `user_path' variable
        user_path = user_input

        # The variables for in-line arguments get cleared every iteration
        starting_fade = True
        reduce_volume = True

        # Parse the in-line arguments
        if "-no-fade" in user_input:
            starting_fade = False
            user_path = re.sub(r"\s*-no-fade\s*", "", user_path)
        if "-original-vol" in user_input:
            reduce_volume = False
            user_path = re.sub(r"\s*-original-vol\s*", "", user_path)

        # Make sure the user input is a valid path
        if not os.path.isfile(user_path):
            printError("The path '%s' is not a file" % user_path)
            continue

        # Play the actual sound
        print("\nNow playing: %s" % user_path)
        try:
            song = AudioSegment.from_file(user_path)

            if reduce_volume:
                song = reduceVolume(song)

            if starting_fade:
                song = applyVolumeFade(song)

            play(song)
        except KeyboardInterrupt:
            print("\nStopped.")

        # Only save previous user input if we reached here (i.e. it was valid)
        last_input = user_input
        sys.stdout.write('\n')

if __name__ == '__main__':
    main()
