
import os, sys
from pydub import AudioSegment
from pydub.playback import play

#-------------------------------------------------------------------------------

def printError(string):
    sys.stderr.write("[Error] " + string + "\n")

def emptyString(string):
    return string.strip() == ""

def applyVolumeFade(song):
    duration_ms = song.duration_seconds * 1000
    return song.fade_in(int(duration_ms / 2))

def reduceVolume(song):
    return song - 20

#-------------------------------------------------------------------------------

def main():
    last_path = ""

    while True:
        user_path = input("Path: ")

        # If the user input is empty
        if emptyString(user_path):
            if not emptyString(last_path):
                # And if there is a previous path, use that one
                user_path = last_path
            else:
                # If there is no previous path, error
                printError("Invalid input")
                continue

        # Make sure the user input is a valid path
        if not os.path.isfile(user_path):
            printError("The path '%s' is not a file" % user_path)
            continue

        # Play the actual sound
        print("\nNow playing: %s" % user_path)
        try:
            song = AudioSegment.from_file(user_path)
            play(applyVolumeFade(reduceVolume(song)))
        except KeyboardInterrupt:
            print("\nStopped.")

        # Only save previous user input if we reached here (i.e. it was valid)
        last_path = user_path
        sys.stdout.write('\n')

if __name__ == '__main__':
    main()
