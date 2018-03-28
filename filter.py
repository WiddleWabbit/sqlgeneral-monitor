#!/usr/bin/python

import os.path
import os
import re
from enum import Enum
from enum import IntEnum

#              ---   SCRIPT REQUIREMENTS  ---
# 
#         Requires Python Version 2.6.6 or above.
#         Requires Python 3.4 Enum Backported
#            - enum34 1.1.6
#

#               ---   DEFINING ENUMERATORS   ---
# Defined as IntEnums so they are compatible with Integers

# Columns in 2D array
class FILE(IntEnum):
    Line = 0
    String = 1
    Stripped = 2
    Save = 3

#               ---   SUB FUNCTIONS   ---

# Function to return an index minus one
# Recieves one integer
def indexMinusOne(index_given):

    return index_given - 1


# Function to remove new line characters from a string
# Recieves one string to remove new lines from
def removeNewlines(string):

    string = string.replace('\n', '')
    return string

#               ---   BEGIN MAIN   ---

def filter(importfile):

    # Used to Store all lines from the file
    sqlcmds = []

    # Define counters
    lineno = 1
    counter = 0
    lines_removed = 0

    print
    print('Checking ' + str(importfile))

    if os.path.exists(importfile) and os.path.isfile(importfile):

        # Write the information from the sql log to the array in a 2D format
        with open(importfile) as import_file:
            for line in import_file:
                # Append List to the current position
                sqlcmds.append([])
                sqlcmds[counter].insert(FILE.Line.value, lineno)
                sqlcmds[counter].insert(FILE.String.value, removeNewlines(line))

                # Strip whitespace from before and after the lines string and then store
                sqlcmds[counter].insert(FILE.Stripped.value, sqlcmds[counter][FILE.String.value].strip())

                # If this line is a COMMIT only line
                if sqlcmds[counter][FILE.Stripped.value] == 'COMMIT;' or sqlcmds[counter][FILE.Stripped.value] == 'ROLLBACK;':

                    # Check the previous line to see if it was an START TRANSACTION only line
                    # If this is the case then this is a useless command that can be filtered as it does nothing
                    if sqlcmds[indexMinusOne(counter)][FILE.String.value] == 'START TRANSACTION;':

                        sqlcmds[indexMinusOne(counter)].insert(FILE.Save.value, '0')
                        sqlcmds[counter].insert(FILE.Save.value, '0')
                        lines_removed += 1

                lineno += 1
                counter += 1

#               ---   STORE INFORMATION ---

        print('Found ' + str(lines_removed) + ' Empty Transaction/s')
        print('Removed ' + str(lines_removed * 2) + ' Lines')

        # Only Try to Remove Empty Transactions if There Were Some
        if lines_removed != 0:

            print('Creating Temporary file...')

            file_export = importfile
            file_tmp = str(importfile) + "_tmp"

            # Rename the file to a tmp
            os.rename(importfile, str(file_tmp))

            export = open(file_export, "a")
            print('Removing Empty Transactions...')

            # Reiterate through the array to store information
            for x in range(len(sqlcmds)):

                # If the Save has not been set then set it
                if len(sqlcmds[x]) != 4:

                    sqlcmds[x].insert(FILE.Save.value, '1')

                # If the row is set to be saved
                if sqlcmds[x][FILE.Save.value] == '1':            

                    write = sqlcmds[x][FILE.Stripped.value] + "\n"
                    export.write(write)

            export.close()

            print('Empty Transactions Removed Successfully')

#               ---   DELETE TEMP FILE ---

            os.remove(file_tmp)
        
            print('Temporary File Removed')

#               ---   IF FILE DOES NOT EXIST   ---

    else:
        print("No Export File to Filter...")
