#!/usr/bin/python

import os.path
import os
import sys

#              ##############################
#              ---   SCRIPT REQUIREMENTS  ---
#              ##############################
#
#         Requires Python Version 2.6.6 or above.
#

# Specify a path for a lock file
lock = os.path.dirname(os.path.realpath(__file__)) + '/lock'

# Check if the lock file exists if not create it
def checkLock():

    # If the lock file exists then dont run
    if os.path.isfile(lock):
        print("Lock File Exists, Exiting...")
        sys.exit(0)
    else:
        lock_file = open(lock, 'w')

# Check if the lock file exists if so then remove it and exit otherwise just exit
def exit():

    # If the lock file exists then remove it and exit
    if os.path.isfile(lock):
        os.remove(lock)
        sys.exit(0)
    else:
        sys.exit(0)
