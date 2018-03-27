#!/usr/bin/python

import os.path
import os
import re
import MySQLdb
from enum import Enum
from enum import IntEnum

# Import custom modules
import conf

#              ##############################
#              ---   SCRIPT REQUIREMENTS  ---
#              ##############################
#
#         Requires Python Version 2.6.6 or above.
#         Requires Python 3.4 Enum Backported
#            - enum34 1.1.6
#         Requires MYSQL-python
#            - MySQLdb 1.2.5

def runSQL(user, configuration):

    # If it is an actual user and there is a database set for it to run its queries on
    if (user != "Unknown User" and user != "Global") and (configuration[user][conf.CONFIG.Run][0] != ''):

        # For each database it has set to run queries on
        for database in configuration[user][conf.CONFIG.Run]:

            # Find the user of the database to write to
            for entry in configuration:
                try:
                    if configuration[entry][conf.CONFIG.Database.value] == database:
                        dbuser = entry
                        break
                    else:
                        continue
                except:
                    continue

            # Fetch the password for the database user
            dbpass = configuration[dbuser][conf.CONFIG.Pass.value]

            # Connect up to the database
            db = MySQLdb.connect('localhost', dbuser, dbpass, database)
            cursor = db.cursor()

            # Setup error, counter and filename variables
            err = 0
            count = 0
            cmds = []
            stop_at = ''
            tmp_file = configuration[user][conf.CONFIG.Export.value] + "_tmp"                   

            # Open the file
            with open(configuration[user][conf.CONFIG.Export.value]) as file:

                # Store each line in the cmds array
                for line in file:
                    cmds.append(line)

            # Read the list backwards to see if the last entries are part of an unfinished transaction
            for index, cmd in reversed(list(enumerate(cmds))):

                print(cmd.strip())

                # If we find a commit or rollback first then there is no unfinished transaction that may be rolled back so just break
                if cmd.strip() == 'COMMIT;' or cmd.strip() == 'ROLLBACK;':
                    break

                # But if we find a start transaction first then it was never completed with a rollback or commit so set processing to stop at that point and break
                elif cmd.strip() == 'START TRANSACTION;':
                    stop_at = index
                    break

            print('Connected to database ' + str(configuration[dbuser][conf.CONFIG.Database.value]))

            # Open our tmp file to store lines in case of an error
            tmp = open(tmp_file, 'w')

            # Open our commands list and begin processing them
            for index, cmd in list(enumerate(cmds)):

                # If the line is the same as we are set to stop at then set err to 2 to begin writing
                if index == stop_at:
                    err = 2

                if err == 0:

                    # Try executing the command
                    try:
                        cursor.execute(cmd)
                        count += 1

                    # Should an error occur we stop processing commands by setting err to 1 and write the line to our tmp file
                    # To stop on a warning as well we should add ',MySQLdb.Warning' inside brackets
                    except (MySQLdb.Error) as error:
                        print(error)
                        tmp.write(cmd)
                        err = 1
                    except:
                        print("Unknown Error")
                        tmp.write(cmd)
                        err = 1

                # We have hit some form of error, stop processing and just write all the lines to tmp
                # if we kept processing we might end up with commands run out of order, wrong primary keys, other errors etc
                else:
                    tmp.write(cmd)
                           
            # Close the database connection
            db.close()
            # Close the tmp file
            tmp.close()

            # If we hit an error store a backup of the sqlfile we ran commands from in the same directory
            # and rename the tmp_file which contains the errored command and all following, to be the original file
            if err == 1:

                os.rename(configuration[user][conf.CONFIG.Export.value], configuration[user][conf.CONFIG.Export.value] + "_backup")
                os.rename(tmp_file, configuration[user][conf.CONFIG.Export.value])

                print
                print("Encountered Error...")
                print("Ran " + str(count) + " Queries before Error")
                print("Created backup of file executed")
                print("Saved remaining queries (including errored) as file run.")

            elif err == 2:

                os.remove(configuration[user][conf.CONFIG.Export.value])
                os.rename(tmp_file, configuration[user][conf.CONFIG.Export.value])

                print
                print("Encountered Unfinished Transaction...")
                print("Stopped Execution at Line " + str(stop_at + 1))
                print("Ran " + str(count) + " Queries before Unfinished Transaction")

            # Otherwise if there were no errors just delete both files
            else:

                os.remove(configuration[user][conf.CONFIG.Export.value])
                os.remove(tmp_file)

                print
                print("Ran " + str(count) + " Queries...")
                print("Completed Successfully")

