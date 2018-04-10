#!/usr/bin/python

import os.path
import os
import re
import sys
import shlex
import argparse
import subprocess
import ConfigParser
from enum import Enum
from enum import IntEnum
from optparse import OptionParser
from datetime import datetime

# Prevent .pyc files forming
sys.dont_write_bytecode = True

# Import other script file
import filter
import conf
import runsql
import lock

# Begin Timing Script
startTime = datetime.now()

#              ##############################
#              ---   SCRIPT REQUIREMENTS  ---
#              ############################## 
#
#         Requires Python Version 2.6.6 or above.
#         Requires Python 3.4 Enum Backported
#            - enum34 1.1.6
#

#               ######################
#               ---   LOCK CHECK   ---
#               ######################

lock.checkLock()

#               ############################
#               ---   DEFINING OPTIONS   ---
#               ############################

# Define option parser and usage
parser = argparse.ArgumentParser()
usage = "usage: %prog [option] arg"

# Define log option
parser.add_argument("-f", "--file", help="Specify the sql log file to process via an absolute path.", dest="location")
parser.add_argument("-c", "--config", help="Specify a configuration file by name, no path reference required.", required=True, dest="config_file")
parser.add_argument("-d", "--debug-line", help="Specify with a line number to print to screen all recorded information from that line including User, Query Type etc. Only accepts numbers.", dest="debug_line")
parser.add_argument("--run", help="Run queries on any databases configured for them to be run.", action='store_true', dest="run")
parser.add_argument("--run-only", help="Only run queries, do not process any log file", action='store_true', dest="run_only")
parser.add_argument("--flush-log", help="If the file specified is the current general log file for MySQL then you can specify this option to backup the log, flush the logs and process the backup", action='store_true', dest="flush")

args = parser.parse_args()

#               ##############################
#               ---   VALIDATING OPTIONS   ---
#               ##############################

# Stop script if no option is passed for the file location
if not args.run_only and not args.location:
    print("Log file not defined correctly (Use -f)")
    lock.exit()

#               ######################
#               ---   FLUSH LOGS   ---
#               ######################

# Specify a command for moving and flushing the log file
bashCMD = "mv -f " + str(args.location) + " " + str(args.location) + ".backup"
bashCMD2 = "mysqladmin flush-logs"

# Run the command
if args.flush:

    process = subprocess.Popen(bashCMD.split(), stdout=subprocess.PIPE)
    out, err = process.communicate()

    if err:
        print(err)
        print("Error moving log")
        lock.exit()

    else:
        file = str(args.location) + ".backup"
        print(out)
        print("Logfile Moved, Attempting to Flush Logs...")

    process = subprocess.Popen(bashCMD2.split(), stdout=subprocess.PIPE)
    out, err = process.communicate()

    if err:
        print(err)
        print("Error flushing logs")
        lock.exit()

    else:
        file = str(args.location) + ".backup"
        print(out)
        print("Successfully Flushed Logs")

else:

    file = args.location

#               ################################
#               ---   DEFINING ENUMERATORS   ---
#               ################################

# Defined as IntEnums so they are compatible with Integers

# Columns in 2D array
class COL(IntEnum):
    Line = 0
    String = 1
    Type = 2
    QueryNo = 3
    StartIndex = 4
    User = 5
    Save = 6
    Query = 7

# Types of lines found in sql log
# Used to represent values in COL.Type
class TYPE(IntEnum):
    Text = 1
    Query = 2
    Connect = 3
    Quit = 4
    Statistics = 5
    Continued = 6
    InitDB = 7
    FieldList = 8

# The cPanel users associated with databases
# Used to represent values in COL.User
class USER(IntEnum):
    Oem = 1
    Deoem = 2
    Oemused = 3
    Deused = 4

#               #########################
#               ---   SUB FUNCTIONS   ---
#               #########################

# Function to return the sql log query number from the log line and the type of query
# Recieves the line as a string
# Recieves the type as the int of the IntEnum Type
def getSqlNo(string, type):

    # Split the string into an array of two values (the first and second half), seperated by the TYPE
    string = string.split(TYPE(type).name, 1)
    # Pull the last number from the first array element (the first half), this will be the query number
    string = re.findall(r'\d+', string[0])[-1]
    # Return the value
    return string

# Function to return the Valid SQL from a line
# Recieves the line as a string
# Recieves the type of SQL command as a string
def getQuery(string, type):
    
    # Split the string by the type passed and return the second half of the split only
    return string.split(type, 1)[1].lstrip() + ";"


# Function to return the type intenum value
# Recieves the type in string format with captialized first letter
def getType(string):

    if string == "Query":
        return TYPE.Query.value

    elif string == "Connect":
        return TYPE.Connect.value

    elif string == "Quit":
        return TYPE.Quit.value

    elif string == "Statistics":
        return TYPE.Statistics.value

    elif string == "Init DB":
        return TYPE.InitDB.value

    elif string == "Field List":
        return TYPE.FieldList.value

    else:
        print(string)
        print("Unknown type for line")
        lock.exit()

# Function to return the user from a connect statement in the sql log
# Recieves the line as a string and the configuration as a dictionary
def getUser(ln, configuration):

    # For each entry we have listed in the configuration dictionary
    for db in configuration:

        # Check to see if the database name listed is in the connect line (Function only used in connects)
        db_entry = str(db) + str('@') + "localhost"

        line = ln.split()        
        if db_entry in line:
            
            # If it is return the dictionary value for this database
            return db

    # If we cant find the user based on the databases and configuration listed return unknown string
    return "Unknown User"

# Function to remove new line characters from a string
# Recieves one string to remove new lines from
def removeNewlines(string):

    string = string.replace('\n', '')
    return string

# Function to remove all instances of a value from a list
# Recieves the list to remove from as a list
# Recieves the value to remove as a string
def removeFromList(list, value):

    # For each value in the list
    while value in list:
        # Remove the value
        list.remove(value)

# Function to return an index minus one
# Recieves one integer
def indexMinusOne(index_given):

    return index_given - 1

# Function to return an index plus one
# Recieves one integer
def indexPlusOne(index_given):

    return index_given + 1

# Function to see if a string is in the dont_save list
# Recieves one string to check
def doNotSave(string, dont_save):

    for queryno in dont_save:
        if string == queryno.strip():
            return 1

#               ########################################
#               ---   BEGIN DEFINING MAIN FUNCTION   ---
#               ########################################

def main():

#               ##############################
#               ---   READ CONFIGURATION   ---
#               ##############################

    configuration = conf.getConfig(args.config_file)

#               ################################
#               ---   SETUP OTHER VARIBLES   ---
#               ################################

    # Define the main array containing information pertaining to the log
    sqllog = []

    # Define a dictionary to keep track of which query numbers belong to which users
    query_user = {}

    # Define list of query numbers that are not to be saved at all
    dont_save = []

    # Define counters
    lineno = 1
    counter = 0

#               ################################    
#               ---   BEGIN PROCESSING LOG   ---
#               ################################

    print

    # If the run only argument was not passed
    if not args.run_only:

        # If the file specified in the args is both a file and exists
        if os.path.exists(file) and os.path.isfile(file):

            # Write the information from the sql log to the array in a 2D format
            with open(file) as sql_log:
                for line in sql_log:
                    # Append List to the current position
                    sqllog.append([])
                    sqllog[counter].insert(COL.Line.value, lineno)
                    sqllog[counter].insert(COL.String.value, removeNewlines(line))
                    lineno += 1
                    counter += 1

            # Print Number of Lines Processed
            print('Read ' + str(counter) + ' Lines...')

            # Double check that if the debug line is specified it is within the range of the file
            if args.debug_line and int(args.debug_line) > int(sqllog[-1][COL.Line.value]):
                print(args.debug_line)
                print(sqllog[-1][COL.Line.value])
                print("Debug Line is beyond the end of the file specified")
                lock.exit()

            zero_to_two = re.compile("^[0-2]{1}")

            is_number = re.compile("^[0-9]+")
            is_time = re.compile("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}")        
            is_type = re.compile("^Query|^Connect|^Quit")
            statistics = re.compile("^Statistics")
            init = re.compile("^Init")
            db = re.compile("^DB")
            field = re.compile("^Field")
            list = re.compile("^List")

            # Information from sql log now stored in array
            # Begin processing array information
            for x in range(len(sqllog)):

                ### IF THIS IS ONE OF THE FIRST THREE LINES  ###
                if x == 0 or x == 1 or x == 2:
                    # Do not process they are just text
                    continue

                ### SPLIT THE LINE (MAINTAIN QUOTES) ###                
                split = re.findall(r'[^"\s]\S*|".+?"', sqllog[x][COL.String.value])

                ### GET THE TYPE AND QUERY NO ###
                # Time Included Format
                # Use regular expressions to check if this is a entry given with a time or not
                if is_number.match(split[0]) and is_time.match(split[1]) and is_number.match(split[2]) and is_type.match(split[3]):

                    # Insert the type to the array
                    sqllog[x].insert(COL.Type.value, getType(split[3]))
                    # Insert the Query Number into the array
                    sqllog[x].insert(COL.QueryNo.value, split[2])
                    # Insert the index of this query to the array
                    sqllog[x].insert(COL.StartIndex.value, x)

                # Regular Format
                # Use regular expressions to check if this is an entry given without a time
                elif is_number.match(split[0]) and is_type.match(split[1]):

                    # Insert the type to the array
                    sqllog[x].insert(COL.Type.value, getType(split[1]))
                    # Insert the Query Number into the array
                    sqllog[x].insert(COL.QueryNo.value, split[0])
                    # Insert the index of this query to the array
                    sqllog[x].insert(COL.StartIndex.value, x)

                # Statistics Command
                # Check to see if this is a statistics command if it is none of the above
                elif (is_number.match(split[0]) and statistics.match(split[1])) or (is_number.match(split[0]) and is_time.match(split[1]) and is_number.match(split[2]) and statistics.match(split[3])):

                    if statistics.match(split[1]):
                        # Insert the type to the array
                        sqllog[x].insert(COL.Type.value, getType(split[1]))
                        # Insert the Query Number into the array
                        sqllog[x].insert(COL.QueryNo.value, split[0])

                    else:
                        # Insert the type to the array
                        sqllog[x].insert(COL.Type.value, getType(split[3]))
                        # Insert the Query Number into the array
                        sqllog[x].insert(COL.QueryNo.value, split[2])

                    # Insert the index of this query to the array
                    sqllog[x].insert(COL.StartIndex.value, x)

                # Init DB Command
                # Check to see if this is a 'Init DB' command if it is none of the above
                elif (is_number.match(split[0]) and init.match(split[1]) and db.match(split[2])) or (is_number.match(split[0]) and is_time.match(split[1]) and is_number.match(split[2]) and init.match(split[3]) and db.match(split[4])):

                    if init.match(split[1]):
                        type = split[1] + " " + split[2]
                        # Insert the type to the array
                        sqllog[x].insert(COL.Type.value, getType(type))
                        # Insert the Query Number into the array
                        sqllog[x].insert(COL.QueryNo.value, split[0])

                    else:
                        type = split[3] + " " + split[4]
                        # Insert the type to the array
                        sqllog[x].insert(COL.Type.value, getType(type))
                        # Insert the Query Number into the array
                        sqllog[x].insert(COL.QueryNo.value, split[2])

                    # Insert the index of this query to the array
                    sqllog[x].insert(COL.StartIndex.value, x)

                # Field List Command
                # Check to see if this is a 'Field List' command if it is none of the above
                elif (is_number.match(split[0]) and field.match(split[1]) and list.match(split[2])) or (is_number.match(split[0]) and is_time.match(split[1]) and is_number.match(split[2]) and field.match(split[3]) and list.match(split[4])):

                    if field.match(split[1]):
                        type = split[1] + " " + split[2]
                        # Insert the type to the array
                        sqllog[x].insert(COL.Type.value, getType(type))
                        # Insert the Query Number into the array
                        sqllog[x].insert(COL.QueryNo.value, split[0])

                    else:
                        type = split[3] + " " + split[4]
                        # Insert the type to the array
                        sqllog[x].insert(COL.Type.value, getType(type))
                        # Insert the Query Number into the array
                        sqllog[x].insert(COL.QueryNo.value, split[2])

                    # Insert the index of this query to the array
                    sqllog[x].insert(COL.StartIndex.value, x)

                # Continued Query
                # Otherwise this is an entry given that is in neither format and must be a continued query
                else:

                    # Insert the type to the array
                    sqllog[x].insert(COL.Type.value, TYPE.Continued.value)
                    # Insert the Query Number into the array
                    sqllog[x].insert(COL.QueryNo.value, sqllog[indexMinusOne(x)][COL.QueryNo.value])
                    # Insert the index of the beginning of this query to the array
                    sqllog[x].insert(COL.StartIndex.value, sqllog[indexMinusOne(x)][COL.StartIndex.value])

                ### STORE USER ON CONNECTS ###
                # If it is a connect statment
                if sqllog[x][COL.Type.value] == TYPE.Connect.value:

                    # Find and insert the query's associated user (IntEnum value) into the main sqllog array using the getUser function
                    sqllog[x].insert(COL.User.value, getUser(sqllog[x][COL.String], configuration))
                    # Store the query number in the query_user dictionary and associate the correct user to it
                    query_user[sqllog[x][COL.QueryNo.value]] = sqllog[x][COL.User.value]                

                ### GET/STORE USER ON OTHERS ###
                # Insert the user for the current user into the array by fetching it from the query_user array where it is stored on the initial connect statement
                if sqllog[x][COL.Type.value] != TYPE.Connect.value:
                    try:
                        sqllog[x].insert(COL.User.value, query_user[sqllog[x][COL.QueryNo.value]])
                    except:
                        sqllog[x].insert(COL.User.value, "Unknown User")

                ### CHECK IF THIS COMMAND WAS MADE BY THE SCRIPT ###
                if '789IGNORETHISSESSION987' in sqllog[x][COL.String.value]:
                    dont_save.append(sqllog[x][COL.QueryNo.value])
                    print("Ignoring Session " + sqllog[x][COL.QueryNo.value] + ' (Recorded Previously)')

                ### STORE CONTINUED COMMANDS ON THE END OF THE PREVIOUS ###
                if sqllog[x][COL.Type.value] == TYPE.Continued.value:
                    sqllog[sqllog[x][COL.StartIndex.value]][COL.String.value] = sqllog[sqllog[x][COL.StartIndex.value]][COL.String.value] + " " + sqllog[x][COL.String.value]

#               ###########################
#               ---   STORE INFORMATION ---
#               ###########################

            # Check to see if the export directory exists, if it does not create it
            if not os.path.isdir(configuration["Global"][conf.GLOBAL.Export.value]):
                # If the path exists but its not a directory error as a bad folder has been given
                if os.path.exists:
                    print("Config Export path is not an directory and exists")
                    lock.exit()
                # Otherwise create the directory
                else:
                    os.makedirs(configuration["Global"][conf.GLOBAL.Export.value])

            # Count number of Lines that Need to be Saved
            count = 0
            skipped = 0

            # For each line in the log
            for x in range(len(sqllog)):

                # If x is equal to zero, one or two then skip the iteration as we dont process the first three lines
                if x == 0 or x == 1 or x == 2:
                    continue

                # No need to check users that were not set to record
                if sqllog[x][COL.User.value] == "Unknown User":

                    continue

                # We need to skip any query numbers that have been put in dont save because they were run by this script
                if doNotSave(sqllog[x][COL.QueryNo.value], dont_save):

                    skipped += 1
                    continue

                ### DECIDE WHETHER TO SAVE OR NOT ###
                # We only save queries
                if sqllog[x][COL.Type.value] == TYPE.Query.value:

                    # Check the ignore tables if nothing is set then we dont check it
                    if configuration[sqllog[x][COL.User.value]][conf.CONFIG.IgnoreTables.value][0] == "":

                        # If there is a recordable command then set save to one otherwise 0
                        if any(cmd in sqllog[x][COL.String.value] for cmd in configuration["Global"][conf.GLOBAL.Record.value]):
                            sqllog[x].insert(COL.Save.value, '1')
                        else:
                            sqllog[x].insert(COL.Save.value, '0')

                    # Otherwise if the line contains a sql command we have set it to record and not a table set to ignore from the configuration
                    elif any(cmd in sqllog[x][COL.String.value] for cmd in configuration["Global"][conf.GLOBAL.Record.value]): 

                        split = re.findall(r'[^"\s]\S*|".+?"', sqllog[x][COL.String.value])
                        next_cmd = re.compile("^[A-Z]{2,9}")
                        not_null = re.compile("(?!NULL)")
#                        additional = re.compile("(?!(TEMPORARY)?(TABLE)?(IF)?(NOT)?(EXISTS)?)")
                        additional = re.compile("TABLE|TEMPORARY|IF|NOT|EXISTS")

                        # For each command we want to record
                        for cmd in configuration["Global"][conf.GLOBAL.Record.value]:

                            next_sqlcommand = []

                            # IS THE COMMAND IN THE LINE
                            # Try to find it in the list of elements from the current line
                            try:
                                recorded_cmd = split.index(cmd)

                            # If we cant find it then continue with the next command that is recordable
                            except:
                                continue

                            # WHAT STATEMENTS FOLLOW IT
                            # Try to find the next command after the recorded one
                            # For each element in the split
                            for z in range(len(split)):

                                # Check to see if it is a command
                                if next_cmd.match(split[z]) and not_null.match(split[z]) and not additional.match(split[z]):

                                    # Ensure that the next command found cannot be a second part of the current command unless it is a FROM eg. DELETE FROM, INSERT INTO or START TRANSACTION
                                    if (split[z] == "FROM" and z > recorded_cmd) or (z > recorded_cmd and z != indexPlusOne(recorded_cmd)):

                                        # If it is a command then add its index to the list of command indexes following the recordable commands
                                        next_sqlcommand.append(z)

                            # NO STATEMENTS FOLLOWING
                            # If nothing is in the commands after recordable command
                            if not next_sqlcommand:

                                # Search everything after the cmd (recorded_cmd)
                                # For each table in the ignore tables for this queries user
                                for ignore_table in configuration[sqllog[x][COL.User.value]][conf.CONFIG.IgnoreTables.value]:

                                    # For each index between this and the last one
                                    for i in range(recorded_cmd, len(split)):
                                        
                                        # If we find a table we need to ignore then set save to 0
                                        if ignore_table == split[i]:
                                            sqllog[x].insert(COL.Save.value, '0')
                                            break

                                    # If we have already found a table that causes us to ignore this query then break from for
                                    if len(sqllog[x]) == 7 and sqllog[x][COL.Save.value] == '0':
                                        break

                            # FROM STATEMENT FOLLOWING IT
                            # If the next index is a FROM
                            elif split[next_sqlcommand[0]] == "FROM":

                                # If there is no commands following beyond FROM then set the search to check for tables to ignore until the end of the line
                                if len(next_sqlcommand) <= 1:
                                    next_sqlcommand.append(indexMinusOne(len(split)))
                                
                                # Search everything between FROM (next_sqlcommand[0]) and the next command (next_sqlcommand[1])
                                # For each table in the ignore tables for this queries user
                                for ignore_table in configuration[sqllog[x][COL.User.value]][conf.CONFIG.IgnoreTables.value]:

                                    # For each index between this and the next command
                                    for i in range(next_sqlcommand[0], next_sqlcommand[1]):

                                        # If we find a table we need to ignore then set save to 0
                                        if ignore_table == split[i]:
                                            sqllog[x].insert(COL.Save.value, '0')
                                            break

                                    # If we have already found a table that causes us to ignore this query then break from for
                                    if len(sqllog[x]) == 7 and sqllog[x][COL.Save.value] == '0':
                                        break

                            # FROM STATEMENT NOT FOLLOWING IT
                            # If the next command is not a FROM
                            else:

                                # Search everything between the cmd (recorded_cmd) and the next command (next_sqlcommand[0])
                                # For each table in the ignore tables for this queries user
                            
                                for ignore_table in configuration[sqllog[x][COL.User.value]][conf.CONFIG.IgnoreTables.value]:

                                    # If this is an ALTER TABLE statement then dont look for the table with backticks
                                    if split[recorded_cmd] == "ALTER" and split[indexPlusOne(recorded_cmd)] == "TABLE":
                                         # For each index between this and the next command
                                        for i in range(recorded_cmd, next_sqlcommand[0]):

                                            # If we find a table we need to ignore then set save to 0
                                            if ignore_table.translate(None, '`') == split[i]:
                                                sqllog[x].insert(COL.Save.value, '0')
                                                break

                                    # Otherwise look for the tables normally
                                    else:
                                        # For each index between this and the next command
                                        for i in range(recorded_cmd, next_sqlcommand[0]):

                                            # If we find a table we need to ignore then set save to 0
                                            if ignore_table == split[i]:
                                                sqllog[x].insert(COL.Save.value, '0')
                                                break



                                    # If we have already found a table that causes us to ignore this query then break from for
                                    if len(sqllog[x]) == 7 and sqllog[x][COL.Save.value] == '0':
                                        break

                        # If we have made our way through not found any to ignore then set save to 1
                        if len(sqllog[x]) != 7:
                            sqllog[x].insert(COL.Save.value, '1')

                        # We will only hit that if a query makes it all the way through without having any value set which should not be possible so error
                        elif len(sqllog[x]) < 7:
                            print("Didnt set save value")
                            lock.exit()

                    # Otherwise if there is no recordable command in the line
                    else:
                        sqllog[x].insert(COL.Save.value, '0')

                # Otherwise if not a query set save to 0
                else:
                    sqllog[x].insert(COL.Save.value, '0')

                # If the log has been set to save
                if sqllog[x][COL.Save.value] == '1':

                    # Make a note of the how many lines were set to save
                    count += 1

                    # Split the query and store only the valid sql query from this line
                    sqllog[x].insert(COL.Query.value, getQuery(sqllog[x][COL.String.value], "Query"))

                    # Only save if not debugging
                    if not args.debug_line:

                        # Check each user in the configuration to see if they are the user of the query
                        for user in configuration:
                            if sqllog[x][COL.User.value] == user:

                                # Write the code to a variable
                                write = sqllog[x][COL.Query.value] + "\n"
                                # Save the code to the correct file
                                export = open(configuration[user][conf.CONFIG.Export.value], "a")
                                export.write(write)
                                export.close()

            # Print Number of Lines set to Save
            print(str(count) + ' Lines set to Save...')
            print('Skipped ' + str(skipped) + ' Lines...')
            print('Saved Lines')

#               #####################
#               ---  FILTER FILES ---
#               #####################

            # Check each users configuration and filter the output files if they are configured to do so
            for user in configuration:
                if user != "Global" and user != "Unknown User":
                    if configuration[user][conf.CONFIG.Filter.value] == "1":
                        filter.filter(configuration[user][conf.CONFIG.Export.value])

#               ####################
#               ---   DEBUGGING  ---
#               ####################

            # If the debug clause is included show us the list for that line
            if args.debug_line is not None:

                # Convert the number given to the array index of the line        
                test_index = int(args.debug_line) - 1

                # Set variables for each element of the line set to debug
                test_line = sqllog[test_index][COL.Line]
                test_string = sqllog[test_index][COL.String]
                test_string = test_string.replace('\t', ' ')
                try:
                    test_validquery = sqllog[test_index][COL.Query.value]
                except:
                  pass
                try:
                    test_type = TYPE(sqllog[test_index][COL.Type]).name
                except:
                    pass
                try:
                    test_query = sqllog[test_index][COL.QueryNo.value]
                except:
                    pass
                try:
                    test_user = sqllog[test_index][COL.User.value]
                except:
                    pass
                try:
                    test_save = sqllog[test_index][COL.Save.value]
                except:
                    pass

                # Begin printing a debug of the line
                print
                print
                print("Log Line " + str(test_line) + " Contains...")
                print
                print("Index " + str(test_index) + " of Sql Log Array: ")
                print
                print(str(test_string))
                print
                try:
                    print("Query Number " + str(test_query))
                except:
                    print("(No Query Number)")
                    print("The Line Type Stored is " + str(test_type))
                try:
                    print("The User of the Stored Line is " + str(test_user))
                except:
                    pass
                try:
                    print("Save is set to " + str(test_save))
                except:
                    pass
                try:
                    print
                    print("Valid SQL Query: ")
                    try:
                        print(str(test_validquery))
                    except:
                        print("Query not saved")
                except:
                    pass
                print

#               ######################################
#               ---   IF LOG FILE DOES NOT EXIST   ---
#               ######################################

        else:
            print("File does not exist or is a directory")

#               #####################
#               ---   RUN FILE ---
#               #####################

    # If the run flag was set then run the database changes as per the configuration
    if args.run or args.run_only:

        for user in configuration:
            print
            runsql.runSQL(user, configuration)


#               ######################
#               ---   SHOW RUNTIME ---
#               ######################

    print
    print("Total time of execution: " + str(datetime.now() - startTime))
    print
    lock.exit()

#               ########################
#               ---   EXECUTE MAIN   ---
#               ########################

if __name__ == '__main__':
    main()
