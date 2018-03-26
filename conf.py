#!/usr/bin/python

import os.path
import os
import re
import ConfigParser
from enum import Enum
from enum import IntEnum
from optparse import OptionParser

import lock

#              ##############################
#              ---   SCRIPT REQUIREMENTS  ---
#              ##############################
#
#         Requires Python Version 2.6.6 or above.
#         Requires Python 3.4 Enum Backported
#            - enum34 1.1.6

# Configuration Options
class CONFIG(IntEnum):
    Database = 0
    IgnoreTables = 1
    Export = 2
    Filter = 3
    Pass = 4
    Run = 5

class GLOBAL(IntEnum):
    Export = 0
    Record = 1

parser = OptionParser()

try:
    config_dir = os.path.dirname(os.path.realpath(__file__)) + "/config/"
except:
    print("Error inializing conf module. Cannot find file path.")
    lock.exit()

def getConfig(config_file):

    config_file = config_dir + config_file

    # If a configuration is specified
    if config_file:

        # If the file specified either does not exist or is not a file
        if not os.path.exists(config_file) and os.path.isfile(config_file):
            print("Specified config file does not exist or is not a file")
            lock.exit()

        else:
            try:
                config = ConfigParser.ConfigParser()
                config.read(config_file)
            except:
                print("Error Reading Config file")
                lock.exit()

    # FETCH DATABASES AND DATABASE SPECIFIC CONFIGURATIONS
    # Define a dictionary for the config of the databases
    configuration = {}

    # FETCH Export Directory
    # Try to fetch the export directory
    try:
        configuration["Global"] = []
        configuration["Global"].insert(GLOBAL.Export.value, config.get('SYSTEM', 'export'))
    except:
        print("Error fetching export directory")
        lock.exit()

    # FETCH RECORDABLE QUERIES
    # Define an array for the recordable sql queries, processed and unprocessed
    record = []
    count = 0

    # Try to fetch the recordable sql queries from the config and input them into the array
    try:
        configuration["Global"].append([])
        record = config.get('SYSTEM', 'record').split(',')
        if len(record) == 0:
            print("Unable to read record from system")
            lock.exit()
        elif record[0] == "":
            print("No SQL Queries specified for recording")
            lock.exit()
        else:
            # We strip them individually so that SQL commands with a space do not have them removed
            for sqlcmd in record:
                sqlcmd = sqlcmd.strip()
                configuration["Global"][GLOBAL.Record.value].insert(count, sqlcmd)
                count += 1
    except:
        print("Unable to read record from system")
        lock.exit()

    # Try to split the databases from the configuration into a list with whitespace removed
    try:
        databases = re.sub('[\s+]', '', config.get('SYSTEM', 'databases')).split(',')
    except:
        print("Databases incorrectly specified in configuration")
        lock.exit()

    # Using the split up list of databases as a basis for the number of entries in the list
    # Begin moving the config to the configuration list
    for db in databases:

        # Try and get the user for the current entry and store them in a variable
        try:
            current_user = config.get(db, 'user')
        except:
            print("Unable to find the user value in the configuration for %(dbs)s in the configuration file" % {'dbs' : db})
            lock.exit()

        # Set the dictionary value for the current user as a list in configuration
        configuration[current_user] = []
        # Insert to that list at the database index the database name
        configuration[current_user].insert(CONFIG.Database.value, db)

        # Try and separate out the tables to ignore for this user and then insert them into the configuration dictionary at the
        # ignoretables index of the user element
        try:
            ignore_tables = re.sub('[\s+]', '', config.get(db, 'ignore_tables')).split(',')
            configuration[current_user].insert(CONFIG.IgnoreTables.value, ignore_tables)
        except:
            print(configuration)
            print("Unable to read specified ignore_tables for %(dbs)s" %{'dbs' : db})
            lock.exit()

        # Add backticks around any tables that need to be ignored
        if configuration[current_user][CONFIG.IgnoreTables.value][0] != '':
            for x in range(len(configuration[current_user][CONFIG.IgnoreTables.value])):
                configuration[current_user][CONFIG.IgnoreTables.value][x] = '`' + configuration[current_user][CONFIG.IgnoreTables.value][x] + '`'

        # Set the database export location
        try:
            configuration[current_user].insert(CONFIG.Export.value, configuration["Global"][GLOBAL.Export.value] + db)
        except:
            print("Unable to set database export location")
            lock.exit()

        # Try to get the filter value from the user
        try:
            configuration[current_user].insert(CONFIG.Filter.value, config.get(db, 'filter_output'))
        except:
            configuration[current_user].insert(CONFIG.Filter.value, '0')

        # Try to fetch the users password
        try:
            configuration[current_user].insert(CONFIG.Pass.value, config.get(db, 'password'))
        except:
            configuration[current_user].insert(CONFIG.Pass.value, 'None')

        # Try to separate out databases to run queries on, if there are multiple, then store
        try:
            runon = re.sub('[\s+]', '', config.get(db, 'runon')).split(',')
            configuration[current_user].insert(CONFIG.Run.value, runon)
        except:
            print(runon)
            print("Unable to read specified runon configuration")
            lock.exit()

    configuration["Unknown User"] = []
    configuration["Unknown User"].insert(CONFIG.Database.value, "None")
    configuration["Unknown User"].insert(CONFIG.IgnoreTables.value, "None")
    configuration["Unknown User"].insert(CONFIG.Filter.value, "0")
    configuration["Unknown User"].insert(CONFIG.Pass.value, "None")

    return configuration
