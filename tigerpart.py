# a script to make a list of selected outputs from all mdb files in a particular directory

import adodbapi
import os
import pyodbc
import csv
import time
 
#db_path = 'C:/Users/tbaer/Desktop/m1a1/doe/2nonotion/'
db_path = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/DOE/2/'
#db_path = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/Model Building/9-18-14 models/10 per quarter induct redeploy/02 and 04-11 normal effect - discard SOW/'

filelist = os.listdir(db_path) # file_list will be a list of strings
outfile = './3perqtrmain.csv'
results = [['Filename','Graph Label','date','histories','totaleros','relevantAo','depotLLrep','depotLLcond','depotOthrcond','fr0','fr1']] #first item in results list is list of column headers

def connectionString(path, filename):
    '''Return a pyodbc connection string for the Microsoft Access database at 
    the given path and filename.
    '''
    return 'Driver={{Microsoft Access Driver (*.mdb, *.accdb)}};DBQ={p};'.format(p=os.path.join(path, filename))

# get the file
source_mdb = 'C:/Users/tbaer/Desktop/m1a1/doe/m1baseline.mdb'
# move parts
qry_tigerparts = 'SELECT * from tigerparts'
#conn = adodbapi.connect(constr)
conStr = connectionString(db_path, source_mdb)
conn = pyodbc.connect(conStr)
# create a cursor
cur = conn.cursor()
cur.execute(qry_tigerparts)
tiger_parts = cur.fetchall()
headers = [column[0] for column in cur.description]
# close the cursor and connection
cur.close()
conn.close()

qry_createTPtable = 'CREATE TABLE tigerparts (id int, NOMEN varchar(55), NSN varchar(25), objecttype integer, condemnable varchar(25))'
qry_insertTPtable = 'INSERT INTO tigerparts values (%d,%s,%s,%d,%s)' 

# Make a list of mdb files found in chosen path
mdb_list = []

for filename in filelist:
    name_parts = filename.split('.')
    ext = name_parts.pop() 
    if ext == 'MDB' or ext == 'mdb':
        mdb_list.append(filename)

# default values
graph_label = 'label not found'
out1_value = 0
out2_value = 0


for mdb_filename in mdb_list:
    full_filename = db_path + mdb_filename
    print 'changing things in database:', full_filename, '.......'

    #fill in some tables with Insert queries (temporarily)
    
    #run the queries here.
    this_dbs_outs = [mdb_filename]
    
    #constr = 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=%s'  % full_filename
    # connect to the database
    #conn = adodbapi.connect(constr)
    conStr = connectionString(db_path, mdb_filename)
    conn = pyodbc.connect(conStr)

    # create a cursor
    cur = conn.cursor()

    try:
        cur.execute(qry_createTPtable)
        cur.executemany('INSERT INTO tigerparts values (?,?,?,?,?)', tiger_parts)
        # commit the changes
        conn.commit()
    except:
        # bail out
        print 'tigerparts already exists for %s' % mdb_filename
    # close the cursor and connection
    cur.close()
    conn.close()
    #time.sleep(1)

print 'Finished'

