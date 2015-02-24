# a script to insert data into tables

import os
import pyodbc
import csv
import time
 
#db_path = 'C:/Users/tbaer/Desktop/m1a1/doe/2nonotion/'
#db_path = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/DOE/fewmore/'
db_path = 'Z:/Temp/tbaer/2015/m777 winter 14-15/7loc/'

filelist = os.listdir(db_path) # file_list will be a list of strings
outfile = './3perqtrmain.csv'
results = [['Filename','Graph Label','date','histories','totaleros','relevantAo','depotLLrep','depotLLcond','depotOthrcond','fr0','fr1']] #first item in results list is list of column headers

def connectionString(path, filename):
    '''Return a pyodbc connection string for the Microsoft Access database at 
    the given path and filename.
    '''
    return 'Driver={{Microsoft Access Driver (*.mdb, *.accdb)}};DBQ={p};'.format(p=os.path.join(path, filename))

# get the file
source_mdb = 'Z:/Temp/tbaer/2015/m777 winter 14-15/7loc/NoCap/AM16Baseline7Loc30_NoCap1000.mdb'
# move parts
qry_amt = 'SELECT * from amt'
qry_amt_cmpm = 'SELECT * from amt_cmpm'
qry_amt_cmpm_base = 'SELECT * from amt_cmpm_base'
qry_amt_cmpm_mef = 'SELECT * from amt_cmpm_mef'
qry_amt_mef = 'SELECT * from amt_mef'

#conn = adodbapi.connect(constr)
conStr = connectionString(db_path, source_mdb)
conn = pyodbc.connect(conStr)
# create a cursor
cur = conn.cursor()
# get all the relevant data
cur.execute(qry_amt)
amtdata = cur.fetchall()
amtheaders = [column[0] for column in cur.description]

cur.execute(qry_amt_cmpm)
amtcmpmdata = cur.fetchall()
amtcmpmheaders = [column[0] for column in cur.description]

cur.execute(qry_amt_cmpm_base)
amtcmpmbasedata = cur.fetchall()
amtcmpmbaseheaders = [column[0] for column in cur.description]

cur.execute(qry_amt_cmpm_mef)
amtcmpmmefdata = cur.fetchall()
amtcmpmmefheaders = [column[0] for column in cur.description]

cur.execute(qry_amt_mef)
amtmefdata = cur.fetchall()
amtmefheaders = [column[0] for column in cur.description]

# close the cursor and connection
cur.close()
conn.close()

qry_createAMTtable = 'CREATE TABLE amt ("Type being Built" int, AvgAMT numeric)'
qry_insertAMTtable = 'INSERT INTO amt values (%d,%d)' 

qry_createAMTcmpmtable = 'CREATE TABLE amt_cmpm ("Type being Built" int, cmpm varchar(55), AvgAMT numeric)'
qry_insertAMTcmpmtable = 'INSERT INTO amt_cmpm values (%d,%s,%d)' 

qry_createAMTcmpmbasetable = 'CREATE TABLE amt_cmpm_base ("Type being Built" int, cmpm varchar(55), sran int, basename varchar(55), AvgAMT numeric)'
qry_insertAMTcmpmbasetable = 'INSERT INTO amt_cmpm_base values (%d,%s,%d,%s,%d)'

qry_createAMTcmpmmeftable = 'CREATE TABLE amt_cmpm_mef ("Type being Built" int, cmpm varchar(55), mef varchar(55), AvgAMT numeric)'
qry_insertAMTcmpmmeftable = 'INSERT INTO amt_cmpm_mef values (%d,%s,%s,%d)' 

qry_createAMTmeftable = 'CREATE TABLE amt_mef ("Type being Built" int, mefnum int, mef varchar(55), AvgAMT numeric)'
qry_insertAMTmeftable = 'INSERT INTO amt_mef values (%d,%d,%s,%d)' 


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
        cur.execute(qry_createAMTtable)
        cur.executemany('INSERT INTO amt values (?,?)', amtdata)
    except:
        print 'awm already exists for %s' % mdb_filename
        cur.execute('drop table amt')
        cur.execute(qry_createAMTtable)
        cur.executemany('INSERT INTO amt values (?,?)', amtdata)

    try:
        cur.execute(qry_createAMTcmpmtable)
        cur.executemany('INSERT INTO amt_cmpm values (?,?,?)', amtcmpmdata)
    except:
        print 'awm_cmpm already exists for %s' % mdb_filename
        cur.execute('drop table amt_cmpm')
        cur.execute(qry_createAMTcmpmtable)
        cur.executemany('INSERT INTO amt_cmpm values (?,?,?)', amtcmpmdata)

    try:
        cur.execute(qry_createAMTcmpmbasetable)
        cur.executemany('INSERT INTO amt_cmpm_base values (?,?,?,?,?)', amtcmpmbasedata)
    except:
        print 'awm_cmpm_base already exists for %s' % mdb_filename
        cur.execute('drop table amt_cmpm_base')
        cur.execute(qry_createAMTcmpmbasetable)
        cur.executemany('INSERT INTO amt_cmpm_base values (?,?,?,?,?)', amtcmpmbasedata)

    try:
        cur.execute(qry_createAMTcmpmmeftable)
        cur.executemany('INSERT INTO amt_cmpm_mef values (?,?,?,?)', amtcmpmmefdata)
    except:
        print 'awm_cmpm_mef already exists for %s' % mdb_filename
        #cur.execute('drop table ampm_cmpm_mef')
        #cur.execute(qry_createAMTcmpmmeftable)
        #cur.executemany('INSERT INTO amt_cmpm_mef values (?,?,?,?)', amtcmpmmefdata)

    try:
        cur.execute(qry_createAMTmeftable)
        cur.executemany('INSERT INTO amt_mef values (?,?,?,?)', amtmefdata)
    except:
        cur.execute('drop table amt_mef')
        cur.execute(qry_createAMTmeftable)
        cur.executemany('INSERT INTO amt_mef values (?,?,?,?)', amtmefdata)
        print 'awm_mef already exists for %s' % mdb_filename

    # commit the changes
    conn.commit()
    # close the cursor and connection
    cur.close()
    conn.close()
    #time.sleep(1)

print 'Finished'

