# a script to change values in all mdb files in a particular directory

import os
import csv
import pyodbc
 
db_path = 'C:\Users\rgallagher\Desktop\DOE'
filelist = os.listdir(db_path) # file_list will be a list of strings

# Make a list of mdb files found in chosen path
mdb_list = []

for filename in filelist:
    name_parts = filename.split('.')
    ext = name_parts.pop() 
    if ext == 'MDB' or ext == 'mdb':
        mdb_list.append(filename)

# default values
in1_value = 91

for mdb_filename in mdb_list:
    full_filename = db_path + mdb_filename
    print 'changing model data in database:', full_filename, '.......'
    qry_in1 = 'UPDATE [*Operation profiles] SET [*Operation profiles].[Platform type] = [platform type]+99 WHERE ((([*Operation profiles].[Platform type])<>100101001))'
	
    #run the queries here.
    this_dbs = [mdb_filename]
    
    constr = 'Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;'  % full_filename
    # connect to the database
    conn = pyodbc.connect(constr)
 
    # create a cursor
    cur = conn.cursor()
	
	# execute the query
    cur.execute(qry_in1)
  
    # commit the changes
    conn.commit()
    
    # close the cursor and connection
    cur.close()
    conn.close()

print 'All done!'


