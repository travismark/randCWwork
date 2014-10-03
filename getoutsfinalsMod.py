# a script to make a list of selected outputs from all mdb files in a particular directory

import adodbapi
import os
import pyodbc
import csv
 
#db_path = 'C:/Users/tbaer/Desktop/m1a1/doe/2nonotion/'
db_path1 = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/Model Building/9-18-14 models/3 per quarter induct redeploy/'
db_path2 = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/Model Building/9-18-14 models/3 per quarter induct redeploy/02 and 04-11 normal effect - discard SOW/'
db_path3 = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/Model Building/9-18-14 models/6 per quarter induct redeploy/'
db_path4 = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/Model Building/9-18-14 models/6 per quarter induct redeploy/new 03 and 03A/'
db_path5 = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/Model Building/9-18-14 models/6 per quarter induct redeploy/02 and 04-11 normal effect - discard SOW/'
db_path6 = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/Model Building/9-18-14 models/10 per quarter induct redeploy/'
db_path7 = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/Model Building/9-18-14 models/10 per quarter induct redeploy/new 03 and 03A/'
db_path8 = 'P:/External Projects/3104 - ALPS 2014/M1A1/Models/Model Building/9-18-14 models/10 per quarter induct redeploy/02 and 04-11 normal effect - discard SOW/'

outfile = './alls.csv'
results = [['Path','Filename','Graph Label','date','histories','totaleros','relevantAo','depotLLrep','depotLLcond','depotOthrcond',
'depotcondemnTig','depotcondemnNotTig','repairIMAlow','repairIMAhigh','repairDepotLow','repairDepotLow2','repairDepotHigh','repairDepotHigh2']] #first item in results list is list of column headers

def connectionString(path, filename):
    '''Return a pyodbc connection string for the Microsoft Access database at 
    the given path and filename.
    '''
    return 'Driver={{Microsoft Access Driver (*.mdb, *.accdb)}};DBQ={p};'.format(p=os.path.join(path, filename))

def connectionStringOneInput(path):
    '''Return a pyodbc connection string for the Microsoft Access database at 
    the given path and filename.
    '''
    return 'Driver={{Microsoft Access Driver (*.mdb, *.accdb)}};DBQ={p};'.format(p=path)



# Make a list of files found in chosen path - make sure they're all .mdbs first
path_list = []
filelist1 = os.listdir(db_path1) # file_list will be a list of strings
for filename in filelist1:
        path_list.append(os.path.join(db_path1,filename))

print path_list[0]
print path_list[1]

filelist2 = os.listdir(db_path2) # file_list will be a list of strings
for filename in filelist2:
        path_list.append(os.path.join(db_path2,filename))
filelist3 = os.listdir(db_path3) # file_list will be a list of strings
for filename in filelist3:
        path_list.append(os.path.join(db_path3,filename))
filelist4 = os.listdir(db_path4) # file_list will be a list of strings
for filename in filelist4:
        path_list.append(os.path.join(db_path4,filename))
filelist5 = os.listdir(db_path5) # file_list will be a list of strings
for filename in filelist5:
        path_list.append(os.path.join(db_path5,filename))
filelist6 = os.listdir(db_path6) # file_list will be a list of strings
for filename in filelist6:
        path_list.append(os.path.join(db_path6,filename))
filelist7 = os.listdir(db_path7) # file_list will be a list of strings
for filename in filelist7:
        path_list.append(os.path.join(db_path7,filename))
filelist8 = os.listdir(db_path8) # file_list will be a list of strings
for filename in filelist8:
        path_list.append(os.path.join(db_path8,filename))


# default values
graph_label = 'label not found'
out1_value = 0
out2_value = 0
    
# label
qry_label = 'SELECT First([*Description].[Graph Label]) FROM [*Description]'
# sim date
qry_simDate = 'SELECT [*Versions].[Simulation Date] FROM [*Versions]'
# histories
qry_histories = 'SELECT [*Versions].[Number of Histories] FROM [*Versions]'
# total EROs
qry_totaleros = 'SELECT Sum([out LRU events].UER) AS SumOfUER FROM [out LRU events]'
# total ENSIP (LL repairs at depot)
qry_totalDepLLRep = 'SELECT Sum([ENSIP avg]) AS SumOfensip FROM [out Depot ENSIP and Condemnation]'
# total ENSIP (LL cond at depot)
qry_totalDepLLCond = 'SELECT Sum([Cond ENSIP avg]) AS SumOfensip FROM [out Depot ENSIP and Condemnation]'
# total ENSIP (other cond at depot)
qry_totalDepOthCond = 'SELECT Sum([other Condemn avg]) AS SumOfensip FROM [out Depot ENSIP and Condemnation]'
# Condemnations Cost - Tiger Parts
qry_CCt = 'SELECT Sum([out Condemnations].[average]*[doc].[spare cost]*0.65) AS cost FROM ([out Condemnations] INNER JOIN (SELECT DOC.[Object Type], DOC.[Spare Cost] FROM DOC GROUP BY DOC.[Object Type], DOC.[Spare Cost])  AS doc ON [out Condemnations].[object type] = doc.[object type]) INNER JOIN tigerparts ON doc.[Object Type] = tigerparts.objecttype'
# Condemnation Cost - Non Tiger Parts
qry_CCnt = '''SELECT Sum([out Condemnations].[average]*[doc].[spare cost]) AS cost
FROM [out Condemnations] INNER JOIN (SELECT DOC.[Object Type], DOC.[Spare Cost]
FROM DOC GROUP BY DOC.[Object Type], DOC.[Spare Cost])  
AS doc ON [out Condemnations].[object type] = doc.[object type]
WHERE [out condemnations].[object type] not in (select objecttype from tigerparts)'''
# Repair Cost - IMA Low
qry_CRil = '''SELECT Sum([out LRU events].UER*doccost.[spare cost]*0.17) AS SumOfUER
FROM [*NRTS] INNER JOIN ((SELECT DOC.[Object Type], DOC.[Spare Cost] FROM DOC where [object type] not in (select objecttype from tigerparts) GROUP BY DOC.[Object Type], DOC.[Spare Cost])  AS doccost INNER JOIN ([out LRU events] INNER JOIN [*Object type] ON [out LRU events].[Engine type] = [*Object type].[Object type]) ON doccost.[Object Type] = [*Object type].[Object type]) ON [*NRTS].Type = [*Object type].[Object type]
WHERE ((([*Object type].[Repair Level])=1 Or ([*Object type].[Repair Level])=2) AND (([*NRTS].Probability)=0))'''
# Repair Cost - IMA High
qry_CRih = '''SELECT Sum([out LRU events].UER*doccost.[spare cost]*0.21) AS SumOfUER
FROM [*NRTS] INNER JOIN ((SELECT DOC.[Object Type], DOC.[Spare Cost] FROM DOC where [object type] not in (select objecttype from tigerparts) GROUP BY DOC.[Object Type], DOC.[Spare Cost])  AS doccost INNER JOIN ([out LRU events] INNER JOIN [*Object type] ON [out LRU events].[Engine type] = [*Object type].[Object type]) ON doccost.[Object Type] = [*Object type].[Object type]) ON [*NRTS].Type = [*Object type].[Object type]
WHERE ((([*Object type].[Repair Level])=1 Or ([*Object type].[Repair Level])=2) AND (([*NRTS].Probability)=0))'''
# Repair Cost - Depot Low
qry_CRdl = '''SELECT Sum([out LRU events].UER*doccost.[spare cost]*0.30) AS SumOfUER
FROM ((SELECT DOC.[Object Type], DOC.[Spare Cost] FROM DOC WHERE (((DOC.[object type]) Not In (select objecttype from tigerparts)) AND ((DOC.[object type])<>210215001)) GROUP BY DOC.[Object Type], DOC.[Spare Cost])  AS doccost INNER JOIN ([out LRU events] INNER JOIN [*Object type] ON [out LRU events].[Engine type] = [*Object type].[Object type]) ON doccost.[Object Type] = [*Object type].[Object type]) INNER JOIN [*NRTS] ON [out LRU events].[Engine type] = [*NRTS].Type
WHERE ((([*Object type].[Repair Level])=2) AND (([*NRTS].Probability)=1))'''
# Repair Cost - Depot Low - One Part
qry_CRdl_2 = '''SELECT DOC.[Object Type], DOC.[Spare Cost]*0.05*0.30
FROM DOC WHERE (((DOC.[object type])=210215001))
GROUP BY DOC.[Object Type], DOC.[Spare Cost]'''
# Repair Cost - Depot High
qry_CRdh = '''SELECT Sum([out LRU events].UER*doccost.[spare cost]*0.60) AS SumOfUER
FROM ((SELECT DOC.[Object Type], DOC.[Spare Cost] FROM DOC WHERE (((DOC.[object type]) Not In (select objecttype from tigerparts)) AND ((DOC.[object type])<>210215001)) GROUP BY DOC.[Object Type], DOC.[Spare Cost])  AS doccost INNER JOIN ([out LRU events] INNER JOIN [*Object type] ON [out LRU events].[Engine type] = [*Object type].[Object type]) ON doccost.[Object Type] = [*Object type].[Object type]) INNER JOIN [*NRTS] ON [out LRU events].[Engine type] = [*NRTS].Type
WHERE ((([*Object type].[Repair Level])=2) AND (([*NRTS].Probability)=1))'''
# Repair Cost - Depot High - One Part
qry_CRdh_2 = '''SELECT DOC.[Object Type], DOC.[Spare Cost]*0.05*0.60
FROM DOC WHERE (((DOC.[object type])=210215001))
GROUP BY DOC.[Object Type], DOC.[Spare Cost]'''

# total availability (relevant bases)
qry_avgRelevantAo = '''SELECT Sum(avTdep)/sum(sumofDeployed) AS Avail
FROM (SELECT [out Availability].SRAN, [out Availability].X, Sum([Availability]*[#Deployed]) AS avTdep, Sum([out Availability].[#Deployed]) AS sumofDeployed
FROM [out Availability] where sran between 10001 and 10005 GROUP BY [out Availability].SRAN, [out Availability].X)  AS theQuery'''

print path_list[1]

for path_filename in path_list:
    print path_filename
    peles = path_filename.split('/')
    mdb_filename = peles[-1]
    #full_filename = db_path + mdb_filename
    print 'grabbing outputs from database:', mdb_filename, '.......'

    #fill in some tables with Insert queries (temporarily)
    
    #run the queries here.
    this_dbs_outs = [mdb_filename]
    
    #constr = 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=%s'  % full_filename
    # connect to the database
    #conn = adodbapi.connect(constr)
    #conStr = connectionString(db_path, mdb_filename)

    try: 

        conStr = connectionStringOneInput(path_filename)
        conn = pyodbc.connect(conStr)

        # create a cursor
        cur = conn.cursor()
        # append file path
        this_dbs_outs.append(path_filename)

    except:
        print 'found a folder'
        continue

    	# now get a row of the csv
        cur.execute(qry_label)
        qry_result = cur.fetchall() # this is an SQL rows object - must take first element of first row and convert to the correct type 
        graph_label = str(qry_result[0][0])
        this_dbs_outs.append(graph_label)
        
    try: 
        cur.execute(qry_simDate)
        qry_result = cur.fetchall() 
        out1_value = (qry_result[0][0])
    except:
        out_1_value = 'None'
    this_dbs_outs.append(out1_value)

    cur.execute(qry_histories)
    qry_result = cur.fetchall() 
    out1_value = float(qry_result[0][0])
    this_dbs_outs.append(out1_value)

    cur.execute(qry_totaleros)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_avgRelevantAo)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_totalDepLLRep)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_totalDepLLCond)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_totalDepOthCond)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_CCnt)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_CCt)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_CRil)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_CRih)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_CRdl)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_CRdl_2)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_CRdh)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    cur.execute(qry_CRdh_2)
    qry_result = cur.fetchall() 
    out2_value = float(qry_result[0][0])
    this_dbs_outs.append(out2_value)

    
    # close the cursor and connection
    cur.close()
    conn.close()
        
    results.append(this_dbs_outs)

# Write to the csv file        
try:
    f = open(outfile, 'wb')
    csv.writer(f).writerows(results)
    f.close()
    print 'All done! Your outputs are in', outfile,'.  Have a great day!'
except IOError:
    print 'Your output file appears to be open, so I could not write the outputs.  Please close it and rerun the program.'

