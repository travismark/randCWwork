# This code pulls from an DEMAND Pro access database and pushes to a FAST lcm schema to create a copy of the model data
# April 2015

### Goals / Steps
# define a new sim Id (the next one in lcm.simulation?)
# create new record in lcm.simulation
# create location information: region, location_type, location
# create storage information: storage, storage_type
# create object information: object group, object class, object type, object
# create structure information: structure, structure object (use Jessica's code)
# create events: event type, event
# create distributions: distribution_type, distribution_class, distribution_type_parameter, distribution, distribution_parameter, event_distribution, expression
# create routes: route, route_map, resupply
# create shipment times: shipment_time_distribution
# create probabilities: probability, probability_type (consequences of UER)
# create properties: object_type_property (repair level)
# create future inventory: event_schedule, storage_event_schedule, property, event_property
# create optempos: event_schedule, object_event_schedule, distribution, distribution_parameter, profile, operation_schedule, operation_schedule_profile, profile_distribution
# create cost data: currency_value_class, currency_value_type, currency_value
# create acquisitions and retirements: event_schedules

# Import your stuff
import os
import csv
import datetime
#import time
import pyodbc
import pymysql.cursors

# Define a few variables
sID = 1000 # simulation id
tID = 1000 # tenant id
pID = 1 # project id
cUser = 'user' # default create_user

### Define the database connections
# First the Source database (DEMAND Pro)
db_path = 'P:/Internal Projects/Data Scientist Team/InsightLCM/Testing/FAST/DEMAND Pro Basic Training/BasicCourseModelsDEMAND/PreEx1/'
model = "Model 0-01.mdb"
full_filename = db_path+model
constr = 'Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;'  % full_filename
connSource = pyodbc.connect(constr)
curSource = connSource.cursor()

# Now the Sink databases (FAST)
localport = 3306 # 3307
localuser = 'root'
localpw = 'password' # ''
connSinkInput = pymysql.connect(host='localhost',
							 port=localport,
                             user=localuser,
                             passwd=localpw,
                             db='input',
                             charset='utf8mb4',
                             cursorclass=pymysql.cursors.DictCursor)
cursorINP = connSinkInput.cursor()
connSinkLCM = pymysql.connect(host='localhost',
							 port=localport,
                             user=localuser,
                             passwd=localpw,
                             db='lcm',
                             charset='utf8mb4',
                             cursorclass=pymysql.cursors.DictCursor)
cursorLCM = connSinkLCM.cursor() 


################################################################
################################################################ CONFIRM THREE IDS
### Make sure tenant, project, and simulation are in order 
# Create a tenant if it doesn't exist
sql = "SELECT id FROM `tenant` WHERE `id`=%s"
if cursorLCM.execute(sql, (tID,)) != 1: # will return 1 if it exists, 0 if not
    sqlEnterTenant = '''INSERT INTO `tenant` (`id`, `name`, `create_user`, `create_timestamp`) VALUES (%s, %s, %s, %s)'''
    cursorLCM.execute(sqlEnterTenant, (tID, str('Tenant_'+str(tID)), 'user',datetime.datetime.now().isoformat()))
# otherwise do nothing - we'll use this tenant

#result = cursor.fetchone() # if it doesnt exist, result will be empty, it's of type NoneType

# Create the project if it doesn't exist
sql = "SELECT id FROM `project` WHERE `id`=%s"
if cursorLCM.execute(sql, (pID,)) != 1: # then create it
    sqlEnterProject = '''INSERT INTO `project` (`id`, `tenant_id`, `name`, `project_number`, `hidden`, `create_user`, `create_timestamp`) VALUES (%s, %s, %s, %s, %s, %s, %s)'''
    cursorLCM.execute(sqlEnterProject, (pID, tID, str('Project_'+str(tID)), pID, 0, 'user', datetime.datetime.now().isoformat()))
# otherwise do nothing - we'll use this project


# Assuming: user enters in a simulation id to build (vice just getting the next one)
# Create a new simulation if it doesn't exist
sql = "SELECT id FROM `simulation` WHERE `id`=%s"
cursorLCM.execute(sql, (sID,))
if cursorLCM.rowcount == 0: # then create it, otherwise give an error
    sqlEnterSimulation = '''INSERT INTO `simulation` (`id`, `tenant_id`, `name`, `simulation_type_id`, `output_flag`, `baseline_flag`, `project_id`, `number_of_replications`, `start_date`,
    `interval_unit_id`, `create_user`, `create_timestamp`) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)'''
    cursorLCM.execute(sqlEnterSimulation, (sID, tID, model.split('.')[0], 1, 1, 0, pID, 100, '2015-01-01', 14, 'user', datetime.datetime.now().isoformat()))
else:
    print 'no simulation - many errors coming '# return; # gives an error b/c not in function, deal with later

################################################################
################################################################ DONE CONFIRMING THREE IDS

################################################################ NOW ENTER DATA

###################################### / BEGIN UNIT OF MEASURE
sqlUoM = '''INSERT INTO unit_of_measure (tenant_id, name, interval_unit_id, external_id, create_user, create_timestamp) VALUES (%s, %s, %s, %s, %s, CURRENT_TIMESTAMP)'''
cursorLCM.execute(sqlUoM, (tID, 'Hours', 10, 1, cUser)) # for failures
cursorLCM.execute(sqlUoM, (tID, 'Days', 9, 2, cUser)) # for repairs
print "Done with Unit of Measure"
###################################### / END UNIT OF MEASURE

###################################### / BEGIN EVENT TYPE
sqlET = '''INSERT INTO event_type (tenant_id, simulation_id, name, external_id, create_user, create_timestamp) VALUES (%s, %s, %s, %s, %s, CURRENT_TIMESTAMP)'''
for aName in enumerate(('FAILURE','REPAIR','REPAIR_COMPLETE','PM','PM_COMPLETE','SHIPMENT','RELOCATE','INSTALLATION','INSTALLATION_COMPLETE','REMOVAL','REMOVAL_COMPLETE','ACQUISITION','CONDEMNATION','CONDEMNATION_COMPLETE','RETIREMENT','UDF','ACTIVATE','PASSIVATE','SPARE_REQUEST','SPARE_RESPONSE','REQUEST_CANCELLATION'),start=1): # distributions in alphabetical order
    cursorINP.execute(sqlET, (tID, sID, aName[1], aName[0], cUser))
print "Done with Event Type"
###################################### / END EVENT TYPE
###################################### / BEGIN EVENT
# most event types only have one event, but we'll have to add a few more after looping through
sqlE = '''INSERT INTO event (tenant_id, simulation_id, name, external_id, event_type_id, create_user, create_timestamp) SELECT %s, %s, %s, %s, et.id, %s, CURRENT_TIMESTAMP FROM event_type et WHERE et.tenant_id = %s and et.simulation_id = %s and et.external_id = %s'''
for aName in enumerate(('FAILURE','REPAIR','REPAIR_COMPLETE','PM','PM_COMPLETE','SHIPMENT','RELOCATE','INSTALLATION','INSTALLATION_COMPLETE','REMOVAL','REMOVAL_COMPLETE','ACQUISITION','CONDEMNATION','CONDEMNATION_COMPLETE','RETIREMENT','UDF','ACTIVATE','PASSIVATE','SPARE_REQUEST','SPARE_AVAILABLE','REQUEST_CANCELLATION'),start=1): # distributions in alphabetical order
    cursorINP.execute(sqlE, (tID, sID, aName[1], aName[0], cUser, tID, sID, aName[0]))
# now add a few more
cursorINP.execute(sqlE, (tID, sID, 'SPARE_UNAVAILABLE', 22, cUser, tID, sID, 20))  ### THIS IS BAD - HARDCODED !!! TODO: GET RID OF 
print "Done with Event"
###################################### / END EVENT

# Location info
###################################### / BEGIN LOCATION REGION
# idealy this would come from the command table, but there's no guarentee that'll be filled in.  so just use one region
sqlLR = '''INSERT INTO location_region (tenant_id, simulation_id, name, internal_name, external_id, create_user, create_timestamp) values (%s, %s, %s, %s, %s, %s, %s)''' # just one class right now
cursorINP.execute(sqlLR, (tID, sID, 'Region1', 'Region1', 1, cUser, datetime.datetime.now().isoformat()))
print "Done with Location Region"
###################################### / END LOCATION REGION
###################################### / BEGIN LOCATION TYPE
# Types are hardcoded to the three Maintenance Levels
sqlLT = '''INSERT INTO location_type (tenant_id, simulation_id, name, external_id, create_user, create_timestamp) values (%s, %s, %s, %s, %s, %s)''' # just one class right now
cursorINP.execute(sqlLT, (tID, sID, 'Organizational', 1, cUser, datetime.datetime.now().isoformat()))
cursorINP.execute(sqlLT, (tID, sID, 'Intermediate', 2, cUser, datetime.datetime.now().isoformat()))
cursorINP.execute(sqlLT, (tID, sID, 'Depot', 3, cUser, datetime.datetime.now().isoformat()))
print "Done with Location Type"
###################################### / END LOCATION TYPE
###################################### / BEGIN LOCATION
# get source data
sqlL = '''SELECT [base name] as name, [base name] as intname, left([sran],1) as Mlevel, sran as lEid FROM [*base names]'''
curSource.execute(sqlL)
locations = curSource.fetchall()
# remove spaces from internal name and check for duplictates, then add
locationdict = {}
dupes = 1
# query to paste into source input
sqlL = '''INSERT INTO location (tenant_id, simulation_id, name, internal_name, external_id, location_type_id, location_region_id, create_user, create_timestamp) SELECT %s, %s, %s, %s, %s, lt.id, lr.id, 'user', CURRENT_TIMESTAMP FROM input.location_type lt JOIN input.location_region lr on lt.tenant_id = lr.tenant_id and lt.simulation_id = lr.simulation_id where lt.tenant_id = %s and lt.simulation_id = %s and lt.external_id = %s'''
for row in locations:
    if locationdict.has_key(row.name): # if it's a duplicate, add a number to it
        row.name = row.name + str(dupes)
        row.intname = row.name
        dupes += 1
    locationdict.update({row.name:1})   # add to dictionary after checking for duplicate
    row.intname = row.intname.replace(' ','') # do this check regardless
    cursorINP.execute(sqlL, (tID, sID, row.name, row.intname, row.lEid, tID, sID, row.Mlevel))
print "Done with Location"
###################################### / END LOCATION

# Insert the object information: group, class, type, name
###################################### / BEGIN OBJECT GROUP
# group is hard coded as asset or component
# we don't know the ids, so don't specify 
sql = '''INSERT INTO `object_group` (`tenant_id`, `simulation_id`, `name`, `internal_name`, `external_id`, `create_user`, `create_timestamp`) VALUES (%s, %s, %s, %s, %s, %s, %s)'''
cursorINP.execute(sql, (tID, sID, 'Asset', 'Asset', 1, 'user', datetime.datetime.now().isoformat()))
cursorINP.execute(sql, (tID, sID, 'Component', 'Component', 2, 'user', datetime.datetime.now().isoformat()))
print "Done with Object Group"
###################################### / END OBJECT GROUP

###################################### / BEGIN OBJECT CLASS
# Class comes from the class data table
# create a temp table to store this for now
sqlTemp = '''CREATE TABLE input.tempObjectClass (ten integer, sim integer, ocEid integer, ogEid integer, name varchar(255), intname varchar(255))'''
# drop table sqlDrop = '''DROP TABLE `input`.`tempobjectclass`;'''
cursorINP.execute(sqlTemp)
# insert both assets and components
sqlA = '''SELECT DISTINCT {!s} AS ten, {!s} AS sim, [*Class data].[object class] AS ocEid, 1 as ogEid, [*Class data].Name, [*Class data].Name AS intname FROM [*Class data] INNER JOIN [*Object type] ON [*Class data].[Object Class] = [*Object type].[Object class] WHERE ((([*Object type].[Ind level])=1))'''  ### I CAN'T use replace in the query, so I"ll have to do it later
sqlC = '''SELECT DISTINCT {!s} AS ten, {!s} AS sim, [*Class data].[object class] AS ocEid, 2 as ogEid, [*Class data].Name, [*Class data].Name AS intname FROM [*Class data] INNER JOIN [*Object type] ON [*Class data].[Object Class] = [*Object type].[Object class] WHERE ((([*Object type].[Ind level])<>1))'''  ### I CAN'T use replace in the query, so I"ll have to do it later
sqlA = sqlA.format(tID,sID)
sqlC = sqlC.format(tID,sID) # replace tenant ID and simulation ID

# first insert the assets, then the components
curSource.execute(sqlA) # get the data
classes=curSource.fetchall()
# now convert internal name so it doesn't have spaces
for row in classes:
    row.intname = row.intname.replace(' ','')
# deposit the data - this is a real problem because the data is in pydobc.row format and it needs to be in rows
sqlA = '''INSERT INTO tempObjectClass (`ten`, `sim`, `ocEid`, `ogEid`, `name`, `intname`) VALUES (%s, %s, %s, %s, %s, %s)''' ## pymysql takes %s, wheras pyodbc takes ?
for row in classes:
    cursorINP.execute(sqlA, list(row)) # put the data in the temp table
  # then put the data in the real table
sqlA = '''INSERT INTO `object_class` (`tenant_id`, `simulation_id`, `external_id`, `name`, `internal_name`, `object_group_id`, `create_user`, `create_timestamp`) 
SELECT toc.ten, toc.sim, toc.ocEid, toc.name, toc.intname, og.id, 'user', CURRENT_TIMESTAMP from tempObjectClass toc, object_group og where og.simulation_id = %s and og.external_id = 1''' % sID
cursorINP.execute(sqlA)

## and repeat for components
# clear out the temp table
sqlTemp = '''DELETE FROM input.tempObjectClass'''
cursorINP.execute(sqlTemp) 
# get the pyodbc table
curSource.execute(sqlC) 
classes=curSource.fetchall()
# take out spaces
for row in classes:
    row.intname = row.intname.replace(' ','')
 # put the data in the temp table
sqlC = '''INSERT INTO tempObjectClass (`ten`, `sim`, `ocEid`, `ogEid`, `name`, `intname`) VALUES (%s, %s, %s, %s, %s, %s)''' ## pymysql takes %s, wheras pyodbc takes ?
for row in classes:
    cursorINP.execute(sqlC, list(row))
sqlC = '''INSERT INTO `object_class` (`tenant_id`, `simulation_id`, `external_id`, `name`, `internal_name`, `object_group_id`, `create_user`, `create_timestamp`) 
SELECT toc.ten, toc.sim, toc.ocEid, toc.name, toc.intname, og.id, 'user', CURRENT_TIMESTAMP from tempObjectClass toc, object_group og where og.simulation_id = %s and og.external_id = 2''' % sID
cursorINP.execute(sqlC)

# Object class is done - drop that temp table
sqlTemp = '''DROP TABLE input.tempObjectClass'''
cursorINP.execute(sqlTemp)
print "Done with Object Class"
###################################### / END OBJECT CLASS

###################################### / BEGIN OBJECT TYPE
# query the data, input all the data in the temp table, move all the data over - this type with no temporary table
# object class Insight ID will come from link through temp table to class external id
# get source data
sqlT = '''SELECT name, name as intname, [part number] as pn, [object type] as otEid, [object class] as ocEid FROM [*object type]'''
curSource.execute(sqlT)
types = curSource.fetchall()
# remove spaces from internal name and check for duplictates, then add
typedict = {}
dupes = 1
# query to paste into source input
sqlT = '''INSERT INTO object_type (tenant_id, simulation_id, object_class_id, name, internal_name, part_number, external_id, create_user, create_timestamp) SELECT %s, %s, oc.id, %s, %s, %s, %s, 'user', CURRENT_TIMESTAMP FROM input.object_class oc where oc.tenant_id = %s and oc.simulation_id = %s and oc.external_id = %s'''
for row in types:
    if typedict.has_key(row.name): # if it's a duplicate, add a number to it
        row.name = row.name + str(dupes)
        row.intname = row.name
        dupes += 1
    typedict.update({row.name:1})   # add to dictionary after checking for duplicate
    row.intname = row.intname.replace(' ','') # do this check regardless
    cursorINP.execute(sqlT, (tID, sID, row.name, row.intname, row.pn, row.otEid, tID, sID, row.ocEid))
print "Done with Object Type"
###################################### / END OBJECT TYPE

###################################### / BEGIN OBJECT
# only select the objects that are originally in the simulation (with arrival time = 0)
sqlO = '''SELECT id as oEid, TreeCode as name, [object type] as otEid, sran as lEid, [parent pp] as poEid, iif(mid([object type],4,1)=1,1,0) as asset FROM [*object attributes initial] WHERE [Arrival time ta] = 0'''
curSource.execute(sqlO)
objects = curSource.fetchall()
sqlO = '''INSERT INTO object (tenant_id, simulation_id, object_type_id, location_id, active, available, asset, cloned_flag, template_flag, name, internal_name, serial_number, stg_id, external_id, create_user, create_timestamp) SELECT %s, %s, ot.id, l.id, 1, 1, %s, 0, 0, %s, %s, %s, %s, %s, 'user', CURRENT_TIMESTAMP FROM object_type ot join location l on l.tenant_id = ot.tenant_id and l.simulation_id = ot.simulation_id WHERE ot.tenant_id = %s AND ot.simulation_id = %s and l.external_id = %s and ot.external_id = %s'''
# there should never be spaces in the tree code, so can leave the internal name check out
for row in objects:
    cursorINP.execute(sqlO, (tID, sID, row.asset, row.name, row.name, row.name, row.poEid, row.oEid, tID, sID, row.lEid, row.otEid))
# update the parent objects
sqlO = '''UPDATE object o JOIN object po ON o.stg_id = po.external_id AND o.simulation_id = po.simulation_id SET o.parent_object_id = po.id WHERE o.tenant_id = %s and o.simulation_id = %s''' % (tID, sID)
cursorINP.execute(sqlO)
print "Done with Object"
###################################### / END OBJECT

# Probability - nrts and conseq_uer
###################################### / BEGIN PROBABILITY CLASS
# two of these so far - condemnation and nrts
sqlPC = '''INSERT INTO probability_class (tenant_id, simulation_id, name, sum_of_one_flag, external_id, create_user, create_timestamp) VALUES (%s, %s, %s, %s, %s, %s, CURRENT_TIMESTAMP)'''
cursorINP.execute(sqlPC, (tID, sID, 'Maintenance_Unscheduled', 1, 1, cUser))
cursorINP.execute(sqlPC, (tID, sID, 'Evacuation_Probability', 0, 2, cUser))
print "Done with Probabilty Class"
###################################### / END PROBABILITY CLASS
###################################### / BEGIN PROBABILITY TYPE
# five of these
sqlPT = '''INSERT INTO probability_type (tenant_id, simulation_id, name, external_id, stg_id, create_user, create_timestamp) VALUES (%s, %s, %s, %s, %s, %s, CURRENT_TIMESTAMP)'''
# Maintenance
for aName in enumerate(('No_Action','No_Fault_Found','Condemn','Repair'),start=1): # distributions in alphabetical order
    cursorINP.execute(sqlPT, (tID, sID, aName[1], aName[0], 1, cUser))
# NRTS
cursorINP.execute(sqlPT, (tID, sID, 'Evacuation_Probability', 5, 2, cUser))
print "Done with Probability Type"
###################################### / END PROBABILITY TYPE
###################################### / BEGIN Probability  ----- TODO: NEED TO HANDLE DEFAULTS TO CLASS !!!!!!!!!!! #################### - not working yet
# # first do conseq_uer
# sqlP = '''SELECT [*Consequences of UER].[Object type] AS otype, [*Consequences of UER].SRAN, [*Consequences of UER].[Pu s,non] AS non, [*Consequences of UER].[Pu s,nff] AS nff, [*Consequences of UER].[Pu s,con] AS con, [*Consequences of UER].[Pu s,rep] AS rep, "type" AS spec
# FROM [*Consequences of UER] WHERE (((Right([object type],1))<>0)) UNION
# SELECT [*Consequences of UER].[Object type] AS otype, [*Consequences of UER].SRAN, [*Consequences of UER].[Pu s,non] AS non, [*Consequences of UER].[Pu s,nff] AS nff, [*Consequences of UER].[Pu s,con] AS con, [*Consequences of UER].[Pu s,rep] AS rep, "class" AS spec
# FROM [*Consequences of UER] WHERE (((Right([object type],1))=0))'''
# curSource.execute(sqlP)
# objects = curSource.fetchall()
# sqlP = '''INSERT INTO object (tenant_id, simulation_id, object_type_id, location_id, active, available, asset, cloned_flag, template_flag, name, internal_name, serial_number, stg_id, external_id, create_user, create_timestamp) SELECT %s, %s, ot.id, l.id, 1, 1, %s, 0, 0, %s, %s, %s, %s, %s, 'user', CURRENT_TIMESTAMP FROM object_type ot join location l on l.tenant_id = ot.tenant_id and l.simulation_id = ot.simulation_id WHERE ot.tenant_id = %s AND ot.simulation_id = %s and l.external_id = %s and ot.external_id = %s'''
# # there should never be spaces in the tree code, so can leave the internal name check out
# for row in objects:
#     cursorINP.execute(sqlP, (tID, sID, row.asset, row.name, row.name, row.name, row.poEid, row.oEid, tID, sID, row.lEid, row.otEid))
# # update the parent objects
# sqlP = '''UPDATE object o JOIN object po ON o.stg_id = po.external_id AND o.simulation_id = po.simulation_id SET o.parent_object_id = po.id WHERE o.tenant_id = %s and o.simulation_id = %s''' % (tID, sID)
# cursorINP.execute(sqlP)
# print "Done with Probability"
###################################### / END Probability

# Insert Distribution information
# these are standard for each simulation
###################################### / BEGIN DISTRIBUTION TYPE
sqlDT = '''INSERT INTO distribution_type (tenant_id, simulation_id, external_id, name, create_user, create_timestamp) values (%s, %s, %s, %s, %s, %s)''' # standard hard-coded types
for aName in enumerate(('Constant','Exponential','Normal','Uniform','Weibull'),start=1): # distributions in alphabetical order
    cursorINP.execute(sqlDT, (tID, sID, aName[0], aName[1],'user', datetime.datetime.now().isoformat()))
print "Done with Distribution Type"
###################################### / END DISTRIBUTION TYPE
###################################### / BEGIN DISTRIBUTION CLASS
sqlDC = '''INSERT INTO distribution_class (tenant_id, simulation_id, name, create_user, create_timestamp) values (%s, %s, %s, %s, %s)''' # just one class right now
cursorINP.execute(sqlDC, (tID, sID, 'Distribution','user', datetime.datetime.now().isoformat()))
print "Done with Distribution Class"
###################################### / END DISTRIBUTION CLASS
###################################### / BEGIN DISTRIBUTION TYPE PARAMETER
# first the first parameter
sqlDTP = '''insert into input.distribution_type_parameter (tenant_id, simulation_id, distribution_type_id, name, parameter_number, create_user, create_timestamp) SELECT tenant_id, simulation_id, id, %s, %s, 'user', current_timestamp() FROM input.distribution_type dt where dt.simulation_id = %s and dt.tenant_id = %s and dt.external_id = %s'''
for aName in enumerate(('Fixed','Mean','Mean','Lower','Shape'),start=1): # distributions in alphabetical order
    cursorINP.execute(sqlDTP, (aName[1], 1 ,tID, sID, aName[0]))
# enter second parameters manually
cursorINP.execute(sqlDTP, ('Standard Deviation', 2 ,tID, sID, 3))
cursorINP.execute(sqlDTP, ('Lower', 2 ,tID, sID, 4))
cursorINP.execute(sqlDTP, ('Scale', 2 ,tID, sID, 5))
print "Done with Distribution Type Parameter"
###################################### / END DISTRIBUTION TYPE PARAMETER
###################################### / BEGIN DISTRIBUTION PARAMETER
# failure distributions first - DEMAND Pro always uses weibull failure rates
weibullExID = 5
sqlFR = '''SELECT [*Unscheduled Removal rates].[LRU  type] AS etId, [*Unscheduled Removal rates].[Platform type] AS PetId, [*Unscheduled Removal rates].Base as lEid, [*Unscheduled Removal rates].[Age Unit] AS ageType, [*Unscheduled Removal rates].Rate AS scale, [*Unscheduled Removal rates].Shape, [*Unscheduled Removal rates].[Completed Repairs] as cr FROM [*Unscheduled Removal rates]'''
curSource.execute(sqlFR)
failureRates = curSource.fetchall()
# lcm.unit_of_measure is in a different database, so I'll have to query it separately
sqlUOM = 'SELECT id from unit_of_measure where external_id = 1 and tenant_id = %s' % tID 
cursorLCM.execute(sqlUOM)
failureUOM = cursorLCM.fetchone() # this gives me a dictionary
failureUOM = failureUOM.get(u'id') # get the value
exID = 1 # use as counter for inputting parameters later
sqlFR = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) SELECT %s, %s, %s, %s, dc.id, dt.id, %s, %s, CURRENT_TIMESTAMP FROM distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = %s and dc.simulation_id = %s and dt.external_id = %s'''
for row in failureRates:
    tempName = 'Failure exTID: ' + str(row.etId) + ' exPID' + str(row.PetId) + ' exLID: ' + str(row.lEid) + ' CR: ' + str(row.cr) # concat a name, can include spaces
    cursorINP.execute(sqlFR, (tID, sID, exID, tempName, failureUOM, cUser, tID, sID, weibullExID))
    exID += 1
# now repair distributions ############# TBD TBD TBD ##########################
#################################
# REPAR DISTRIBUTION SOURCE QUERY ISN'T WORKING NOW ##################################3
#################################

cursorLCM.execute('SELECT id from lcm.unit_of_measure where external_id = 2 and tenant_id = %s' % tID ) # hard coded
repairUOM = cursorLCM.fetchone()
repairUOM = repairUOM.get(u'id')
constantExID = 1

# no external ID
dexID = 100
otExternalID = 100201001 # engine
hardcodeRepairDist = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) SELECT %s, %s, %s, %s, dc.id, dt.id, %s, %s, CURRENT_TIMESTAMP FROM distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = %s and dc.simulation_id = %s and dt.external_id = %s'''
tempName = 'engineRepair'
cursorINP.execute(hardcodeRepairDist, (tID, sID, dexID, tempName, repairUOM, cUser, tID, sID, constantExID))
hardcodeRepairParam = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT %s, %s, %s, %s, d.id, dtp.id, %s, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = %s AND d.simulation_id = %s AND d.external_id = %s AND dtp.parameter_number = %s'''
cursorINP.execute(hardcodeRepairParam, (tID, sID, dexID, 60, cUser, tID, sID, dexID, 1))
hardcodeRepairEventDist = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, create_user, create_timestamp) SELECT %s, %s, %s, d.name, d.id, e.id, ot.id, %s, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id WHERE e.tenant_id = %s and e.simulation_id = %s AND d.external_id = %s and ot.external_id = %s and e.name like "Repair"''' # can also include location and parent object type for failure rates, but haven't yet
cursorINP.execute(hardcodeRepairEventDist, (tID, sID, dexID, cUser, tID, sID, dexID, otExternalID ))



# right now I'm hardcoding to just get repair at any level 10000 because I'm short on time TODO: make repair distributions more general
#sqlRep = '''SELECT sr.[SRAN ID] as lEid, sr.[SERVER TYPE] as mntType, sr.[Object type] as otEid, sr.[Tsf Dist] AS distType, sr.[Tsf P1] AS p1, sr.[Tsf P2] AS p2 FROM [*Server times] as sr WHERE sr.[SRAN ID]=10000 AND sr.[SERVER TYPE]="Repair"'''
#curSource.execute(sqlRep)
#repairDists = curSource.fetchall()
# lcm.unit_of_measure is in a different database, so I'll have to query it separately
#cursorLCM('SELECT id from lcm.unit_of_measure where external_id = 2 and tenant_id = %s' % tID )
#repairUOM = cursorLCM.fetchone()
#repairUOM = repairUOM.get(u'id')
#exID = 1
#sqlRep = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) SELECT %s, %s, %s, %s, dc.id, dt.id, %s, %s, CURRENT_TIMESTAMP FROM distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = %s and dc.simulation_id = %s and dt.external_id = %s'''
#print "Done with Distribution"
###################################### / END DISTRIBUTION PARAMETER
###################################### / BEGIN DISTRIBUTION PARAMETER
# we already have the parameters from inserting the distributions
sqlFR = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT %s, %s, %s, %s, d.id, dtp.id, %s, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = %s AND d.simulation_id = %s AND d.external_id = %s AND dtp.parameter_number = %s'''
dexID = 1 # for the distribution number (row number)
exID = 1 # for the parameter number (row number x 2-ish)
for row in failureRates:
    # first shape
    cursorINP.execute(sqlFR, (tID, sID, exID, row.Shape, cUser, tID, sID, dexID, 1))
    exID += 1
    # now scale
    cursorINP.execute(sqlFR, (tID, sID, exID, row.scale, cUser, tID, sID, dexID, 2))
    exID += 1
    dexID += 1
######################### STILL ######################### MUST ######################### DO ######################### REPAIRS #########################
######################### STILL ######################### MUST ######################### DO ######################### REPAIRS #########################
######################### STILL ######################### MUST ######################### DO ######################### REPAIRS #########################
######################### STILL ######################### MUST ######################### DO ######################### REPAIRS #########################


###################################### / END DISTRIBUTION PARAMETER
###################################### / BEGIN EVENT DISTRIBUTION
sqlED = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, create_user, create_timestamp) SELECT %s, %s, %s, d.name, d.id, e.id, ot.id, %s, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id WHERE e.tenant_id = %s and e.simulation_id = %s AND d.external_id = %s and ot.external_id = %s and e.name like "Failure"''' # can also include location and parent object type for failure rates, but haven't yet
# use that same failure rate pyodbc list
exID = 1 # for external event_distribution id
for row in failureRates:
    cursorINP.execute(sqlED, (tID, sID, exID, cUser, tID, sID, exID, row.etId ))
    exID += 1

######################### STILL ######################### MUST ######################### DO ######################### REPAIRS #########################
######################### STILL ######################### MUST ######################### DO ######################### REPAIRS #########################
######################### STILL ######################### MUST ######################### DO ######################### REPAIRS #########################
######################### STILL ######################### MUST ######################### DO ######################### REPAIRS #########################

###################################### / END EVENT DISTRIBUTION
# FROM JESSICA W
# parent structures (objects with at least one child)
sqlPS ='''insert into input.structure 
        ( name
        , internal_name
        , minimum_count
        , object_id
        , tenant_id
        , simulation_id
        , create_user
        , create_timestamp
        )
    select concat(obj.name,' Parent')
        , concat(obj.internal_name,'Parent')
        , 0
        , obj.id
        , %s
        , %s
        , '%s'
        , CURRENT_TIMESTAMP
    from input.object obj
    where obj.id in (
        select distinct(parent_object_id) from input.object
        where tenant_id=%s and simulation_id=%s
        and parent_object_id is not null
        )
;''' % (tID, sID, cUser, tID, sID)
cursorINP.execute(sqlPS)

# min_count
sqlMC='''update input.structure s
    join (
        select str.id
            , count(obj.id) n 
        from input.structure str 
        join input.object obj 
        on str.object_id=obj.parent_object_id 
        where str.tenant_id=%s and str.simulation_id=%s
        group by str.object_id
        ) num
    on s.id=num.id
    set s.minimum_count = num.n
    where s.tenant_id=%s and s.simulation_id=%s
;''' % (tID, sID, tID, sID)
cursorINP.execute(sqlMC)

#child structures
sqlCS='''insert into input.structure
        ( name
        , internal_name
        , minimum_count
        , parent_structure_id
        , tenant_id
        , simulation_id
        , create_user
        , create_timestamp
        )
    select concat(obj.name,' Child')
        , concat(obj.internal_name,'Child')
        , 1
        , str.id
        , %s
        , %s
        , '%s'
        , CURRENT_TIMESTAMP
    from input.object obj
    join input.structure str
    on str.object_id = obj.parent_object_id
    where str.tenant_id=%s and str.simulation_id=%s
;''' % (tID, sID, cUser, tID, sID)
cursorINP.execute(sqlCS)

#structure_object - connect children objects to their children (not top-level objects, e.g. assets)
# the structures of interest are child structures whose names end with ' Child'
sqlSO='''insert into input.structure_object
        ( object_id
        , structure_id
        , tenant_id
        , simulation_id
        , create_user
        , create_timestamp
        )
    select o.id
        , s.id
        , %s
        , %s
        , '%s'
        , CURRENT_TIMESTAMP 
    from input.object o 
    join input.structure s 
    on left(s.name,length(s.name)-6)=o.name 
    where s.object_id is null 
    and o.parent_object_id is not null 
    and right(s.name,5) = 'Child'
    and s.tenant_id=%s and s.simulation_id=%s;''' % (tID, sID, cUser, tID, sID)
cursorINP.execute(sqlSO)
###################################### / BEGIN STRUCTURE


###################################### / END STRUCTURE
###################################### / BEGIN STRUCTURE OBJECT


###################################### / END STRUCTURE OBJECT











#     # connection is not autocommit by default. So you must commit to save
#     # your changes.
#     connSinkLCM.commit()

#     with connSinkLCM.cursor() as cursor:
#         # Read a single record
#         sql = "SELECT `id`, `name` FROM `simulation` WHERE `id`=%s"
#         cursor.execute(sql, ('1',))
#         result = cursor.fetchone()
#         print(result)
# finally:
#     connSinkLCM.close()

# # simple example - enter in a new simulation into lcm
# try:
#     with connSinkLCM.cursor() as cursor:
#         # Create a new record
#         sql = '''INSERT INTO `simulation` (`id`, `tenant_id`, `name`, `simulation_type_id`, `output_flag`, `project_id`, `number_of_replications`, `start_date`,
#             `interval_unit_id`, `create_user`, `create_timestamp`) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)'''
#         cursor.execute(sql, (1, 4, 'testSim', 1, 0, 1, 100, '2015-01-01', 14, 'user',datetime.datetime.now().isoformat()))

#     # connection is not autocommit by default. So you must commit to save
#     # your changes.
#     connSinkLCM.commit()

#     with connSinkLCM.cursor() as cursor:
#         # Read a single record
#         sql = "SELECT `id`, `name` FROM `simulation` WHERE `id`=%s"
#         cursor.execute(sql, ('1',))
#         result = cursor.fetchone()
#         print(result)
# except IOError:

# finally:
#     connSinkLCM.close()

