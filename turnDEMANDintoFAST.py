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
import math
import timeit # debug
import datetime
import pyodbc
#import pymysql.cursors

# Define a few variables
sID = 1001 # simulation id
tID = 1000 # tenant id
pID = 2 # project id
cUser = 'user' # default create_user
InsightOrDemand = 2 # insight, 1 (with failure rates defined in calendar time) or demo, 2 (with failure rates defined in operating hours)
LocalOrWeb = 1 # local, 1 (localhost) or web, 2 (out on insight)
numberOfReps = 100

### Define the database connections
# First the Source database (DEMAND Pro)
db_path = 'P:/Internal Projects/Data Scientist Team/InsightLCM/Testing/FAST/DEMAND Pro Basic Training/BasicCourseModelsDEMAND/PreEx1/'
model = "Model 0-01.mdb"
db_path = "C:/Users/tbaer/Desktop/demo/"
model = "1-Baseline_p70bp.mdb"
full_filename = db_path+model
constr = 'Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=%s;'  % full_filename
connSource = pyodbc.connect(constr)
curSource = connSource.cursor()

# Now the Sink databases (FAST)
# local or web
if LocalOrWeb == 1:
    aServer = 'localhost'
    aPort = 3306
    aUser = 'root'
    aPW = 'password'
else:
    aServer = '10.100.1.229'  ### SET THIS TO YOUR FAVORITE WEB DB
    aPort = 3306
    aUser = 'csiuser'
    aPW = 'LCMW!llMakeS0me'

constr = 'DRIVER={MySQL ODBC 5.3 Unicode Driver};SERVER=%s;PORT=%s;DATABASE=input;user=%s;Password=%s' % (aServer,aPort,aUser,aPW)
connSinkInput = pyodbc.connect(constr)
cursorINP = connSinkInput.cursor()

constr = 'DRIVER={MySQL ODBC 5.3 Unicode Driver};SERVER=%s;PORT=%s;DATABASE=lcm;user=%s;Password=%s' % (aServer,aPort,aUser,aPW)
connSinkLCM = pyodbc.connect(constr)
cursorLCM = connSinkLCM.cursor() 

constr = 'DRIVER={MySQL ODBC 5.3 Unicode Driver};SERVER=%s;PORT=%s;DATABASE=output;user=%s;Password=%s' % (aServer,aPort,aUser,aPW)
connSinkOut = pyodbc.connect(constr)
cursorOUT = connSinkOut.cursor() 


################################################################
################################################################ CONFIRM THREE IDS
### Make sure tenant, project, and simulation are in order 
# Create a tenant if it doesn't exist
sqlCheck = "SELECT id FROM `tenant` WHERE `id`=?"
sqlMax = "SELECT max(id) FROM `tenant`"
if cursorLCM.execute(sqlCheck, (tID,)) != 1: # will return 1 if it exists, 0 if not
    sqlEnterTenant = '''INSERT INTO `tenant` (`id`, `name`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?)'''
    cursorLCM.execute(sqlEnterTenant, (tID, str('Tenant_'+str(tID)), 'user', datetime.datetime.now().isoformat()))
else:
    cursorLCM.execute(sqlMax)
    maxID = cursorLCM.fetchone() + 1
# otherwise do nothing - we'll use this tenant

#result = cursor.fetchone() # if it doesnt exist, result will be empty, it's of type NoneType

# Create the project if it doesn't exist
sql = "SELECT id FROM `project` WHERE `id`=?"
if cursorLCM.execute(sql, (pID,)) != 1: # then create it
    sqlEnterProject = '''INSERT INTO `project` (`id`, `tenant_id`, `name`, `project_number`, `hidden`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?, ?, ?, ?)'''
    cursorLCM.execute(sqlEnterProject, (pID, tID, str('Project_'+str(tID)), pID, 0, 'user', datetime.datetime.now().isoformat()))
# otherwise do nothing - we'll use this project

# get analysis range from DEMAND 
sqlDates = '''SELECT [*Weeks and Dates].Year, [*Weeks and Dates].Quarter, [*Weeks and Dates].Date, [*Analysis Range].[Amount of quarters] as NumQ
FROM [*Analysis Range] INNER JOIN [*Weeks and Dates] ON ([*Analysis Range].[First year (>= 1999)] = [*Weeks and Dates].Year) AND ([*Analysis Range].[First quarter (1 - 4)] = [*Weeks and Dates].Quarter)
WHERE ((([*Weeks and Dates].Week)=1))'''
curSource.execute(sqlDates)
dateInfo = curSource.fetchone()
sqlMSperQtr = '''SELECT milliseconds FROM lcm.interval_unit WHERE name LIKE "Quarter"'''
cursorLCM.execute(sqlMSperQtr)
milsPerQ = cursorLCM.fetchone()[0]
totalSimMils = milsPerQ * dateInfo.NumQ

# Assuming: user enters in a simulation id to build (vice just getting the next one)
# Create a new simulation if it doesn't exist
simTypeID = 1 if InsightOrDemand == 1 else 2 # set simulation_type_id to 1 (Insight) or 2 (DEMAND) 
sql = "SELECT id FROM `simulation` WHERE `id`=?"
cursorLCM.execute(sql, (sID,))
if cursorLCM.rowcount == 0: # then create it, otherwise give an error
    sqlEnterSimulation = '''INSERT INTO `simulation` (`id`, `tenant_id`, `name`, `simulation_type_id`, `output_flag`, `baseline_flag`, `project_id`, `number_of_replications`, `start_date`, `end_time`,
    `interval_unit_id`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'''
    cursorLCM.execute(sqlEnterSimulation, (sID, tID, model.split('.')[0], simTypeID, True, 0, pID, numberOfReps, dateInfo.Date.isoformat(), totalSimMils, 14, 'user', datetime.datetime.now().isoformat()))
else:
    print 'no simulation - many errors coming '# return; # gives an error b/c not in function, deal with later

################################################################
################################################################ DONE CONFIRMING THREE IDS

################################################################ NOW ENTER DATA

###################################### / BEGIN UNIT OF MEASURE
sqlUoM = '''INSERT INTO unit_of_measure (tenant_id, name, interval_unit_id, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
sqlUoMnoIU = '''INSERT INTO unit_of_measure (tenant_id, name, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)'''
cursorLCM.execute(sqlUoM, (tID, 'Hours', 10, 1, cUser)) # for failures
cursorLCM.execute(sqlUoM, (tID, 'Days', 9, 2, cUser)) # for repairs
cursorLCM.execute(sqlUoM, (tID, 'Operating Hours', 10, 3, cUser)) # for operating hours (viewing failures in scenario editor)
cursorLCM.execute(sqlUoMnoIU, (tID, 'Each', 4, cUser)) # for Future Inventory
cursorLCM.execute(sqlUoMnoIU, (tID, 'Integer', 5, cUser)) # for Future Inventory

print "Done with Unit of Measure"
###################################### / END UNIT OF MEASURE

# DONE WITH THE LCM STUFF - COMMIT This
connSinkLCM.commit()
####


###################################### / BEGIN EVENT TYPE
sqlET = '''INSERT INTO event_type (tenant_id, simulation_id, name, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
for aName in enumerate(('Failure','Repair','Repair Complete','PM','PM Complete','Shipment','Relocate','Installation','Installation Complete','Removal','Removal Complete','Condemnation','Condemnation Complete','User Defined Function','Retirement','Activate','Passivate','Spare Request','Spare Response','Request Cancellation','Acquisition'),start=1): # 21 event types
    cursorINP.execute(sqlET, (tID, sID, aName[1], aName[0], cUser))
print "Done with Event Type"
###################################### / END EVENT TYPE
###################################### / BEGIN EVENT
# most event types only have one event, but we'll have to add a few more after looping through
sqlE = '''INSERT INTO event (tenant_id, simulation_id, name, external_id, event_type_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, et.id, ?, CURRENT_TIMESTAMP FROM event_type et WHERE et.tenant_id = ? and et.simulation_id = ? and et.external_id = ?'''
for aName in enumerate(('Failure','Repair','Repair Complete','PM','PM Complete','Shipment','Relocate','Installation','Installation Complete','Removal','Removal Complete','Condemnation','Condemnation Complete','User Defined Function','Retiremnet','Activate','Passivate','Spare Request','Spare Available','Request Cancellation','Acquisition'),start=1): # 20 events here
    cursorINP.execute(sqlE, (tID, sID, aName[1], aName[0], cUser, tID, sID, aName[0]))
# now add a few more
cursorINP.execute(sqlE, (tID, sID, 'SPARE UNAVAILABLE', 22, cUser, tID, sID, 19))  ### THIS IS BAD - HARDCODED !!! TODO: GET RID OF 
print "Done with Event"
###################################### / END EVENT

# Location info
###################################### / BEGIN LOCATION REGION
# idealy this would come from the command table, but there's no guarentee that'll be filled in.  so just use one region
sqlLR = '''INSERT INTO location_region (tenant_id, simulation_id, name, internal_name, external_id, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?, ?)''' # just one class right now
cursorINP.execute(sqlLR, (tID, sID, 'Region1', 'Region1', 1, cUser, datetime.datetime.now().isoformat()))
print "Done with Location Region"
###################################### / END LOCATION REGION
###################################### / BEGIN LOCATION TYPE
# Types are hardcoded to the three Maintenance Levels
sqlLT = '''INSERT INTO location_type (tenant_id, simulation_id, name, external_id, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?)''' # just one class right now
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
sqlL = '''INSERT INTO location (tenant_id, simulation_id, name, internal_name, external_id, location_type_id, location_region_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, ?, lt.id, lr.id, 'user', CURRENT_TIMESTAMP FROM input.location_type lt JOIN input.location_region lr on lt.tenant_id = lr.tenant_id and lt.simulation_id = lr.simulation_id where lt.tenant_id = ? and lt.simulation_id = ? and lt.external_id = ?'''
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

# Insert the object information: state_type, group, class, type, name
###################################### / BEGIN STATE TYPE
# HARD CODED
sql = '''INSERT INTO `object_state_type` (`tenant_id`, `simulation_id`, `name`, `internal_name`, `external_id`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?, ?, ?, ?)'''
cursorINP.execute(sql, (tID, sID, 'Serviceable', 'Serviceable', 1, cUser, datetime.datetime.now().isoformat()))
cursorINP.execute(sql, (tID, sID, 'Unserviceable', 'Unserviceable', 2, cUser, datetime.datetime.now().isoformat()))
print "Done with Object State Type"
###################################### / END OBJECT STATE TYPE
###################################### / BEGIN OBJECT GROUP
# group is hard coded as asset or component
# we don't know the ids, so don't specify 
sql = '''INSERT INTO `object_group` (`tenant_id`, `simulation_id`, `name`, `internal_name`, `external_id`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?, ?, ?, ?)'''
cursorINP.execute(sql, (tID, sID, 'Asset', 'Asset', 1, cUser, datetime.datetime.now().isoformat()))
cursorINP.execute(sql, (tID, sID, 'Component', 'Component', 2, cUser, datetime.datetime.now().isoformat()))
print "Done with Object Group"
###################################### / END OBJECT GROUP

###################################### / BEGIN OBJECT CLASS
# Class comes from the class data table
# insert both assets and components
sqlA = '''SELECT DISTINCT %s AS ten, %s AS sim, [*Class data].[object class] AS ocEid, 1 as ogEid, [*Class data].Name, [*Class data].Name AS intname FROM [*Class data] INNER JOIN [*Object type] ON [*Class data].[Object Class] = [*Object type].[Object class] WHERE ((([*Object type].[Ind level])=1))''' % (tID, sID) 
sqlC = '''SELECT DISTINCT %s AS ten, %s AS sim, [*Class data].[object class] AS ocEid, 2 as ogEid, [*Class data].Name, [*Class data].Name AS intname FROM [*Class data] INNER JOIN [*Object type] ON [*Class data].[Object Class] = [*Object type].[Object class] WHERE ((([*Object type].[Ind level])<>1))'''  % (tID, sID)
# first insert the assets, then the components
curSource.execute(sqlA) # get the data
classes=curSource.fetchall()
sqlAput = '''INSERT INTO object_class (tenant_id, simulation_id, external_id, object_group_id, name, internal_name, create_user, create_timestamp) SELECT ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP''' ## pymysql takes %s, wheras pyodbc takes ?
# now loop through data
for row in classes:
    # convert internal name so it doesn't have spaces
    row.intname = row.intname.replace(' ','')
    # deposit the data
    cursorINP.execute(sqlAput, (tID,sID,row.ocEid, row.ogEid, row.Name, row.intname, cUser))
## and repeat for components
# get the pyodbc table
curSource.execute(sqlC) 
classes=curSource.fetchall()
sqlCput = '''INSERT INTO object_class (tenant_id, simulation_id, external_id, object_group_id, name, internal_name, create_user, create_timestamp) SELECT ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP''' ## pymysql takes %s, wheras pyodbc takes ?
# take out spaces
for row in classes:
    row.intname = row.intname.replace(' ','')
    cursorINP.execute(sqlCput, (tID,sID,row.ocEid, row.ogEid, row.Name, row.intname, cUser))

# for row in classes:
#     cursorINP.execute(sqlC, list(row))
# sqlC = '''INSERT INTO `object_class` (`tenant_id`, `simulation_id`, `external_id`, `name`, `internal_name`, `object_group_id`, `create_user`, `create_timestamp`) 
# SELECT toc.ten, toc.sim, toc.ocEid, toc.name, toc.intname, og.id, 'user', CURRENT_TIMESTAMP from tempObjectClass toc, object_group og where og.simulation_id = %s and og.external_id = 2''' % sID
# cursorINP.execute(sqlC)

# Object class is done - drop that temp table
# sqlTemp = '''DROP TABLE input.tempObjectClass'''
# cursorINP.execute(sqlTemp)
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
sqlT = '''INSERT INTO object_type (tenant_id, simulation_id, object_class_id, name, internal_name, part_number, external_id, create_user, create_timestamp) SELECT ?, ?, oc.id, ?, ?, ?, ?, 'user', CURRENT_TIMESTAMP FROM input.object_class oc where oc.tenant_id = ? and oc.simulation_id = ? and oc.external_id = ?'''
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
sqlO = '''INSERT INTO object (tenant_id, simulation_id, object_type_id, location_id, active, available, asset, cloned_flag, template_flag, name, internal_name, serial_number, stg_id, external_id, create_user, create_timestamp) SELECT ?, ?, ot.id, l.id, 1, 1, ?, 0, 0, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP FROM object_type ot join location l on l.tenant_id = ot.tenant_id and l.simulation_id = ot.simulation_id WHERE ot.tenant_id = ? AND ot.simulation_id = ? and l.external_id = ? and ot.external_id = ?'''
# there should never be spaces in the tree code, so can leave the internal name check out
for row in objects:
    cursorINP.execute(sqlO, (tID, sID, row.asset, row.name, row.name, row.name, row.poEid, row.oEid, cUser, tID, sID, row.lEid, row.otEid))
# update the parent objects
sqlO = '''UPDATE object o JOIN object po ON o.stg_id = po.external_id AND o.simulation_id = po.simulation_id SET o.parent_object_id = po.id WHERE o.tenant_id = %s and o.simulation_id = %s''' % (tID, sID)
cursorINP.execute(sqlO)
print "Done with Object"
###################################### / END OBJECT

# Probability - nrts and conseq_uer
###################################### / BEGIN PROBABILITY CLASS
# two of these so far - condemnation and nrts
sqlPC = '''INSERT INTO probability_class (tenant_id, simulation_id, name, sum_of_one_flag, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
cursorINP.execute(sqlPC, (tID, sID, 'Maintenance_Unscheduled', True, 1, cUser))
cursorINP.execute(sqlPC, (tID, sID, 'Evacuation_Probability', False, 2, cUser))
print "Done with Probabilty Class"
###################################### / END PROBABILITY CLASS
###################################### / BEGIN PROBABILITY TYPE
# five of these
sqlPT = '''INSERT INTO probability_type (tenant_id, simulation_id, name, external_id, stg_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
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

#### TODO: THROW an error if it sees a Lognormal distribution
###################################### / BEGIN DISTRIBUTION TYPE
sqlDT = '''INSERT INTO distribution_type (tenant_id, simulation_id, external_id, name, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?)''' # standard hard-coded types
for aName in enumerate(('Constant','Exponential','Normal','Uniform','Weibull'),start=1): # distributions in alphabetical order
    cursorINP.execute(sqlDT, (tID, sID, aName[0], aName[1],'user', datetime.datetime.now().isoformat()))
print "Done with Distribution Type"
###################################### / END DISTRIBUTION TYPE
###################################### / BEGIN DISTRIBUTION CLASS
sqlDC = '''INSERT INTO distribution_class (tenant_id, simulation_id, name, create_user, create_timestamp) values (?, ?, ?, ?, ?)''' # just one class right now
cursorINP.execute(sqlDC, (tID, sID, 'Distribution','user', datetime.datetime.now().isoformat()))
print "Done with Distribution Class"
###################################### / END DISTRIBUTION CLASS
###################################### / BEGIN DISTRIBUTION TYPE PARAMETER
# first the first parameter
sqlDTP = '''insert into input.distribution_type_parameter (tenant_id, simulation_id, distribution_type_id, name, parameter_number, create_user, create_timestamp) SELECT tenant_id, simulation_id, id, ?, ?, ?, current_timestamp() FROM input.distribution_type dt where dt.tenant_id = ? and dt.simulation_id = ? and dt.external_id = ?'''
for aName in enumerate(('Fixed','Mean','Mean','Lower','Scale'),start=1): # distributions in alphabetical order
    cursorINP.execute(sqlDTP, (aName[1], 1, cUser, tID, sID, aName[0]))
# enter second parameters manually
cursorINP.execute(sqlDTP, ('Standard Deviation', 2 , cUser, tID, sID, 3))
cursorINP.execute(sqlDTP, ('Upper', 2 , cUser, tID, sID, 4))
cursorINP.execute(sqlDTP, ('Shape', 2 , cUser, tID, sID, 5))
print "Done with Distribution Type Parameter"
###################################### / END DISTRIBUTION TYPE PARAMETER
###################################### / BEGIN DISTRIBUTION PARAMETER
# failure distributions first - DEMAND Pro always uses weibull failure rates
weibullExID = 5
sqlFR = '''SELECT [*Unscheduled Removal rates].[LRU  type] AS otEid, [*Unscheduled Removal rates].[Platform type] AS PetId, [*Unscheduled Removal rates].Base as lEid, [*Unscheduled Removal rates].[Age Unit] AS ageType, [*Unscheduled Removal rates].Rate AS scale, [*Unscheduled Removal rates].Shape as shape, [*Unscheduled Removal rates].[Completed Repairs] as cr FROM [*Unscheduled Removal rates]'''
curSource.execute(sqlFR)
failureRates = curSource.fetchall()
# lcm.unit_of_measure is in a different database, so I'll have to query it separately
sqlUOM = 'SELECT id from unit_of_measure where external_id = 1 and tenant_id = %s' % tID 
cursorLCM.execute(sqlUOM)
failureUOM = cursorLCM.fetchone() # this gives me a dictionary (can use string.encode to convert from bytestring to unicodestring)
failureUOM = failureUOM.id # get the value
exID = 1 # use as counter for inputting parameters later
sqlFR = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, dc.id, dt.id, ?, ?, CURRENT_TIMESTAMP FROM distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = ? and dc.simulation_id = ? and dt.external_id = ?'''
for row in failureRates:
    tempName = 'Failure exTID: ' + str(row.otEid) + ' exPID' + str(row.PetId) + ' exLID: ' + str(row.lEid) + ' CR: ' + str(row.cr) # concat a name, can include spaces
    cursorINP.execute(sqlFR, (tID, sID, exID, tempName, failureUOM, cUser, tID, sID, weibullExID))
    exID += 1
# now repair distributions ############# TBD TBD TBD ##########################
#################################
# REPAR DISTRIBUTION SOURCE QUERY ISN'T WORKING NOW ##################################
#################################

# lcm.unit_of_measure is in a different database, so I'll have to query it separately
cursorLCM.execute('SELECT id from lcm.unit_of_measure where external_id = 2 and tenant_id = %s' % tID ) # hard coded
repairUOM = cursorLCM.fetchone()
repairUOM = repairUOM.id
constantExID = 1

# no external ID - when hardcoded - TODO - still add distribution_parameter and event_distribution for repair events
# dexID = 100
# otExternalID = 100201001 # engine
# hardcodeRepairDist = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) SELECT %s, %s, %s, %s, dc.id, dt.id, %s, %s, CURRENT_TIMESTAMP FROM distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = %s and dc.simulation_id = %s and dt.external_id = %s'''
# tempName = 'engineRepair'
# cursorINP.execute(hardcodeRepairDist, (tID, sID, dexID, tempName, repairUOM, cUser, tID, sID, constantExID))
# hardcodeRepairParam = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT %s, %s, %s, %s, d.id, dtp.id, %s, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = %s AND d.simulation_id = %s AND d.external_id = %s AND dtp.parameter_number = %s'''
# cursorINP.execute(hardcodeRepairParam, (tID, sID, dexID, 60, cUser, tID, sID, dexID, 1))
# hardcodeRepairEventDist = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, create_user, create_timestamp) SELECT %s, %s, %s, d.name, d.id, e.id, ot.id, %s, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id WHERE e.tenant_id = %s and e.simulation_id = %s AND d.external_id = %s and ot.external_id = %s and e.name like "Repair"''' # can also include location and parent object type for failure rates, but haven't yet
# cursorINP.execute(hardcodeRepairEventDist, (tID, sID, dexID, cUser, tID, sID, dexID, otExternalID ))



# right now I'm hardcoding to just get repair at any level 10000 because I'm short on time TODO: make repair distributions more general
# I can't specify the type of maintenance without throwing an error so I'll pull all the data and cull it later
sqlAllMaint = '''SELECT sr.[SRAN ID] as lEid, sr.[SERVER TYPE] as mntType, sr.[Object type] as otEid, sr.[Tsf Dist] AS distType, sr.[Tsf P1] AS p1, sr.[Tsf P2] AS p2 FROM [*Server times] as sr WHERE sr.[SRAN ID]=10000'''
curSource.execute(sqlAllMaint)
maintDists = curSource.fetchall()
repairDists = []
for row in maintDists:
    if row.mntType.encode() == 'REPAIR':
        repairDists.append(row)
# now deposit
exID = 1
sqlRep = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, dc.id, dt.id, ?, ?, CURRENT_TIMESTAMP FROM distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = ? and dc.simulation_id = ? and dt.external_id = ?'''
for row in repairDists:
    tempName = 'Repair exTID: ' + str(row.otEid) + ' exLID: ' + str(row.lEid) # concat a name, can include spaces
    cursorINP.execute(sqlRep, (tID, sID, exID, tempName, repairUOM, cUser, tID, sID, constantExID))
    exID += 1
print "Done with Distribution"
###################################### / END DISTRIBUTION PARAMETER
###################################### / BEGIN DISTRIBUTION PARAMETER
# Failure distributions
# these will always be weibulls with two parameters
# we already have the parameters from inserting the distributions
sqlFR = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like "Failure%" AND dtp.parameter_number = ?'''
dexID = 1 # for the distribution number (row number)
exID = 1 # for the parameter number (row number x 2-ish)
for row in failureRates: 
    # first scale
    cursorINP.execute(sqlFR, (tID, sID, exID, 1000/row.scale if row.scale>0 else 1e8, cUser, tID, sID, dexID, 1)) # DEMAND model is in failures per thousand operating hours - we need hours between failure (need to also adjust for optempo)
    exID += 1
    # now shape
    cursorINP.execute(sqlFR, (tID, sID, exID, row.shape, cUser, tID, sID, dexID, 2))
    exID += 1
    dexID += 1

# Now the repair distributions
sqlRep = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like "Repair%" AND dtp.parameter_number = ?'''
dexID = 1 # for the distribution number (row number)
exID = 1 # for the parameter number (row number x 2-ish)
for row in repairDists:
    # first p1
    cursorINP.execute(sqlRep, (tID, sID, exID, row.p1, cUser, tID, sID, dexID, 1))
    exID += 1
    # now p2
    if row.distType.encode() in ('Weibull','Normal'): # 2-parameter distributions.  uniform isn't allowed in DEMAND Pro
        cursorINP.execute(sqlRep, (tID, sID, exID, row.p2, cUser, tID, sID, dexID, 2))
        exID += 1
    dexID += 1

###################################### / END DISTRIBUTION PARAMETER
###################################### / BEGIN EVENT DISTRIBUTION
sqlFED = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, ot.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and d.name like "Failure%" and e.name like "Failure"''' # can also include location and parent object type for failure rates, but haven't yet
## Failure distributions
# use that same failure rate pyodbc list
exID = 1 # for external event_distribution id
for row in failureRates:
    cursorINP.execute(sqlFED, (tID, sID, exID, cUser, tID, sID, exID, row.otEid ))
    exID += 1

## Repair distributions
sqlRED = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, ot.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and d.name like "Repair%" and e.name like "Repair"''' # can also include location and parent object type for failure rates, but haven't yet
exID = 1 # for external event_distribution id
for row in repairDists:
    cursorINP.execute(sqlRED, (tID, sID, exID, cUser, tID, sID, exID, row.otEid ))
    exID += 1

###################################### / END EVENT DISTRIBUTION

###################################### / BEGIN STRUCTURE
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
print 'Done with Structure and Structure_Object'
###################################### / END STRUCTURE

###################################### / BEGIN STORAGE TYPE
# Types are hardcoded to 'MaintenanceAndStorage'
sqlST = '''INSERT INTO storage_type (tenant_id, simulation_id, name, internal_name, external_id, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?, ?)''' # just one class right now
cursorINP.execute(sqlST, (tID, sID, 'Maintenance and Storage', 'MaintenanceAndStorage', 1, cUser, datetime.datetime.now().isoformat()))
print "Done with Storage Type"
###################################### / END STORAGE TYPE
###################################### / BEGIN STORAGE
# Each location has one storage
sqlS = '''INSERT INTO input.storage (tenant_id, simulation_id, name, internal_name, location_id, external_id, storage_type_id, default_for_new_buys, create_user, create_timestamp)
select ?, ?, l.name, l.internal_name, l.id, l.external_id, st.id, 0, ?, current_timestamp() FROM location l join storage_type st on st.simulation_id=l.simulation_id where st.external_id = ? and l.tenant_id = ? and l.simulation_id = ?'''
# query to paste into source input
cursorINP.execute(sqlS, (tID, sID, cUser, 1, tID, sID))
print "Done with Storage"
# Add spares to storages
sqlS = '''UPDATE input.object o join input.storage s on s.location_id = o.location_id set o.storage_id = s.id where o.parent_object_id is null and asset = 0 and s.tenant_id = ? and s.simulation_id = ?'''
cursorINP.execute(sqlS, (tID, sID))
###################################### / END STORAGE
 
###################################### / BEGIN FUTURE INVENTORY 
# get data - only select the objects that are not originally in the simulation (with arrival time > 0)
sqlS = '''SELECT oai.[object type] AS otEid, oai.sran AS lEid, Count(oai.id) AS num2acq, oai.[Arrival time ta] as ArrivalYear FROM [*object attributes initial] AS oai
WHERE (((oai.[Parent Pp])=0)) GROUP BY oai.[object type], oai.sran, oai.[Arrival time ta] HAVING (((oai.[Arrival time ta])<>0))'''
curSource.execute(sqlS)
futureInventory = curSource.fetchall()
# get analysis range
sqlSY = '''SELECT [First year (>= 1999)] from [*Analysis Range]'''
curSource.execute(sqlSY)
startingFiscalYear = curSource.fetchone()[0]
sqlSQ = '''SELECT [First quarter (1 - 4)] from [*Analysis Range]'''
curSource.execute(sqlSQ)
startingFiscalQtr = curSource.fetchone()[0]
# insert properties
sqlProp = '''INSERT INTO property (tenant_id, simulation_id, name, unit_of_measure_id, external_id, create_user, create_timestamp) SELECT ?, ?, ?, uom.id, ?, ?, CURRENT_TIMESTAMP FROM lcm.unit_of_measure uom WHERE uom.tenant_id = ? AND uom.external_id = ?'''
cursorINP.execute(sqlProp, (tID, sID, 'Object Type Internal Name', 1, cUser, tID, 4))
cursorINP.execute(sqlProp, (tID, sID, 'Number to Acquire', 2, cUser, tID, 5))
# insert events and event schedules and properties
SQLacquisitionID = '''SELECT id from input.event_type where name like "Acquisition" AND tenant_id = %s and simulation_id = %s''' % (tID,sID)
cursorINP.execute(SQLacquisitionID)
acquisitionEtId = cursorINP.fetchone()[0]
# save off the previous acquisition event's external id to replace later
sqlExID = '''SELECT external_id from input.event WHERE tenant_id = %s AND simulation_id = %s AND event_type_id = %s''' % (tID, sID, acquisitionEtId)
cursorINP.execute(sqlExID)
oldAcqExID = cursorINP.fetchone()[0]
sqlExID = '''UPDATE input.event SET external_id = 0 WHERE tenant_id = %s AND simulation_id = %s AND event_type_id = %s''' % (tID, sID, acquisitionEtId)
cursorINP.execute(sqlExID)
sqlFIE = '''INSERT INTO input.event (tenant_id, simulation_id, external_id, name, event_type_id, create_user, create_timestamp) VALUES (?,?,?,?,?,?,CURRENT_TIMESTAMP)'''
sqlES = '''INSERT INTO input.event_schedule (tenant_id, simulation_id, external_id, name, timestamp_value, event_id, create_user, create_timestamp)
    SELECT ?, ?, ?, e.name, ?, e.id, ?, CURRENT_TIMESTAMP FROM input.event e WHERE e.event_type_id = ? AND e.external_id = ? AND e.tenant_id = ? AND e.simulation_id = ?'''
sqlEP1 = '''INSERT INTO input.event_property (tenant_id, simulation_id, external_id, event_id, property_id, property_value, create_user, create_timestamp)
    SELECT ?, ?, ?, e.id, p.id, ot.internal_name, ?, CURRENT_TIMESTAMP FROM input.event e JOIN input.property p on e.simulation_id = p.simulation_id JOIN input.object_type ot on ot.simulation_id = e.simulation_id WHERE e.event_type_id = ? AND e.external_id = ? AND e.tenant_id = ? AND e.simulation_id = ? AND p.external_id = ? AND ot.external_id = ?'''
sqlEP2 = '''INSERT INTO input.event_property (tenant_id, simulation_id, external_id, event_id, property_id, property_value, create_user, create_timestamp)
    SELECT ?, ?, ?, e.id, p.id, ?, ?, CURRENT_TIMESTAMP FROM input.event e JOIN input.property p on e.simulation_id = p.simulation_id JOIN input.object_type ot on ot.simulation_id = e.simulation_id WHERE e.event_type_id = ? AND e.external_id = ? AND e.tenant_id = ? AND e.simulation_id = ? AND p.external_id = ? AND ot.external_id = ?'''
exID = 1
for row in futureInventory:
    Year = startingFiscalYear + row.ArrivalYear + math.floor(((row.ArrivalYear % 1) + startingFiscalQtr)/4)
    Qtr = (startingFiscalQtr + (row.ArrivalYear % 1) ) % 4
    cursorINP.execute(sqlFIE, (tID, sID, exID, str('Acquisition OT:' + str(row.otEid) + ' Loc:' + str(row.lEid) + ' Date:' + str(int(Year)) + "Q" + str(int(Qtr))),acquisitionEtId, cUser))
    cursorINP.execute(sqlES, (tID, sID, exID, str(str(int(Year))+'-'+str(1+((int(Qtr)-1)*3)).zfill(2)+'-01'), cUser, acquisitionEtId, exID, tID, sID)) 
    cursorINP.execute(sqlEP1, (tID, sID, exID, cUser, acquisitionEtId, exID, tID, sID, 1, row.otEid))
    cursorINP.execute(sqlEP2, (tID, sID, exID, int(row.num2acq), cUser, acquisitionEtId, exID, tID, sID, 2, row.otEid))
    exID += 1
print "Done with Future Inventory"
###################################### / END FUTURE INVENTORY

###################################### / BEGIN OPTEMPO
sqlFP = '''SELECT [Flying Program] from [*Analysis Control Panel]'''
curSource.execute(sqlFP)
FP = curSource.fetchone()
###################################### / END OPTEMPO

############################################################################ / BEGIN OUTPUT DATA ############################################################################ / BEGIN OUTPUT DATA
############################################################################ / BEGIN OUTPUT DATA ############################################################################ / BEGIN OUTPUT DATA
############################################################################ / BEGIN OUTPUT DATA ############################################################################ / BEGIN OUTPUT DATA

# Output data requires much some information from the LCM schema and much information from the INPUT schema.
# First put data in the output.simulation table.
###################################### / BEGIN SIMULATION
sqlOS = '''INSERT INTO simulation (tenant_id, simulation_id, simulation_name, simulation_type_name) SELECT DISTINCT %s, %s, s.name, st.name FROM lcm.simulation s, lcm.simulation_type st WHERE s.id = %s AND st.id = %s ''' % (tID, sID, sID, simTypeID)
cursorOUT.execute(sqlOS)
###################################### / END SIMULATION
###################################### / BEGIN AVAILABILITY
sqlDOAquarter = '''SELECT [*Weeks and Dates].Date, Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed]) AS avail, [out Availability].SRAN, [out Availability].Type
FROM [out Availability] INNER JOIN [*Weeks and Dates] ON ([out Availability].Year = [*Weeks and Dates].Year) AND ([out Availability].Qtr = [*Weeks and Dates].Quarter)
WHERE ((([*Weeks and Dates].Week)=1))
GROUP BY [*Weeks and Dates].Date, [out Availability].Year, [out Availability].Qtr, [out Availability].SRAN, [out Availability].Type'''
sqlDOAquarter = '''SELECT %s, %s, %s, %s, [*Weeks and Dates].Date, Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed])*100 AS avail, %s, %s, %s, [out Availability].SRAN, 
[out Availability].Type FROM [out Availability] INNER JOIN [*Weeks and Dates] ON ([out Availability].Year = [*Weeks and Dates].Year) AND ([out Availability].Qtr = [*Weeks and Dates].Quarter) WHERE ((([*Weeks and Dates].Week)=1)) 
GROUP BY [*Weeks and Dates].Date, [out Availability].Year, [out Availability].Qtr, [out Availability].SRAN, [out Availability].Type''' % (tID, sID, pID, qtr, pID, tID, qtr)
sqlDOAweek = '''SELECT [*Weeks and Dates].Date, 
Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed]) AS avail, [out Availability].SRAN, [out Availability].Type
FROM [out Availability] INNER JOIN [*Weeks and Dates] ON ([*Weeks and Dates].Week = [out Availability].Week) AND ([out Availability].Qtr = [*Weeks and Dates].Quarter) AND ([out Availability].Year = [*Weeks and Dates].Year)
GROUP BY [*Weeks and Dates].Date, [out Availability].SRAN, [out Availability].Type, [out Availability].Year, [out Availability].Qtr'''
sqlFOA = '''INSERT INTO output.availability (tenant_id, simulation_id, project_name, project_id, asset, spare, interval_unit_id, interval_unit_name, timestamp, value, location_id, location_name, object_type_id, object_type_name)
    SELECT ?, ?, pr.name, ?, 1, 0, iu.id, ?, ?, ?, l.id, l.name, ot.id, ot.name FROM lcm.project pr JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id, lcm.interval_unit iu WHERE pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND l.external_id = ? AND ot.external_id = ?'''
curSource.execute(sqlDOAquarter)
qtrAvail = curSource.fetchall()
qtr = ''' 'CALENDAR_QUARTER' '''
curSource.execute(sqlDOAweek)
wkyAvail = curSource.fetchall()
wky = "CALENDAR_WEEK"
# now load data
# create temp list (of lists)
qtrAvailAll = [[]]
rownum = 0
for row in qtrAvail:
    for col in [tID, sID, pID, qtr, row.Date.isoformat(), row.avail*100, pID, tID, qtr, row.SRAN, row.Type]: # avail in Insight expects a value between 0 and 100
        qtrAvailAll[rownum].append(col)
    qtrAvailAll.append([])
    rownum += 1
qtrAvailAll.pop()
cursorOUT.executemany(sqlFOA, qtrAvailAll)



for row in wkyAvail:
    cursorOUT.execute(sqlFOA, (tID, sID, pID, wky, row.Date.isoformat(), row.avail*100, pID, tID, wky, row.SRAN, row.Type))
print 'Done with Output Availability'
###################################### / END AVAILABILITY
###################################### / BEGIN COMPONENT EVENTS
# first do LRU events (ASSUMES ONLY ONE EVENT OF TYPE FAILURE CALLED FAILURE - SINGLE FAILURE MODE)
eventNameMatch = 'Failure'
sqlDOLruEv = '''SELECT [out LRU events].[SRAN ID] as SRAN, [out LRU events].[Engine type] as Type, [*Weeks and Dates].Date, [out LRU events].UER, [out LRU events].[Life limits] as LL
FROM [out LRU events] INNER JOIN [*Weeks and Dates] ON ([out LRU events].Year = [*Weeks and Dates].Year) AND ([out LRU events].Qtr = [*Weeks and Dates].Quarter)
WHERE ((([*Weeks and Dates].Week)=1))'''
curSource.execute(sqlDOLruEv)
failures = curSource.fetchall()
sqlFOLruEv = '''INSERT INTO output.component_events (tenant_id, simulation_id, project_name, project_id, event_id, event_name, event_type_id, event_type_name, interval_unit_id, interval_unit_name, timestamp, value, location_id, location_name, object_type_id, object_type_name)
    SELECT ?, ?, pr.name, ?, e.id, e.name, et.id, et.name, iu.id, ?, ?, ?, l.id, l.name, ot.id, ot.name FROM input.event e JOIN input.event_type et on et.id = e.event_type_id JOIN lcm.project pr ON e.tenant_id = pr.tenant_id JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id, lcm.interval_unit iu WHERE et.tenant_id = ? and et.simulation_id = ? AND pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND l.external_id = ? AND ot.external_id = ? AND e.name = ?'''
for row in failures:
    cursorOUT.execute(sqlFOLruEv, (tID, sID, pID, qtr, row.Date.isoformat(), row.UER, tID, sID, pID, tID, qtr, row.SRAN, row.Type, eventNameMatch)) # avail in Insight expects a value between 0 and 100

###################################### / END COMPONENT EVENTS

###################################### / BEGIN SPARE QUANTITY
sqlDSpQtr = '''SELECT [*Weeks and Dates].Date, [out Uninstalled-Serviceable items].[Object type] AS Type, [out Uninstalled-Serviceable items].[SRAN ID] AS SRAN, [out Uninstalled-Serviceable items].Serviceables
FROM [out Uninstalled-Serviceable items] INNER JOIN [*Weeks and Dates] ON ([out Uninstalled-Serviceable items].Qtr = [*Weeks and Dates].Quarter) AND ([out Uninstalled-Serviceable items].Year = [*Weeks and Dates].Year) AND ([out Uninstalled-Serviceable items].Week = [*Weeks and Dates].Week)
WHERE ((([*Weeks and Dates].Week)=1))'''
sqlDSpWeek = '''SELECT [*Weeks and Dates].Date, [out Uninstalled-Serviceable items].[Object type] AS Type, [out Uninstalled-Serviceable items].[SRAN ID] AS SRAN, [out Uninstalled-Serviceable items].Serviceables
FROM [out Uninstalled-Serviceable items] INNER JOIN [*Weeks and Dates] ON ([out Uninstalled-Serviceable items].Week = [*Weeks and Dates].Week) AND ([out Uninstalled-Serviceable items].Year = [*Weeks and Dates].Year) AND ([out Uninstalled-Serviceable items].Qtr = [*Weeks and Dates].Quarter)'''
###################################### / END SPARE QUANTITY

connSinkLCM.commit()
connSinkInput.commit()
connSinkOut.commit()
