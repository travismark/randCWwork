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
import math
import csv # output data
import timeit # debug
import datetime # for time
import pyodbc
#import pymysql.cursors

# Define a few variables
sID = 1 # simulation id
tID = 1000 # tenant id
pID = 3 # project id
cUser = 'user' # default create_user
SimOrDemo = 1 # Simulation, 1 (with failure rates defined in calendar time) or demo, 2 (with failure rates defined in operating hours)
LocalOrWeb = 1 # local, 1 (localhost) or web, 2 (out on insight)
numberOfReps = 100
newTenant = 1
prevTime = datetime.datetime.now().time().isoformat()
simTypeID = 1 if SimOrDemo == 1 else 1 # set simulation_type_id to 1 (Insight) for all cases - only these models show up in the scenarios tab  

### Define working directory
#workDir = 'C:/Users/tbaer/Desktop/demo/data/'
#os.chdir(workDir)
### Define the database connections
# First the Source database (DEMAND Pro)
#db_path = 'C:/Users/tbaer/Desktop/demo/data/'
#model = 'test.mdb'
db_path = 'P:/Internal Projects/Data Scientist Team/InsightLCM/Testing/FAST/DEMAND Pro Basic Training/BasicCourseModelsDEMAND/PreEx1/'
model = "Model 1-1_TS3.mdb"
#db_path = "P:/Internal Projects/Data Scientist Team/InsightLCM/Demo/Aviation/Early2015/1June/"
#model = "1-Baseline.mdb"
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
    aServer = '10.100.2.116'  ### SET THIS TO YOUR FAVORITE WEB DB
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
if cursorLCM.execute(sqlCheck, (tID,)).rowcount != 1: # will return 1 if it exists, 0 if not
    sqlEnterTenant = '''INSERT INTO `tenant` (`id`, `name`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?)'''
    cursorLCM.execute(sqlEnterTenant, (tID, str('Tenant_'+str(tID)), 'user', datetime.datetime.now().isoformat()))
else:
    cursorLCM.execute(sqlMax)
    maxTenantID = cursorLCM.fetchone()[0] + 1
    newTenant = 0
# otherwise do nothing - we'll use this tenant

# Create the project if it doesn't exist
sql = "SELECT id FROM `project` WHERE `id`=?"
if cursorLCM.execute(sql, (pID,)).rowcount != 1: # then create it
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
sql = "SELECT id FROM `simulation` WHERE `id`=?"
cursorLCM.execute(sql, (sID,))
if cursorLCM.rowcount == 0: # then create it, otherwise give an error
    sqlEnterSimulation = '''INSERT INTO `simulation` (`id`, `tenant_id`, `name`, `simulation_type_id`, `output_flag`, `baseline_flag`, `project_id`, `number_of_replications`, `start_date`, `end_time`,
    `interval_unit_id`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'''
    cursorLCM.execute(sqlEnterSimulation, (sID, tID, model.split('.')[0], simTypeID, True, False, pID, numberOfReps, dateInfo.Date.isoformat(), totalSimMils, 14, 'user', datetime.datetime.now().isoformat()))
else:
    print 'simulation already exists - choose a different simulation id and try again'
    raise Exception # can't add data to an existing simulation

################################################################
################################################################ DONE CONFIRMING THREE IDS

################################################################ NOW ENTER DATA

###################################### / BEGIN UNIT OF MEASURE
# only enter these units of measure if it's a new tenant
if newTenant == 1:
    sqlUoM = '''INSERT INTO unit_of_measure (tenant_id, name, interval_unit_id, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
    sqlUoMcurrNoIU = '''INSERT INTO unit_of_measure (tenant_id, name, currency_id, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
    sqlUoMnoIU = '''INSERT INTO unit_of_measure (tenant_id, name, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)'''
    sqlUoMcurrAndIU = '''INSERT INTO unit_of_measure (tenant_id, name, currency_id, interval_unit_id, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
    cursorLCM.execute(sqlUoM, (tID, 'Hours', 10, 1, cUser)) # for failures
    cursorLCM.execute(sqlUoM, (tID, 'Days', 9, 2, cUser)) # for repairs and shipping times
    cursorLCM.execute(sqlUoM, (tID, 'Operating Hours', 10, 3, cUser)) # for operating hours (viewing failures in scenario editor)
    cursorLCM.execute(sqlUoMnoIU, (tID, 'Each', 4, cUser)) # for Future Inventory
    cursorLCM.execute(sqlUoMnoIU, (tID, 'Integer', 5, cUser)) # for Future Inventory
    cursorLCM.execute(sqlUoMcurrNoIU, (tID, 'Dollars', 5, 6, cUser)) # for Operations and revenue
    cursorLCM.execute(sqlUoMnoIU, (tID, 'Proportion', 7, cUser)) # for Age Multiplier for initializaiton
    cursorLCM.execute(sqlUoMnoIU, (tID, 'Boolean', 8, cUser)) # for Has failed for initializaiton
    cursorLCM.execute(sqlUoMcurrAndIU, (tID, '$ per Quarter', 5, 3, 9, cUser)) # dollars per quarter for storage cost
    print "Done with Unit of Measure" , datetime.datetime.now().time().isoformat()
###################################### / END UNIT OF MEASURE

# DONE WITH THE LCM STUFF - COMMIT This
connSinkLCM.commit()
####


###################################### / BEGIN LCM 
# only insert LCM data if this is a demo and a new tenant
if SimOrDemo == 2 and newTenant == 1:
    # Location info
    ###################################### / BEGIN LOCATION REGION
    # idealy this would come from the command table, but there's no guarentee that'll be filled in.  so just use one region
    sqlCommand = '''SELECT [*Command names].[Command Name] as name, [*Command names].[Command Name] as intname, [*Command names].Code FROM [*Command names]'''
    curSource.execute(sqlCommand)
    commands = curSource.fetchall()
    sqlLR = '''INSERT INTO location_region (tenant_id, name, internal_name, external_id, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?)'''
    exID = 1
    if len(commands) == 0:
        cursorLCM.execute(sqlLR, (tID, 'Region', 'Region', 1, cUser, datetime.datetime.now().isoformat()))
    else:
        for row in commands:
            row.intname = row.intname.replace(' ','')
            cursorLCM.execute(sqlLR, (tID, row.name, row.intname, exID, cUser, datetime.datetime.now().isoformat()))
            exID += 1
    print "Done with LCM Location Region" , datetime.datetime.now().time().isoformat()
    ###################################### / END LOCATION REGION
    ###################################### / BEGIN LOCATION TYPE
    # Types are hardcoded to the three Maintenance Levels
    sqlLT = '''INSERT INTO location_type (tenant_id, name, external_id, create_user, create_timestamp) values (?, ?, ?, ?, ?)''' # just one class right now
    cursorLCM.execute(sqlLT, (tID, 'Organizational', 1, cUser, datetime.datetime.now().isoformat()))
    cursorLCM.execute(sqlLT, (tID, 'Intermediate', 2, cUser, datetime.datetime.now().isoformat()))
    cursorLCM.execute(sqlLT, (tID, 'Depot', 3, cUser, datetime.datetime.now().isoformat()))
    print "Done with LCM Location Type" , datetime.datetime.now().time().isoformat()
    ###################################### / END LOCATION TYPE
    ###################################### / BEGIN LOCATION
    # get source data
    sqlL = '''SELECT [*base names].[base name] AS name, [*base names].[base name] AS intname, Left([sran],1) AS Mlevel, [*base names].sran AS lEid, [*Command names].[Command Name] as Cname
    FROM [*base names] INNER JOIN [*Command names] ON [*base names].Command = [*Command names].Code'''
    sqlLdefault = '''SELECT [base name] as name, [base name] as intname, left([sran],1) as Mlevel, sran as lEid FROM [*base names]'''
    curSource.execute(sqlL)
    locations = curSource.fetchall()
    if len(locations) == 0: # no location matches to command codes - do default
        curSource.execute(sqlLdefault)
        locations = curSource.fetchall()
        # remove spaces from internal name and check for duplictates, then add
        locationdict = {}
        dupes = 1
        # query to paste into source input
        sqlLdefault = '''INSERT INTO location (tenant_id, name, internal_name, external_id, location_type_id, region_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, lt.id, lr.id, 'user', CURRENT_TIMESTAMP FROM lcm.location_type lt JOIN lcm.location_region lr on lt.tenant_id = lr.tenant_id  where lt.tenant_id = ? and lt.external_id = ?'''
        for row in locations:
            if locationdict.has_key(row.name): # if it's a duplicate, add a number to it
                row.name = row.name + str(dupes)
                row.intname = row.name
                dupes += 1
            locationdict.update({row.name:1})   # add to dictionary after checking for duplicate
            row.intname = row.intname.replace(' ','') # do this check regardless
            cursorLCM.execute(sqlLdefault, (tID, row.name, row.intname, row.lEid, tID, row.Mlevel))
    else: # commands match
        # remove spaces from internal name and check for duplictates, then add
        locationdict = {}
        dupes = 1
        # query to paste into source input
        sqlL = '''INSERT INTO location (tenant_id, name, internal_name, external_id, location_type_id, region_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, lt.id, lr.id, 'user', CURRENT_TIMESTAMP FROM lcm.location_type lt JOIN lcm.location_region lr on lt.tenant_id = lr.tenant_id where lt.tenant_id = ? and lt.external_id = ? and lr.name = ?'''
        for row in locations:
            if locationdict.has_key(row.name): # if it's a duplicate, add a number to it
                row.name = row.name + str(dupes)
                row.intname = row.name
                dupes += 1
            locationdict.update({row.name:1})   # add to dictionary after checking for duplicate
            row.intname = row.intname.replace(' ','') # do this check regardless
            cursorLCM.execute(sqlL, (tID, row.name, row.intname, row.lEid, tID, row.Mlevel, row.Cname))
    print "Done with LCM Location" , datetime.datetime.now().time().isoformat()
    ###################################### / END LOCATION
    ###################################### / BEGIN OBJECT GROUP
    # group is hard coded as asset or component
    # we don't know the ids, so don't specify 
    sql = '''INSERT INTO `object_group` (`tenant_id`, `name`, `internal_name`, `external_id`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?, ?, ?)'''
    cursorLCM.execute(sql, (tID, 'Asset', 'Asset', 1, cUser, datetime.datetime.now().isoformat()))
    cursorLCM.execute(sql, (tID, 'Component', 'Component', 2, cUser, datetime.datetime.now().isoformat()))
    print "Done with LCM Object Group" , datetime.datetime.now().time().isoformat()

    ###################################### / BEGIN OBJECT CLASS
    # Class comes from the class data table
    # insert both assets and components
    sqlA = '''SELECT DISTINCT %s AS ten, [*Class data].[object class] AS ocEid, 1 as ogEid, [*Class data].Name, [*Class data].Name AS intname FROM [*Class data] INNER JOIN [*Object type] ON [*Class data].[Object Class] = [*Object type].[Object class] WHERE ((([*Object type].[Ind level])=1))''' % (tID) 
    sqlC = '''SELECT DISTINCT %s AS ten, [*Class data].[object class] AS ocEid, 2 as ogEid, [*Class data].Name, [*Class data].Name AS intname FROM [*Class data] INNER JOIN [*Object type] ON [*Class data].[Object Class] = [*Object type].[Object class] WHERE ((([*Object type].[Ind level])<>1))'''  % (tID)
    # first insert the assets, then the components
    curSource.execute(sqlA) # get the data
    classes=curSource.fetchall()
    sqlAput = '''INSERT INTO object_class (tenant_id, external_id, object_group_id, name, internal_name, create_user, create_timestamp) SELECT ?, ?, og.id, ?, ?, ?, CURRENT_TIMESTAMP FROM object_group og where og.tenant_id = ? and og.external_id = ?''' ## pymysql takes %s, wheras pyodbc takes ?
    # now loop through data
    for row in classes:
        # convert internal name so it doesn't have spaces
        row.intname = row.intname.replace(' ','')
        # convert any reserved internal names
        if row.intname in ['Part','Platform','Storage','Structure']:
            row.intname = row.intname + '1'
        # deposit the data
        cursorLCM.execute(sqlAput, (tID,row.ocEid, row.Name, row.intname, cUser, tID, row.ogEid))
    ## and repeat for components
    # get the pyodbc table
    curSource.execute(sqlC) 
    classes=curSource.fetchall()
    sqlCput = '''INSERT INTO object_class (tenant_id, external_id, object_group_id, name, internal_name, create_user, create_timestamp) SELECT ?, ?, og.id, ?, ?, ?, CURRENT_TIMESTAMP FROM object_group og where og.tenant_id = ? and og.external_id = ?''' ## pymysql takes %s, wheras pyodbc takes ?
    # take out spaces
    for row in classes:
        row.intname = row.intname.replace(' ','')
        cursorLCM.execute(sqlCput, (tID,row.ocEid, row.Name, row.intname, cUser, tID, row.ogEid))
    print "Done with LCM Object Class" , datetime.datetime.now().time().isoformat()
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
    sqlT = '''INSERT INTO object_type (tenant_id, object_class_id, name, internal_name, part_number, external_id, create_user, create_timestamp) SELECT ?, oc.id, ?, ?, ?, ?, 'user', CURRENT_TIMESTAMP FROM object_class oc where oc.tenant_id = ? and oc.external_id = ?'''
    for row in types:
        if typedict.has_key(row.name): # if it's a duplicate, add a number to it
            row.name = row.name + str(dupes)
            row.intname = row.name
            dupes += 1
        typedict.update({row.name:1})   # add to dictionary after checking for duplicate
        row.intname = row.intname.replace(' ','') # do this check regardless
        cursorLCM.execute(sqlT, (tID, row.name, row.intname, row.pn, row.otEid, tID, row.ocEid))
    print "Done with LCM Object Type" , datetime.datetime.now().time().isoformat()
    ###################################### / END OBJECT TYPE

    ###################################### / BEGIN OBJECT AGING UNIT
    sqlAging = '''INSERT INTO object_aging_unit_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "Flying Hours", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlAging, (tID, cUser))
    ###################################### / END OBJECT AGING UNIT
    ###################################### / BEGIN OBJECT AVAILABILITY AND UTILIZATION AND STATE TYPE
    sqlInsert = '''INSERT INTO object_availability_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "AVAILABLE", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    sqlInsert = '''INSERT INTO object_availability_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "NOT AVAILABLE", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    sqlInsert = '''INSERT INTO object_utilization_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "UTILIZED", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    sqlInsert = '''INSERT INTO object_utilization_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "NOT UTILIZED", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    sqlInsert = '''INSERT INTO object_state_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "PLATFORM", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    sqlInsert = '''INSERT INTO object_state_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "PART", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    sqlInsert = '''INSERT INTO object_state_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "SPARE", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    sqlInsert = '''INSERT INTO object_state_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "Available_Spare", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    sqlInsert = '''INSERT INTO object_state_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "In-overhaul_Spare", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    sqlInsert = '''INSERT INTO object_state_type (tenant_id, name, create_user, create_timestamp) VALUES (?, "On-order_Spare", ?, CURRENT_TIMESTAMP) '''
    cursorLCM.execute(sqlInsert, (tID, cUser))
    ###################################### / END OBJECT AVAILABILITY AND UTILIZATION AND STATE TYPE

    ###################################### / BEGIN OBJECT & METRICS
    # only select the objects that are originally in the simulation (with arrival time = 0)
    # sqlO = '''SELECT id as oEid, TreeCode as name, [object type] as otEid, sran as lEid, [parent pp] as poEid, iif(mid([object type],4,1)=1,1,0) as asset FROM [*object attributes initial] WHERE [Arrival time ta] = 0'''
    # curSource.execute(sqlO)
    # objects = curSource.fetchall()
    sqlOAll = '''SELECT %s as tenantID, TreeCode as name, TreeCode as intname, TreeCode as sernum, [parent pp] as poEid, id as oEid, '%s' as username, %s as tenant_id2, sran as lEid, [object type] as otEid FROM [*object attributes initial] WHERE [Arrival time ta] = 0''' % (tID, cUser, tID)
    sqlOIns = '''INSERT INTO object (tenant_id, object_type_id, location_id, name, internal_name, serial_number, object_state_type_id, primary_aging_unit_id, update_user, external_id, create_user, create_timestamp) SELECT ?, ot.id, l.id, ?, ?, ?, ost.id, oau.id, ?, ?, ?, CURRENT_TIMESTAMP FROM object_type ot JOIN location l on l.tenant_id = ot.tenant_id JOIN object_state_type ost on ost.tenant_id = l.tenant_id JOIN object_aging_unit_type oau on oau.tenant_id = l.tenant_id WHERE ot.tenant_id = ? and l.external_id = ? and ot.external_id = ? and ost.name like "Part"'''
    curSource.execute(sqlOAll)
    objects = curSource.fetchall()
    # there should never be spaces in the tree code, so can leave the internal name check out
    #for row in objects:
    #   cursorINP.execute(sqlOIns, (tID, sID, row.asset, row.name, row.name, row.name, row.poEid, row.oEid, cUser, tID, sID, row.lEid, row.otEid))
    cursorLCM.executemany(sqlOIns, objects)
    # update the parent objects
    sqlO = '''UPDATE object o JOIN object po ON o.update_user = po.external_id and o.tenant_id = po.tenant_id SET o.parent_object_id = po.id WHERE o.tenant_id = %s''' % (tID)
    cursorLCM.execute(sqlO)
    # update object state types for platforms and spares
    sqlUpd = '''UPDATE object o JOIN object_state_type ost on ost.tenant_id = o.tenant_id SET o.object_state_type_id = ost.id WHERE ost.name = "Platform" and o.parent_object_id is null and o.name like "P%" and o.tenant_id = ?'''
    cursorLCM.execute(sqlUpd, (tID))
    sqlUpd = '''UPDATE object o JOIN object_state_type ost on ost.tenant_id = o.tenant_id SET o.object_state_type_id = ost.id WHERE ost.name = "Available_Spare" and o.name not like "P%" AND o.tenant_id = ?'''
    cursorLCM.execute(sqlUpd, (tID))
    # create object status record for platforms
    sqlInsert = '''INSERT INTO object_status (tenant_id, external_id, object_availability_type_id, object_utilization_type_id, create_user, create_timestamp) SELECT ?, o.id, oat.id, obut.id, ?, CURRENT_TIMESTAMP FROM object o JOIN object_utilization_type obut on obut.tenant_id = o.tenant_id JOIN object_availability_type oat on oat.tenant_id = o.tenant_id JOIN object_state_type ost on ost.tenant_id = o.tenant_id WHERE o.tenant_id = ? AND oat.name like "AVAILABLE" AND obut.name like "UTILIZED" AND ost.id = o.object_state_type_id AND ost.name like "Platform"'''
    cursorLCM.execute(sqlInsert, (tID, cUser, tID))
    # link this status record back to object
    sqlUpd = '''UPDATE object o JOIN object_status os on os.tenant_id = o.tenant_id SET o.object_status_id = os.id WHERE os.external_id = o.id AND o.tenant_id = ?'''
    cursorLCM.execute(sqlUpd, (tID))
    # update status location
    sqlUpd = '''UPDATE object o JOIN object_status os on os.tenant_id = o.tenant_id and o.object_status_id = os.id join location l on l.id = o.location_id SET os.location = l.name WHERE os.tenant_id = ?'''
    cursorLCM.execute(sqlUpd, (tID))
    # update status operations status
    sqlUpd = '''UPDATE object_utilization_type obut JOIN object_status os on os.tenant_id = obut.tenant_id and obut.id = os.object_utilization_type_id SET os.operations_status = obut.name WHERE os.tenant_id = ?'''
    cursorLCM.execute(sqlUpd, (tID))

    # create object metrics record for platforms
    sqlInsert = '''INSERT INTO object_metrics (tenant_id, external_id, create_user, create_timestamp) SELECT ?, o.id, ?, CURRENT_TIMESTAMP FROM object o WHERE o.tenant_id = ? AND o.update_user = 0 and o.name like "P%"'''
    cursorLCM.execute(sqlInsert, (tID, cUser, tID))
    # link this metrics record back to object
    sqlUpd = '''UPDATE object o JOIN object_metrics om on om.tenant_id = o.tenant_id SET o.object_metrics_id = om.id WHERE om.external_id = o.id AND o.tenant_id = ?'''
    cursorLCM.execute(sqlUpd, (tID))

    # create object specifications record for platforms
    sqlInsert = '''INSERT INTO object_specification (tenant_id, external_id, create_user, create_timestamp) SELECT ?, o.id, ?, CURRENT_TIMESTAMP FROM object o WHERE o.tenant_id = ? AND o.update_user = 0 and o.name like "P%"'''
    cursorLCM.execute(sqlInsert, (tID, cUser, tID))
    # link this metrics record back to object
    sqlUpd = '''UPDATE object o JOIN object_specification os on os.tenant_id = o.tenant_id SET o.object_specification_id = os.id WHERE os.external_id = o.id AND o.tenant_id = ?'''
    cursorLCM.execute(sqlUpd, (tID))

    # create future inventory - any part that arrives by the first year (inclusive)
    sqlOFutureSpares = '''SELECT %s as tenantID, TreeCode as name, TreeCode as intname, TreeCode as sernum, [parent pp] as poEid, id as oEid, '%s' as username, %s as tenant_id2, sran as lEid, [object type] as otEid FROM [*object attributes initial] WHERE [Parent Pp]=0 AND [Arrival time ta] Between 0.25 and 1''' % (tID, cUser, tID)
    sqlOIns = '''INSERT INTO object (tenant_id, object_type_id, location_id, name, internal_name, serial_number, object_state_type_id, primary_aging_unit_id, update_user, external_id, create_user, create_timestamp) SELECT ?, ot.id, l.id, ?, ?, ?, ost.id, oau.id, ?, ?, ?, CURRENT_TIMESTAMP FROM object_type ot JOIN location l on l.tenant_id = ot.tenant_id JOIN object_state_type ost on ost.tenant_id = l.tenant_id JOIN object_aging_unit_type oau on oau.tenant_id = l.tenant_id WHERE ot.tenant_id = ? and l.external_id = ? and ot.external_id = ? and ost.name like "On-order_Spare"'''
    curSource.execute(sqlOFutureSpares)
    futureInventory = curSource.fetchall()
    cursorLCM.executemany(sqlOIns, futureInventory)
    # update object state types for future spares
    sqlUpd = '''UPDATE object o JOIN object_state_type ost on ost.tenant_id = o.tenant_id SET o.object_state_type_id = ost.id WHERE ost.name = "On-order_Spare" and o.parent_object_id is null and o.object_state_type_id is null and o.tenant_id = ?'''
    cursorLCM.execute(sqlUpd, (tID))

    print "Done with LCM Object and Metrics" , datetime.datetime.now().time().isoformat()
    ###################################### / END OBJECT & METRICS
    # Relevant object information to update:
    #   Age since last install
    connSinkLCM.commit()

######################################
# INPUT DATA


###################################### / BEGIN EVENT TYPE
sqlET = '''INSERT INTO event_type (tenant_id, simulation_id, name, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
if SimOrDemo == 1:    
    for aName in enumerate(('Failure','Repair','Repair Complete','PM','PM Complete','Shipment','Relocate','Installation','Installation Complete','Removal','Removal Complete','Condemnation','Condemnation Complete','User Defined Function','Retirement','Activate','Passivate','Spare Request','Spare Response','Request Cancellation','Acquisition','Operations','Inspection'),start=1):
       cursorINP.execute(sqlET, (tID, sID, aName[1], aName[0], cUser))
else: # if it's a demo, leave out the events that the consistency check doesn't like: inspection
    for aName in enumerate(('Failure','Repair','Repair Complete','PM','PM Complete','Shipment','Relocate','Installation','Installation Complete','Removal','Removal Complete','Condemnation','Condemnation Complete','User Defined Function','Retirement','Activate','Passivate','Spare Request','Spare Response','Request Cancellation','Acquisition','Operations'),start=1):
       cursorINP.execute(sqlET, (tID, sID, aName[1], aName[0], cUser))
print "Done with Event Type" , datetime.datetime.now().time().isoformat()
###################################### / END EVENT TYPE
###################################### / BEGIN EVENT
# most event types only have one event, but we'll have to add a few more after looping through
sqlE = '''INSERT INTO event (tenant_id, simulation_id, name, external_id, event_type_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, et.id, ?, CURRENT_TIMESTAMP FROM event_type et WHERE et.tenant_id = ? and et.simulation_id = ? and et.external_id = ?'''
if SimOrDemo == 1:
    for aName in enumerate(('Failure','Repair','Repair Complete','PM','PM Complete','Shipment','Relocate','Installation','Installation Complete','Removal','Removal Complete','Condemnation','Condemnation Complete','User Defined Function','Retirement','Activate','Passivate','Spare Request','Spare Available','Request Cancellation','Acquisition','Operations','Inspection'),start=1): # 23 on this row
        cursorINP.execute(sqlE, (tID, sID, aName[1], aName[0], cUser, tID, sID, aName[0]))
    # now add a few more
    cursorINP.execute(sqlE, (tID, sID, 'Spare Unavailable', 26, cUser, tID, sID, 19))  ### THIS IS BAD - HARDCODED !!! TODO: GET RID OF
    cursorINP.execute(sqlE, (tID, sID, 'Parent Needs Inspection', 27, cUser, tID, sID, 23)) # Inspection accessory event
    cursorINP.execute(sqlE, (tID, sID, 'Parent Received Inspection', 28, cUser, tID, sID, 23)) # Inspection accessory event
    cursorINP.execute(sqlE, (tID, sID, 'Inspection Request', 29, cUser, tID, sID, 23)) # Inspection accessory event
    cursorINP.execute(sqlE, (tID, sID, 'No Action', 30, cUser, tID, sID, 2)) # of type Repair
    cursorINP.execute(sqlE, (tID, sID, 'No Fault Found', 31, cUser, tID, sID, 2)) # of type Repair
    cursorINP.execute(sqlE, (tID, sID, 'Tear', 32, cUser, tID, sID, 10)) # of type Removal
    cursorINP.execute(sqlE, (tID, sID, 'Build', 33, cUser, tID, sID, 8)) # of type Installation
    cursorINP.execute(sqlE, (tID, sID, 'Build Complete', 34, cUser, tID, sID, 8)) # of type Installation
else: # demo, so leave some out
    for aName in enumerate(('Failure','Repair','Repair Complete','PM','PM Complete','Shipment','Relocate','Installation','Installation Complete','Removal','Removal Complete','Condemnation','Condemnation Complete','User Defined Function','Retirement','Activate','Passivate','Spare Request','Spare Available','Request Cancellation','Acquisition','Operations'),start=1): # 22 on this row
        cursorINP.execute(sqlE, (tID, sID, aName[1], aName[0], cUser, tID, sID, aName[0]))
    # now add a few more
    cursorINP.execute(sqlE, (tID, sID, 'Spare Unavailable', 23, cUser, tID, sID, 19))  ### THIS IS BAD - HARDCODED !!! TODO: GET RID OF 
print "Done with Event" , datetime.datetime.now().time().isoformat()
###################################### / END EVENT
###################################### / BEGIN PROPERTY
sqlProp = '''INSERT INTO property (tenant_id, simulation_id, name, unit_of_measure_id, external_id, create_user, create_timestamp) SELECT ?, ?, ?, uom.id, ?, ?, CURRENT_TIMESTAMP FROM lcm.unit_of_measure uom WHERE uom.tenant_id = ? AND uom.external_id = ?'''
cursorINP.execute(sqlProp, (tID, sID, 'Failure Time Since Birth', 1, cUser, tID, 3)) # for initialization
cursorINP.execute(sqlProp, (tID, sID, 'Failure Age Multiplier', 2, cUser, tID, 7)) # for repair distributions
cursorINP.execute(sqlProp, (tID, sID, 'Failure Has Failed', 3, cUser, tID, 8)) # for initialization
###################################### / END PROPERTY
###################################### / BEGIN EVENT PROPERTY
# insert properties
sqlProp = '''INSERT INTO event_property (tenant_id, simulation_id, event_id, property_id, property_value, create_user, create_timestamp) SELECT ?, e.simulation_id, e.id, p.id, ?, ?, CURRENT_TIMESTAMP FROM input.event e JOIN input.property p on e.simulation_id = p.simulation_id WHERE e.simulation_id = ? and e.external_id = ? and p.external_id = ?'''
cursorINP.execute(sqlProp, (tID, 1, cUser, sID, 31, 2)) # for No Fault Found on the standard "failure" failure mode. constrast to default for repair: zero
###################################### / END EVENT PROPERTY

# Location info
###################################### / BEGIN LOCATION REGION
# just use one region if none are defined
sqlCommand = '''SELECT [*Command names].[Command Name] as name, [*Command names].[Command Name] as intname, [*Command names].Code FROM [*Command names]'''
curSource.execute(sqlCommand)
commands = curSource.fetchall()
sqlLR = '''INSERT INTO location_region (tenant_id, simulation_id, name, internal_name, external_id, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?, ?)'''
exID = 1
if len(commands) == 0:
    cursorINP.execute(sqlLR, (tID, sID, 'Region', 'Region', 1, cUser, datetime.datetime.now().isoformat()))
else:
    for row in commands:
        row.intname = row.intname.replace(' ','')
        cursorINP.execute(sqlLR, (tID, sID, row.name, row.intname, exID, cUser, datetime.datetime.now().isoformat()))
        exID += 1
print "Done with Location Region" , datetime.datetime.now().time().isoformat()
###################################### / END LOCATION REGION
###################################### / BEGIN LOCATION TYPE
# Types are hardcoded to the three Maintenance Levels
sqlLT = '''INSERT INTO location_type (tenant_id, simulation_id, name, external_id, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?)''' # just one class right now
cursorINP.execute(sqlLT, (tID, sID, 'Organizational', 1, cUser, datetime.datetime.now().isoformat()))
cursorINP.execute(sqlLT, (tID, sID, 'Intermediate', 2, cUser, datetime.datetime.now().isoformat()))
cursorINP.execute(sqlLT, (tID, sID, 'Depot', 3, cUser, datetime.datetime.now().isoformat()))
print "Done with Location Type" , datetime.datetime.now().time().isoformat()
###################################### / END LOCATION TYPE
###################################### / BEGIN LOCATION
# get source data
sqlL = '''SELECT [*base names].[base name] AS name, [*base names].[base name] AS intname, Left([sran],1) AS Mlevel, [*base names].sran AS lEid, [*Command names].[Command Name] as Cname
FROM [*base names] INNER JOIN [*Command names] ON [*base names].Command = [*Command names].Code'''
sqlLdefault = '''SELECT [base name] as name, [base name] as intname, left([sran],1) as Mlevel, sran as lEid FROM [*base names]'''
curSource.execute(sqlL)
locations = curSource.fetchall()
if len(locations) == 0: # no location matches to command codes - do default
    curSource.execute(sqlLdefault)
    locations = curSource.fetchall()
    # remove spaces from internal name and check for duplictates, then add
    locationdict = {}
    dupes = 1
    # query to paste into source input
    sqlLdefault = '''INSERT INTO location (tenant_id, simulation_id, name, internal_name, external_id, location_type_id, location_region_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, ?, lt.id, lr.id, 'user', CURRENT_TIMESTAMP FROM input.location_type lt JOIN input.location_region lr on lt.tenant_id = lr.tenant_id and lt.simulation_id = lr.simulation_id where lt.tenant_id = ? and lt.simulation_id = ? and lt.external_id = ?'''
    for row in locations:
        if locationdict.has_key(row.name): # if it's a duplicate, add a number to it
            row.name = row.name + str(dupes)
            row.intname = row.name
            dupes += 1
        locationdict.update({row.name:1})   # add to dictionary after checking for duplicate
        row.intname = row.intname.replace(' ','') # do this check regardless
        cursorINP.execute(sqlLdefault, (tID, sID, row.name, row.intname, row.lEid, tID, sID, row.Mlevel))
else: # commands match
    # remove spaces from internal name and check for duplictates, then add
    locationdict = {}
    dupes = 1
    # query to paste into source input
    sqlL = '''INSERT INTO location (tenant_id, simulation_id, name, internal_name, external_id, location_type_id, location_region_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, ?, lt.id, lr.id, 'user', CURRENT_TIMESTAMP FROM input.location_type lt JOIN input.location_region lr on lt.tenant_id = lr.tenant_id and lt.simulation_id = lr.simulation_id where lt.tenant_id = ? and lt.simulation_id = ? and lt.external_id = ? and lr.name = ?'''
    for row in locations:
        if locationdict.has_key(row.name): # if it's a duplicate, add a number to it
            row.name = row.name + str(dupes)
            row.intname = row.name
            dupes += 1
        locationdict.update({row.name:1})   # add to dictionary after checking for duplicate
        row.intname = row.intname.replace(' ','') # do this check regardless
        cursorINP.execute(sqlL, (tID, sID, row.name, row.intname, row.lEid, tID, sID, row.Mlevel, row.Cname))
print "Done with Location" , datetime.datetime.now().time().isoformat()
###################################### / END LOCATION

# Insert the object information: state_type, group, class, type, name
###################################### / BEGIN STATE TYPE
# HARD CODED
sql = '''INSERT INTO `object_state_type` (`tenant_id`, `simulation_id`, `name`, `internal_name`, `external_id`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?, ?, ?, ?)'''
cursorINP.execute(sql, (tID, sID, 'Serviceable', 'Serviceable', 1, cUser, datetime.datetime.now().isoformat()))
cursorINP.execute(sql, (tID, sID, 'Unserviceable', 'Unserviceable', 2, cUser, datetime.datetime.now().isoformat()))
cursorINP.execute(sql, (tID, sID, 'Unserviceable and Awaiting Parts', 'UnserviceableAndAwaitingParts', 3, cUser, datetime.datetime.now().isoformat()))
print "Done with Object State Type", datetime.datetime.now().time().isoformat()
###################################### / END OBJECT STATE TYPE
###################################### / BEGIN OBJECT GROUP
# group is hard coded as asset or component
# we don't know the ids, so don't specify 
sql = '''INSERT INTO `object_group` (`tenant_id`, `simulation_id`, `name`, `internal_name`, `external_id`, `create_user`, `create_timestamp`) VALUES (?, ?, ?, ?, ?, ?, ?)'''
cursorINP.execute(sql, (tID, sID, 'Asset', 'Asset', 1, cUser, datetime.datetime.now().isoformat()))
cursorINP.execute(sql, (tID, sID, 'Component', 'Component', 2, cUser, datetime.datetime.now().isoformat()))
print "Done with Object Group" , datetime.datetime.now().time().isoformat()
###################################### / END OBJECT GROUP

###################################### / BEGIN OBJECT CLASS
# Class comes from the class data table
# insert both assets and components
sqlA = '''SELECT DISTINCT %s AS ten, %s AS sim, [*Class data].[object class] AS ocEid, 1 as ogEid, [*Class data].Name, [*Class data].Name AS intname FROM [*Class data] INNER JOIN [*Object type] ON [*Class data].[Object Class] = [*Object type].[Object class] WHERE ((([*Object type].[Ind level])=1))''' % (tID, sID) 
sqlC = '''SELECT DISTINCT %s AS ten, %s AS sim, [*Class data].[object class] AS ocEid, 2 as ogEid, [*Class data].Name, [*Class data].Name AS intname FROM [*Class data] INNER JOIN [*Object type] ON [*Class data].[Object Class] = [*Object type].[Object class] WHERE ((([*Object type].[Ind level])<>1))'''  % (tID, sID)
# first insert the assets, then the components
curSource.execute(sqlA) # get the data
classes=curSource.fetchall()
sqlAput = '''INSERT INTO object_class (tenant_id, simulation_id, external_id, object_group_id, name, internal_name, create_user, create_timestamp) SELECT ?, ?, ?, og.id, ?, ?, ?, CURRENT_TIMESTAMP FROM input.object_group og where og.tenant_id = ? and og.simulation_id = ? and og.external_id = ?''' ## pymysql takes %s, wheras pyodbc takes ?
# now loop through data
for row in classes:
    # convert internal name so it doesn't have spaces
    row.intname = row.intname.replace(' ','')
    # deposit the data
    cursorINP.execute(sqlAput, (tID,sID,row.ocEid, row.Name, row.intname, cUser, tID, sID, row.ogEid))
## and repeat for components
# get the pyodbc table
curSource.execute(sqlC) 
classes=curSource.fetchall()
sqlCput = '''INSERT INTO object_class (tenant_id, simulation_id, external_id, object_group_id, name, internal_name, create_user, create_timestamp) SELECT ?, ?, ?, og.id, ?, ?, ?, CURRENT_TIMESTAMP FROM input.object_group og where og.tenant_id = ? and og.simulation_id = ? and og.external_id = ?''' ## pymysql takes %s, wheras pyodbc takes ?
# take out spaces
for row in classes:
    row.intname = row.intname.replace(' ','')
    cursorINP.execute(sqlCput, (tID,sID,row.ocEid, row.Name, row.intname, cUser, tID, sID, row.ogEid))
print "Done with Object Class" , datetime.datetime.now().time().isoformat()
###################################### / END OBJECT CLASS

###################################### / BEGIN OBJECT TYPE  
# TODO: add template objects during this table
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
print "Done with Object Type" , datetime.datetime.now().time().isoformat()
###################################### / END OBJECT TYPE

###################################### / BEGIN OBJECT
# only select the objects that are originally in the simulation (with arrival time = 0)
# sqlO = '''SELECT id as oEid, TreeCode as name, [object type] as otEid, sran as lEid, [parent pp] as poEid, iif(mid([object type],4,1)=1,1,0) as asset FROM [*object attributes initial] WHERE [Arrival time ta] = 0'''
# curSource.execute(sqlO)
# objects = curSource.fetchall()
sqlOAll = '''SELECT %s as tenantID, %s as simulationID, iif(mid([object type],4,1)=1,1,0) as asset, TreeCode as name, TreeCode as intname, TreeCode as sernum, [parent pp] as poEid, id as oEid, '%s' as username, %s as tenant_id2, %s as simulation_id2, sran as lEid, [object type] as otEid FROM [*object attributes initial] WHERE [Arrival time ta] = 0''' % (tID, sID, cUser, tID, sID)
sqlOIns = '''INSERT INTO object (tenant_id, simulation_id, object_type_id, location_id, active, available, asset, cloned_flag, template_flag, name, internal_name, serial_number, stg_id, external_id, create_user, create_timestamp) SELECT ?, ?, ot.id, l.id, 1, 1, ?, 0, 0, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP FROM object_type ot join location l on l.tenant_id = ot.tenant_id and l.simulation_id = ot.simulation_id WHERE ot.tenant_id = ? AND ot.simulation_id = ? and l.external_id = ? and ot.external_id = ?'''
curSource.execute(sqlOAll)
objects = curSource.fetchall()
# there should never be spaces in the tree code, so can leave the internal name check out
#for row in objects:
#   cursorINP.execute(sqlOIns, (tID, sID, row.asset, row.name, row.name, row.name, row.poEid, row.oEid, cUser, tID, sID, row.lEid, row.otEid))
cursorINP.executemany(sqlOIns, objects)
# update the parent objects
sqlO = '''UPDATE object o JOIN object po ON o.stg_id = po.external_id AND o.simulation_id = po.simulation_id SET o.parent_object_id = po.id WHERE o.tenant_id = %s and o.simulation_id = %s''' % (tID, sID)
cursorINP.execute(sqlO)
print "Done with Object" , datetime.datetime.now().time().isoformat()
###################################### / END OBJECT

# Probability - nrts and conseq_uer
###################################### / BEGIN PROBABILITY CLASS
# two of these so far - condemnation and nrts
sqlPC = '''INSERT INTO probability_class (tenant_id, simulation_id, name, sum_of_one_flag, external_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
cursorINP.execute(sqlPC, (tID, sID, 'Maintenance Unscheduled', True, 1, cUser))
cursorINP.execute(sqlPC, (tID, sID, 'Evacuation Probability', False, 2, cUser))
print "Done with Probabilty Class" , datetime.datetime.now().time().isoformat()
###################################### / END PROBABILITY CLASS
###################################### / BEGIN PROBABILITY TYPE
# five of these
sqlPT = '''INSERT INTO probability_type (tenant_id, simulation_id, name, external_id, stg_id, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
# Maintenance
for aName in enumerate(('No Action','No Fault Found','Condemn','Repair'),start=1): # distributions in alphabetical order
    cursorINP.execute(sqlPT, (tID, sID, aName[1], aName[0], 1, cUser))
# NRTS
cursorINP.execute(sqlPT, (tID, sID, 'Evacuation Probability', 5, 2, cUser))
print "Done with Probability Type" , datetime.datetime.now().time().isoformat()
###################################### / END PROBABILITY TYPE
###################################### / BEGIN Probability  
# the templates are set up to handle defaulting mechanism.  it chooses the most specific matching option
# TODO: need to check whether it deems object information or location information more specific. I would suggest object information is more important

# these are the default types in case I need them
# First do conseq_uer
# what are all the possibilities?  
    # object type: default to group + indenture, class, or specific to type
    # sran: default to all, default to level, specific to sran
# 1) specific type + specific sran
# 2) specific type + level
# 3) specific type + all default
#  ) class + specific sran NOT ALLOWED IN DEMAND
#  ) class + level NOT ALLOWED IN DEMAND
# 4) class + all default
#  ) group/ind + specific sran NOT ALLOWED IN DEMAND
#  ) group/ind + level NOT ALLOWED IN DEMAND
# 5) group/ind + all default

sqlPUER1 = '''SELECT [*Consequences of UER].[Object type] AS otype, [*Consequences of UER].SRAN, [*Consequences of UER].[Pu s,non] AS non, [*Consequences of UER].[Pu s,nff] AS nff, [*Consequences of UER].[Pu s,con] AS con, [*Consequences of UER].[Pu s,rep] AS rep
FROM [*Consequences of UER] WHERE Right([object type],1)>0 AND Right([SRAN],2)>0'''
sqlPUER2 = '''SELECT [*Consequences of UER].[Object type] AS otype, left([*Consequences of UER].SRAN,1) as locTypeExId, [*Consequences of UER].[Pu s,non] AS non, [*Consequences of UER].[Pu s,nff] AS nff, [*Consequences of UER].[Pu s,con] AS con, [*Consequences of UER].[Pu s,rep] AS rep
FROM [*Consequences of UER] WHERE Right([object type],1)>0 AND Right([SRAN],2)=0'''
sqlPUER3 = '''SELECT [*Consequences of UER].[Object type] AS otype, [*Consequences of UER].[Pu s,non] AS non, [*Consequences of UER].[Pu s,nff] AS nff, [*Consequences of UER].[Pu s,con] AS con, [*Consequences of UER].[Pu s,rep] AS rep
FROM [*Consequences of UER] WHERE Right([object type],1)>0 AND Right([SRAN],2)<0'''
sqlPUER4 = '''SELECT Left([*Consequences of UER].[Object type],6) AS oclass, [*Consequences of UER].SRAN, [*Consequences of UER].[Pu s,non] AS non, [*Consequences of UER].[Pu s,nff] AS nff, [*Consequences of UER].[Pu s,con] AS con, [*Consequences of UER].[Pu s,rep] AS rep
FROM [*Consequences of UER] WHERE Right([object type],1)=0 AND Right([object type],5)>0 AND [SRAN]=-1'''
# this last one, group-default, must still use class default and exclude all the matches from the previous one - group doesn't mean the same thing in FAST TODO: investigate how/why we can get this back
sqlPUER5 = '''SELECT Left([*Object Type].[Object type],6) as oclass, [*Consequences of UER].SRAN, [*Consequences of UER].[Pu s,non] AS non, [*Consequences of UER].[Pu s,nff] AS nff, [*Consequences of UER].[Pu s,con] AS con, [*Consequences of UER].[Pu s,rep] AS rep
FROM [*Consequences of UER] INNER JOIN [*Object type] ON left([*Consequences of UER].[Object type],3) = left([*Object type].[Object type],3)
WHERE right([*Consequences of UER].[Object type],5) = 0 AND [*Consequences of UER].SRAN = -1 AND left([*Object type].[Object type],6) not in (SELECT Left([Object type],6) AS avoidClass
FROM [*Consequences of UER] WHERE (((Right([Object type],1))=0) AND (([*Consequences of UER].SRAN)=-1) AND ((Right([Object type],5))<>0)))'''

sqlPUERinsert1 = '''INSERT INTO probability (tenant_id, simulation_id, object_type_id, location_id, probability, probability_type_id, probability_class_id, create_user, create_timestamp) 
SELECT %s, %s, ot.id, l.id, ?, pt.id, pc.id, "%s", CURRENT_TIMESTAMP FROM input.object_type ot JOIN input.location l ON l.simulation_id = ot.simulation_id JOIN input.probability_class pc ON pc.simulation_id = l.simulation_id JOIN input.probability_type pt ON pt.simulation_id = pc.simulation_id WHERE l.tenant_id = %s AND ot.simulation_id = %s AND ot.external_id = ? AND l.external_id = ? AND pc.name like "Maintenance Unscheduled" AND pt.name like ?''' % (tID, sID, cUser, tID, sID)
curSource.execute(sqlPUER1)
probs = curSource.fetchall()
# loop through - insert four LCM rows per DEMAND row
for row in probs:
    cursorINP.execute(sqlPUERinsert1, (row.rep, row.otype, row.SRAN, "Repair"))
    cursorINP.execute(sqlPUERinsert1, (row.con, row.otype, row.SRAN, "Condemn"))
    cursorINP.execute(sqlPUERinsert1, (row.nff, row.otype, row.SRAN, "No Fault Found"))
    cursorINP.execute(sqlPUERinsert1, (row.non, row.otype, row.SRAN, "No Action"))

sqlPUERinsert2 = '''INSERT INTO probability (tenant_id, simulation_id, object_type_id, location_type_id, probability, probability_type_id, probability_class_id, create_user, create_timestamp) 
SELECT %s, %s, ot.id, lt.id, ?, pt.id, pc.id, "%s", CURRENT_TIMESTAMP FROM input.object_type ot JOIN input.location_type lt ON lt.simulation_id = ot.simulation_id JOIN input.probability_class pc ON pc.simulation_id = lt.simulation_id JOIN input.probability_type pt ON pt.simulation_id = pc.simulation_id WHERE lt.tenant_id = %s AND ot.simulation_id = %s AND ot.external_id = ? AND lt.external_id = ? AND pc.name like "Maintenance Unscheduled" AND pt.name like ?''' % (tID, sID, cUser, tID, sID)
curSource.execute(sqlPUER2)
probs = curSource.fetchall()
# loop through - insert four LCM rows per DEMAND row
for row in probs:
    cursorINP.execute(sqlPUERinsert2, (row.rep, row.otype, row.locTypeExId, "Repair"))
    cursorINP.execute(sqlPUERinsert2, (row.con, row.otype, row.locTypeExId, "Condemn"))
    cursorINP.execute(sqlPUERinsert2, (row.nff, row.otype, row.locTypeExId, "No Fault Found"))
    cursorINP.execute(sqlPUERinsert2, (row.non, row.otype, row.locTypeExId, "No Action"))

sqlPUERinsert3 = '''INSERT INTO probability (tenant_id, simulation_id, object_type_id, probability, probability_type_id, probability_class_id, create_user, create_timestamp) 
SELECT %s, %s, ot.id, ?, pt.id, pc.id, "%s", CURRENT_TIMESTAMP FROM input.object_type ot JOIN input.probability_class pc ON pc.simulation_id = ot.simulation_id JOIN input.probability_type pt ON pt.simulation_id = pc.simulation_id WHERE ot.tenant_id = %s AND ot.simulation_id = %s AND ot.external_id = ? AND pc.name like "Maintenance Unscheduled" AND pt.name like ?''' % (tID, sID, cUser, tID, sID)
curSource.execute(sqlPUER3)
probs = curSource.fetchall()
# loop through - insert four LCM rows per DEMAND row
for row in probs:
    cursorINP.execute(sqlPUERinsert3, (row.rep, row.otype, "Repair"))
    cursorINP.execute(sqlPUERinsert3, (row.con, row.otype, "Condemn"))
    cursorINP.execute(sqlPUERinsert3, (row.nff, row.otype, "No Fault Found"))
    cursorINP.execute(sqlPUERinsert3, (row.non, row.otype, "No Action"))

sqlPUERinsert4 = '''INSERT INTO probability (tenant_id, simulation_id, object_class_id, probability, probability_type_id, probability_class_id, create_user, create_timestamp) 
SELECT %s, %s, oc.id, ?, pt.id, pc.id, "%s", CURRENT_TIMESTAMP FROM input.object_class oc JOIN input.probability_class pc ON pc.simulation_id = oc.simulation_id JOIN input.probability_type pt ON pt.simulation_id = pc.simulation_id WHERE oc.tenant_id = %s AND oc.simulation_id = %s AND oc.external_id = ? AND pc.name like "Maintenance Unscheduled" AND pt.name like ?''' % (tID, sID, cUser, tID, sID)
curSource.execute(sqlPUER4)
probs = curSource.fetchall()
# loop through - insert four LCM rows per DEMAND row
for row in probs:
    cursorINP.execute(sqlPUERinsert4, (row.rep, row.oclass, "Repair"))
    cursorINP.execute(sqlPUERinsert4, (row.con, row.oclass, "Condemn"))
    cursorINP.execute(sqlPUERinsert4, (row.nff, row.oclass, "No Fault Found"))
    cursorINP.execute(sqlPUERinsert4, (row.non, row.oclass, "No Action"))

# five uses the same insert as four
curSource.execute(sqlPUER5)
probs = curSource.fetchall()
# loop through - insert four LCM rows per DEMAND row
for row in probs:
    cursorINP.execute(sqlPUERinsert4, (row.rep, row.oclass, "Repair"))
    cursorINP.execute(sqlPUERinsert4, (row.con, row.oclass, "Condemn"))
    cursorINP.execute(sqlPUERinsert4, (row.nff, row.oclass, "No Fault Found"))
    cursorINP.execute(sqlPUERinsert4, (row.non, row.oclass, "No Action"))
# TODO: fix the really minor digits at the end of the probabilities
# NRTS:
# allowed combinations of Type, SRAN, Qtr, Year - only six of them
# what are all the possibilities?  
    # object type: default to group + indenture, class, or specific to type
    # sran: default to all, default to level, specific to sran
    # time: both year and qtr default to -1 or specific
# 1) specific type + specific sran + specific time (Good)
# 2) specific type + specific sran + general time (Good)
#  ) specific type + level + specific time NOT ALLOWED IN DEMAND
# 3) specific type + level + general time (Good)
#  ) specific type + all default + specific time NOT ALLOWED IN DEMAND
# 4) specific type + all default + general time (Good)
#  ) class + specific sran NOT ALLOWED IN DEMAND
#  ) class + level NOT ALLOWED IN DEMAND
# 5) class + all default + time default (Good)
#  ) group/ind + specific sran NOT ALLOWED IN DEMAND
#  ) group/ind + level NOT ALLOWED IN DEMAND
# 6) group/ind + all default + time default (Good)
# print "Done with Probability"
###################################### / END Probability

# Insert Distribution information
# these are standard for each simulation

#### TODO: THROW an error if it sees a Lognormal distribution
###################################### / BEGIN DISTRIBUTION TYPE
sqlDT = '''INSERT INTO distribution_type (tenant_id, simulation_id, external_id, name, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?)''' # standard hard-coded types
for aName in enumerate(('Constant','Exponential','Normal','Uniform','Weibull'),start=1): # distributions in alphabetical order
    cursorINP.execute(sqlDT, (tID, sID, aName[0], aName[1],'user', datetime.datetime.now().isoformat()))
print "Done with Distribution Type" , datetime.datetime.now().time().isoformat()
###################################### / END DISTRIBUTION TYPE
###################################### / BEGIN DISTRIBUTION CLASS
sqlDC = '''INSERT INTO distribution_class (tenant_id, simulation_id, external_id, name, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?)''' # just one class right now
for aName in enumerate(('Unscheduled Removal', 'Shipment Time', 'Server Time', 'Operation Profile', 'General')):
    cursorINP.execute(sqlDC, (tID, sID, aName[0], aName[1],'user', datetime.datetime.now().isoformat()))
print "Done with Distribution Class" , datetime.datetime.now().time().isoformat()
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
print "Done with Distribution Type Parameter" , datetime.datetime.now().time().isoformat()
###################################### / END DISTRIBUTION TYPE PARAMETER
###################################### / BEGIN DISTRIBUTION
# failure distributions first - DEMAND Pro always uses weibull failure rates
weibullExID = 5
sqlFR = '''SELECT [*Unscheduled Removal rates].[LRU  type] AS otEid, [*Unscheduled Removal rates].[Platform type] AS PetId, [*Unscheduled Removal rates].Base as lEid, [*Unscheduled Removal rates].[Age Unit] AS ageType, [*Unscheduled Removal rates].Rate AS scale, [*Unscheduled Removal rates].Shape as shape, [*Unscheduled Removal rates].[Completed Repairs] as cr FROM [*Unscheduled Removal rates]'''
curSource.execute(sqlFR)
failureRates = curSource.fetchall()
# lcm.unit_of_measure is in a different database, so I'll have to query it separately
if SimOrDemo ==1:
    sqlUOM = 'SELECT id from unit_of_measure where external_id = 1 and tenant_id = %s' % tID # calendar "hours"
else:
    sqlUOM = 'SELECT id from unit_of_measure where external_id = 3 and tenant_id = %s' % tID # "operating hours"
cursorLCM.execute(sqlUOM)
failureUOM = cursorLCM.fetchone() # this gives me a dictionary (can use string.encode to convert from bytestring to unicodestring)
failureUOM = failureUOM.id # get the value
exID = 1 # use as counter for inputting parameters later
sqlFR = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, dc.id, dt.id, ?, ?, CURRENT_TIMESTAMP FROM distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = ? and dc.simulation_id = ? and dt.external_id = ? AND dc.name like "Unscheduled Removal" '''
for row in failureRates:
    if row.cr != 0:  # only load the zeroth completed repair now TODO: ADD MORE CRs (use expression & object property?)
        continue
    else:
        if LocalOrWeb == 1: # if it'll be put on the web, set the event distribution to something cleaner, like 'Failure'
            tempName = 'Failure exTID: ' + str(row.otEid) + ' exPID' + str(row.PetId) + ' exLID: ' + str(row.lEid) + ' CR: ' + str(row.cr) # concat a name, can include spaces
        else:
            tempName = 'Failure'
        cursorINP.execute(sqlFR, (tID, sID, exID, tempName, failureUOM, cUser, tID, sID, weibullExID))
        exID += 1
# now repair distributions ############# TBD TBD TBD ##########################  - I SKIPPED REPAIRS AND THEIR DISTRIBUTION_PARAMETERS FOR ONLINE DEMO MODEL

# lcm.unit_of_measure is in a different database, so I'll have to query it separately -- THIS IS NOT TRUE - YOU CAN QUERY ANY SCHEMA
cursorLCM.execute('SELECT id from lcm.unit_of_measure where external_id = 2 and name like "Days" and tenant_id = %s' % tID ) # DEMAND Pro server times are in Days 
maintUOM = cursorLCM.fetchone()
maintUOM = maintUOM.id
constantExID = 1

# right now I'm hardcoding to just get repair at any level 10000 because I'm short on time TODO: make repair distributions more general
# I can't specify the type of maintenance without throwing an error so I'll pull all the data and cull it later
sqlAllMaint = '''SELECT sr.[SRAN ID] as lEid, sr.[SERVER TYPE] as mntType, sr.[Object type] as otEid, sr.[Tsf Dist] AS distType, sr.[Tsf P1] AS p1, sr.[Tsf P2] AS p2 FROM [*Server times] as sr'''
curSource.execute(sqlAllMaint)
maintDists = curSource.fetchall()
repairDists = []
removalDists = []
installDists = []
inspectDists = []
buildDists = []
for row in maintDists:
    if row.mntType.encode().upper() == 'REPAIR':
        repairDists.append(row)
    if row.mntType.encode().upper() == 'TEAR':
        removalDists.append(row)
    if row.mntType.encode().upper() == 'BUILD':
        buildDists.append(row)
    if row.mntType.encode().upper() == 'INSPECT':
        inspectDists.append(row)
# now deposit
# repair
exID = 1
sqlMnt = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, dc.id, dt.id, ?, ?, CURRENT_TIMESTAMP FROM distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = ? and dc.simulation_id = ? and dt.external_id = ? AND dc.name like "Server Time" '''
for row in repairDists:
    tempName = 'Repair exTID: ' + str(row.otEid) + ' exLID: ' + str(row.lEid) # concat a name, can include spaces
    cursorINP.execute(sqlMnt, (tID, sID, exID, tempName, maintUOM, cUser, tID, sID, constantExID))
    exID += 1
# removal and install
exID = 1
for row in removalDists:
    tempName = 'Removal exTID: ' + str(row.otEid) + ' exLID: ' + str(row.lEid)
    cursorINP.execute(sqlMnt, (tID, sID, exID, tempName, maintUOM, cUser, tID, sID, constantExID))
    tempName = 'Installation exTID: ' + str(row.otEid) + ' exLID: ' + str(row.lEid) # install times are zero in DEMAND
    cursorINP.execute(sqlMnt, (tID, sID, exID, tempName, maintUOM, cUser, tID, sID, constantExID))
    exID += 1
# inspect - not handled in Insight yet b/c consistency checks - leave out if demo
if SimOrDemo == 1:
    exID = 1
    for row in inspectDists:
        tempName = 'Inspection exTID: ' + str(row.otEid) + ' exLID: ' + str(row.lEid)
        cursorINP.execute(sqlMnt, (tID, sID, exID, tempName, maintUOM, cUser, tID, sID, constantExID))
        exID += 1
    # build
    exID = 1
    for row in buildDists:
        # build times are assigned to the parent - one time to install all children parts
        tempName = 'Build exTID: ' + str(row.otEid) + ' exLID: ' + str(row.lEid)
        cursorINP.execute(sqlMnt, (tID, sID, exID, tempName, maintUOM, cUser, tID, sID, constantExID))
        exID += 1
# Distribution of constant zero
sqlZero = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) SELECT dc.tenant_id, dc.simulation_id, ?, ?, dc.id, dt.id, uom.id, ?, CURRENT_TIMESTAMP FROM distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id JOIN lcm.unit_of_measure uom on uom.tenant_id = dc.tenant_id WHERE dc.tenant_id = ? and dc.simulation_id = ? and dt.external_id = ? AND uom.external_id = ? AND dc.name like "General" '''
cursorINP.execute(sqlZero, (1, 'Zero', cUser, tID,sID, 1, 2)) # 1 for external id and then constant distribution
print "Done with Distribution" , datetime.datetime.now().time().isoformat()
###################################### / END DISTRIBUTION
###################################### / BEGIN DISTRIBUTION PARAMETER
# Failure distributions
# these will always be weibulls with two parameters
# we already have the parameters from inserting the distributions
sqlFR = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like "Failure%" AND dtp.parameter_number = ?'''
dexID = 1 # for the distribution number (row number)
exID = 1 # for the parameter number (row number x 2-ish)
for row in failureRates: 
    # first scale
    cursorINP.execute(sqlFR, (tID, sID, exID, 1000/row.scale if row.scale>0 else 1e8, cUser, tID, sID, dexID, 1)) # DEMAND model is in failures per thousand operating hours - we need hours between failure (TODO: need to also adjust for optempo)
    exID += 1
    # now shape
    cursorINP.execute(sqlFR, (tID, sID, exID, row.shape, cUser, tID, sID, dexID, 2))
    exID += 1
    dexID += 1

# Now the Repair distributions
sqlMnt = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like "Repair%" AND dtp.parameter_number = ?'''
dexID = 1 # for the distribution number (row number)
exID = 1 # for the parameter number (row number x 2-ish)
for row in repairDists:
    # first p1
    cursorINP.execute(sqlMnt, (tID, sID, exID, row.p1, cUser, tID, sID, dexID, 1))
    exID += 1
    # now p2
    if row.distType.encode() in ('Weibull','Normal'): # 2-parameter distributions.  uniform isn't allowed in DEMAND Pro
        cursorINP.execute(sqlMnt, (tID, sID, exID, row.p2, cUser, tID, sID, dexID, 2))
        exID += 1
    dexID += 1

# Now the Removal distributions
sqlMnt = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like "Removal%" AND dtp.parameter_number = ?'''
dexID = 1 # for the distribution number (row number)
exID = 1 # for the parameter number (row number x 2-ish)
for row in removalDists:
    # first p1
    cursorINP.execute(sqlMnt, (tID, sID, exID, row.p1, cUser, tID, sID, dexID, 1))
    exID += 1
    # now p2
    if row.distType.encode() in ('Weibull','Normal'): # 2-parameter distributions.  uniform isn't allowed in DEMAND Pro
        cursorINP.execute(sqlMnt, (tID, sID, exID, row.p2, cUser, tID, sID, dexID, 2))
        exID += 1
    dexID += 1

# Now the Installation distributions
sqlMnt = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like "Install%" AND dtp.parameter_number = ?'''
dexID = 1 # for the distribution number (row number)
exID = 1 # for the parameter number (row number x 2-ish)
for row in removalDists:
    # first p1
    cursorINP.execute(sqlMnt, (tID, sID, exID, 0, cUser, tID, sID, dexID, 1)) # installation takes no time according to demand pro
    exID += 1
    # now p2
    if row.distType.encode() in ('Weibull','Normal'): # 2-parameter distributions.  uniform isn't allowed in DEMAND Pro
        cursorINP.execute(sqlMnt, (tID, sID, exID, round(row.p2,5), cUser, tID, sID, dexID, 2))
        exID += 1
    dexID += 1

if SimOrDemo == 1: # only if it's not being loaded to the web for a demo
    # Now the Inspection distributions
    sqlMnt = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like "Inspection%" AND dtp.parameter_number = ?'''
    dexID = 1 # for the distribution number (row number)
    exID = 1 # for the parameter number (row number x 2-ish)
    for row in inspectDists:
        # first p1
        cursorINP.execute(sqlMnt, (tID, sID, exID, round(row.p1,5), cUser, tID, sID, dexID, 1))
        exID += 1
        # now p2
        if row.distType.encode() in ('Weibull','Normal'): # 2-parameter distributions.  uniform isn't allowed in DEMAND Pro
            cursorINP.execute(sqlMnt, (tID, sID, exID, round(row.p2,5), cUser, tID, sID, dexID, 2))
            exID += 1
        dexID += 1

    # Now the Build distributions
    sqlMnt = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like "Build%" AND dtp.parameter_number = ?'''
    dexID = 1 # for the distribution number (row number)
    exID = 1 # for the parameter number (row number x 2-ish)
    for row in buildDists:
        # first p1
        cursorINP.execute(sqlMnt, (tID, sID, exID, round(row.p1,5), cUser, tID, sID, dexID, 1))
        exID += 1
        # now p2
        if row.distType.encode() in ('Weibull','Normal'): # 2-parameter distributions.  uniform isn't allowed in DEMAND Pro
            cursorINP.execute(sqlMnt, (tID, sID, exID, round(row.p2,5), cUser, tID, sID, dexID, 2))
            exID += 1
        dexID += 1
    # Zero distribution (for event distributions with expressions that shouldn't take time)
    sqlZero = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT d.tenant_id, d.simulation_id, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.name like 'Zero' AND dtp.parameter_number = ?'''
    cursorINP.execute(sqlZero, (1, 0, cUser, tID, sID, 1))
print "Done with Distribution Parameter" , datetime.datetime.now().time().isoformat()
###################################### / END DISTRIBUTION PARAMETER
###################################### / BEGIN EXPRESSION
sqlExpressionInsert = '''INSERT INTO input.expression (tenant_id, simulation_id, external_id, expression, name, create_user, create_timestamp) SELECT ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP'''
cursorINP.execute(sqlExpressionInsert, (tID, sID, 1, 'false', 'Never', cUser))
cursorINP.execute(sqlExpressionInsert, (tID, sID, 2, 'state.available && state.active', 'Internally Available & Active', cUser))
cursorINP.execute(sqlExpressionInsert, (tID, sID, 3, '!state.available && state.storageId != null', 'Internally Unavailable & In Storage', cUser))
cursorINP.execute(sqlExpressionInsert, (tID, sID, 4, '!state.available && state.storageId == null', 'Internally Unavailable & Not in Storage', cUser))
###################################### / END EXPRESSION
###################################### / BEGIN EVENT DISTRIBUTION
#### DOES NOT SUPPORT OBJECT GROUP DEFAULTING -- TODO: SUPPORT THIS
#### DOES NOT MULTIPLE COMPLETED REPAIRS -- TODO: SUPPORT THIS
#### DOES NOT SUPPORT DIFFERENT AGE UNITS -- TODO: SUPPORT THIS (DEMAND Pro only allows one age unit per item)

# UNSCHEDULED REMOVAL DEFAULTING OPTIONS:
# > Object Group, Object Class, Object Type
# > Parent Type, all Parents
# > SRAN, all bases

# 1) Type - Type - SRAN
#  ) Class - Type - SRAN - NOT ALLOWED IN DEMAND
#  ) Group - Type - SRAN - NOT ALLOWED IN DEMAND
# 2) Type - All - SRAN
# 3) Type - All - All
#  ) Class - All - SRAN - NOT ALLOWED IN DEMAND
# 4) Class - All - All
#  ) Group - All - SRAN - NOT ALLOWED IN DEMAND
# 5) Group - All - All # Not Supporting Yet TODO: support this way
# 6) Type - Type - All
#  ) Class - Type - All - NOT ALLOWED IN DEMAND
#  ) Group - Type - All - NOT ALLOWED IN DEMAND

if LocalOrWeb == 1: # if it'll be put on the web, set the event distribution name to something cleaner, like 'Failure'
    sqlFED1 = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, parent_object_type_id, location_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, ot.id, pot.id, l.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.location l on l.simulation_id = e.simulation_id JOIN input.object_type pot on pot.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and l.external_id = ? and pot.external_id = ? and d.name like "Failure%" and e.name like "Failure" and exp.external_id = ?'''
    sqlFED2defaultParentType = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, location_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, ot.id, l.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.location l on l.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and l.external_id = ? and d.name like "Failure%" and e.name like "Failure" and exp.external_id = ?'''
    sqlFED3defaultLocationParentType = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, ot.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and d.name like "Failure%" and e.name like "Failure" and exp.external_id = ?'''
    sqlFED4defaultTypeLocationParentType = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_class_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, oc.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_class oc ON oc.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and oc.external_id = ? and d.name like "Failure%" and e.name like "Failure" and exp.external_id = ?'''
    sqlFED5defaultClassLocationParentType = '' ### NO WAY not going to spend the effort to get this working
    sqlFED6defaultLocation = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, parent_object_type_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, ot.id, pot.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.object_type pot on pot.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and pot.external_id = ? and d.name like "Failure%" and e.name like "Failure"'''
else:    
    sqlFED1 = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, parent_object_type_id, location_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, "Failure", d.id, e.id, ot.id, pot.id, l.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.location l on l.simulation_id = e.simulation_id JOIN input.object_type pot on pot.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and l.external_id = ? and pot.external_id = ? and d.name like "Failure%" and e.name like "Failure" and exp.external_id = ?'''
    sqlFED2defaultParentType = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, location_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, "Failure", d.id, e.id, ot.id, l.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.location l on l.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and l.external_id = ? and d.name like "Failure%" and e.name like "Failure" and exp.external_id = ?'''
    sqlFED3defaultLocationParentType = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, "Failure", d.id, e.id, ot.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and d.name like "Failure%" and e.name like "Failure" and exp.external_id = ?'''
    sqlFED4defaultTypeLocationParentType = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_class_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, "Failure", d.id, e.id, oc.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_class oc ON oc.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and oc.external_id = ? and d.name like "Failure%" and e.name like "Failure" and exp.external_id = ?'''
    sqlFED5defaultClassLocationParentType = '' ### NO WAY not going to spend the effort to get this working
    sqlFED6defaultLocation = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, parent_object_type_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, "Failure", d.id, e.id, ot.id, pot.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.object_type pot on pot.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and pot.external_id = ? and d.name like "Failure%" and e.name like "Failure" and exp.external_id = ?'''
    sqlFED2 = '''UPDATE input.event_distribution ed JOIN input.object_type ot ON ot.id = ed.object_type_id
        JOIN input.object_class oc ON oc.id = ot.object_class_id SET ed.object_class_id = oc.id
        WHERE ed.tenant_id = %s AND ed.simulation_id = %s AND ed.name like "Failure"''' % (tID, sID)
    sqlFED3 = '''UPDATE input.event_distribution ed JOIN input.object_type ot ON ot.id = ed.object_type_id
        JOIN input.object_class oc ON oc.id = ot.object_class_id SET ed.object_type_id = NULL
        WHERE ed.tenant_id = %s AND ed.simulation_id = %s AND ed.name like "Failure"''' % (tID, sID)
## Failure distributions
# use that same failure rate pyodbc list
exID = 1 # for external event_distribution id
expExtId = 2 # xpression external id
for row in failureRates:
    if int(str(row.otEid)[-1]) != 0 and row.PetId != -1 and row.lEid != -1: # all specifics 
        cursorINP.execute(sqlFED1, (tID, sID, exID, cUser, tID, sID, exID, row.otEid, row.lEid, row.PetId, expExtId))
    elif int(str(row.otEid)[-1]) != 0 and row.PetId == -1 and row.lEid != -1: # default parent type
        cursorINP.execute(sqlFED2defaultParentType, (tID, sID, exID, cUser, tID, sID, exID, row.otEid, row.lEid, expExtId))
    elif int(str(row.otEid)[-1]) != 0 and row.PetId == -1 and row.lEid == -1: # default parent type and location
        cursorINP.execute(sqlFED3defaultLocationParentType, (tID, sID, exID, cUser, tID, sID, exID, row.otEid, expExtId))
    elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and row.PetId == -1 and row.lEid == -1: # default parent type and location
        cursorINP.execute(sqlFED4defaultTypeLocationParentType, (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), expExtId))
    elif int(str(row.otEid)[-1]) != 0 and row.PetId != -1 and row.lEid == -1: # default parent type and location
        cursorINP.execute(sqlFED6defaultLocation, (tID, sID, exID, cUser, tID, sID, exID, row.otEid, row.PetId, expExtId))
    else:
        print "Warning:  This failure rate is not supported %s %s %s" % (row.otEid, row.PetId, row.lEid)
    exID += 1 # defaults will likely match to several Object types at a time so this external ID will no longer be unique for the failure distribution
if LocalOrWeb == 2: # switch to class info to show up in failure distribution
    cursorINP.execute(sqlFED2)
    cursorINP.execute(sqlFED3)

# SERVER TIME DEFAULTING OPTIONS:
# > Object Group, Object Class, Object Type
# > SRAN, Echelon

# 1) Type - SRAN
# 2) Type - Echelon
# 3) Class - SRAN
# 4) Class - Echelon
# 5) Group - SRAN
#  ) Group - Echelon - NOT ALLOWED IN DEMAND

# define the general Server Time insert queries
sqlSED1 = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, location_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, ot.id, l.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.location l on l.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and l.external_id = ? and d.name like "%s%%" and e.name like "%s" and exp.external_id = ?'''
sqlSED2 = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, location_type_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, ot.id, lt.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id JOIN input.location_type lt on lt.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and lt.external_id = ? and d.name like "%s%%" and e.name like "%s" and exp.external_id = ?'''
sqlSED3 = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_class_id, location_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, oc.id, l.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_class oc ON oc.simulation_id = e.simulation_id JOIN input.location l on l.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and oc.external_id = ? and l.external_id = ? and d.name like "%s%%" and e.name like "%s" and exp.external_id = ?'''
sqlSED4 = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_class_id, location_type_id, expression_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, oc.id, lt.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_class oc ON oc.simulation_id = e.simulation_id JOIN input.location_type lt on lt.simulation_id = e.simulation_id JOIN input.expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and oc.external_id = ? and lt.external_id = ? and d.name like "%s%%" and e.name like "%s" and exp.external_id = ?'''
sqlSED5 = '' # NOT IMPLEMENTING THIS YET
slqSEDevyExpEname = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, expression_id, create_user, create_timestamp) SELECT d.tenant_id, d.simulation_id, ?, e.name, d.id, e.id, exp.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN expression exp on exp.simulation_id = e.simulation_id WHERE e.tenant_id = ? AND e.simulation_id = ? AND exp.external_id = ? AND d.name like "%s" and e.name like "%s"'''


## Repair distributions
#sqlRED = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_type_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, ot.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_type ot ON ot.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and ot.external_id = ? and d.name like "Repair%" and e.name like "Repair"''' # can also include location and parent object type for failure rates, but haven't yet
#sqlREDdefault = '''INSERT INTO event_distribution (tenant_id, simulation_id, external_id, name, distribution_id, event_id, object_class_id, create_user, create_timestamp) SELECT ?, ?, ?, d.name, d.id, e.id, oc.id, ?, CURRENT_TIMESTAMP FROM distribution d JOIN event e on d.simulation_id = e.simulation_id JOIN object_class oc ON oc.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? AND d.external_id = ? and oc.external_id = ? and d.name like "Repair%" and e.name like "Repair"''' # can also include location and parent object type for failure rates, but haven't yet
exID = 1 # for external event_distribution id
expExtId = 1 # xpression external id - false
for row in repairDists:
    if int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) != 0:
        cursorINP.execute(sqlSED1 % ("Repair","Repair"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, row.lEid, expExtId) )
    elif int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) == 0:
        cursorINP.execute(sqlSED2 % ("Repair","Repair"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, int(str(row.lEid)[0]), expExtId))
    elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) != 0:
        cursorINP.execute(sqlSED3 % ("Repair","Repair"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), row.lEid, expExtId))
    elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) == 0:
        cursorINP.execute(sqlSED4 % ("Repair","Repair"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), int(str(row.lEid)[0]), expExtId))
    else:
        print "Warning these Repair server times are not allowed %s %s %s" % (row.otEid, row.lEid, row.mntType)
    exID += 1

## Removal distributions
exID = 1 # for external event_distribution id
expExtId = 1 # xpression external id - false
for row in removalDists:
    if int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) != 0:
        cursorINP.execute(sqlSED1 % ("Removal","Removal"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, row.lEid, expExtId) )
    elif int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) == 0:
        cursorINP.execute(sqlSED2 % ("Removal","Removal"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, int(str(row.lEid)[0]), expExtId))
    elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) != 0:
        cursorINP.execute(sqlSED3 % ("Removal","Removal"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), row.lEid, expExtId))
    elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) == 0:
        cursorINP.execute(sqlSED4 % ("Removal","Removal"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), int(str(row.lEid)[0]), expExtId))
    else:
        print "Warning these Removal server times are not allowed %s %s %s" % (row.otEid, row.lEid, row.mntType)
    exID += 1

## Install distributions
exID = 1 # for external event_distribution id
expExtId = 1 # xpression external id - false
for row in removalDists:
    if int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) != 0:
        cursorINP.execute(sqlSED1 % ("Installation","Installation"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, row.lEid, expExtId) )
    elif int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) == 0:
        cursorINP.execute(sqlSED2 % ("Installation","Installation"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, int(str(row.lEid)[0]), expExtId))
    elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) != 0:
        cursorINP.execute(sqlSED3 % ("Installation","Installation"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), row.lEid, expExtId))
    elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) == 0:
        cursorINP.execute(sqlSED4 % ("Installation","Installation"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), int(str(row.lEid)[0]), expExtId))
    else:
        print "Warning these Installation server times are not allowed %s %s %s" % (row.otEid, row.lEid, row.mntType)
    exID += 1

if SimOrDemo == 1:
    ## Inspection distributions
    exID = 1 # for external event_distribution id
    expExtId = 3 # xpression external id - false
    for row in inspectDists:
        if int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) != 0:
            cursorINP.execute(sqlSED1 % ("Inspection","Inspection"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, row.lEid, expExtId) )
        elif int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) == 0:
            cursorINP.execute(sqlSED2 % ("Inspection","Inspection"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, int(str(row.lEid)[0]), expExtId))
        elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) != 0:
            cursorINP.execute(sqlSED3 % ("Inspection","Inspection"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), row.lEid, expExtId))
        elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) == 0:
            cursorINP.execute(sqlSED4 % ("Inspection","Inspection"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), int(str(row.lEid)[0]), expExtId))
        else:
            print "Warning these Inspection server times are not allowed %s %s %s" % (row.otEid, row.lEid, row.mntType)
        exID += 1

    ## Build distributions
    exID = 1 # for external event_distribution id
    expExtId = 1 # xpression external id - false
    for row in buildDists:
        if int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) != 0:
            cursorINP.execute(sqlSED1 % ("Build","Build"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, row.lEid, expExtId))
        elif int(str(row.otEid)[-1]) != 0 and int(str(row.lEid)[-1]) == 0:
            cursorINP.execute(sqlSED2 % ("Build","Build"), (tID, sID, exID, cUser, tID, sID, exID, row.otEid, int(str(row.lEid)[0]), expExtId))
        elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) != 0:
            cursorINP.execute(sqlSED3 % ("Build","Build"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), row.lEid, expExtId))
        elif int(str(row.otEid)[4:]) != 0 and int(str(row.otEid)[-1]) == 0 and int(str(row.lEid)[-1]) == 0:
            cursorINP.execute(sqlSED4 % ("Build","Build"), (tID, sID, exID, cUser, tID, sID, exID, int(str(row.otEid)[:6]), int(str(row.lEid)[0]), expExtId))
        else:
            print "Warning these Build server times are not allowed %s %s %s" % (row.otEid, row.lEid, row.mntType)
        exID += 1

    ## Inspection distributions
    exID = 1
    # parent needs inspection event :: distribution time of zero
    cursorINP.execute(slqSEDevyExpEname % ('Zero','Parent Needs Inspection'),  (1, cUser, tID,sID, 4))

print "Done with Event Distribution" , datetime.datetime.now().time().isoformat()
###################################### / END EVENT DISTRIBUTION

############################################################################ / BEGIN CURRENCY
###################################### / BEGIN CURRENCY VALUE CLASS
SQLcurclass1 = '''INSERT INTO currency_value_class (tenant_id, simulation_id, name, create_user, create_timestamp) VALUES (%s, %s, "Revenue", "%s", CURRENT_TIMESTAMP)''' % (tID, sID, cUser)
SQLcurclass2 = '''INSERT INTO currency_value_class (tenant_id, simulation_id, name, create_user, create_timestamp) VALUES (%s, %s, "Cost", "%s", CURRENT_TIMESTAMP)''' % (tID, sID, cUser)
cursorINP.execute(SQLcurclass1) # currency value class - revenue
cursorINP.execute(SQLcurclass2) # currency value class - cost
###################################### / END CURRENCY VALUE CLASS
###################################### / BEGIN CURRENCY VALUE TYPE
SQLcurtype = '''INSERT INTO currency_value_type (tenant_id, simulation_id, external_id, name, create_user, create_timestamp) VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)'''
for aName in enumerate(('Acquisition','Repair','Shipment','Storage'),start=1):
    cursorINP.execute(SQLcurtype, (tID, sID, aName[0], aName[1], cUser))
###################################### / END CURRENCY VALUE TYPE
############################################################################ / END CURRENCY

###################################### / BEGIN STRUCTURE
# FROM JESSICA W with adjustments to simplify
# parent structures (objects with at least one child)
sqlPS ='''INSERT into input.structure
        ( name, internal_name, minimum_count, object_id, tenant_id
        , simulation_id, create_user, create_timestamp)
    SELECT concat(obj.name,' Parent'), concat(obj.internal_name,'Parent')
        , 0, obj.id, %s, %s, '%s', CURRENT_TIMESTAMP 
    FROM input.object obj
    where obj.id in (
        select distinct(parent_object_id) from input.object
        where tenant_id=%s and simulation_id=%s
        and parent_object_id is not null
        )''' % (tID, sID, cUser, tID, sID)
cursorINP.execute(sqlPS)

# min_count
sqlMC='''UPDATE input.structure s
    JOIN (
        SELECT str.id, count(obj.id) n 
        FROM input.structure str 
        JOIN input.object obj 
        on str.object_id=obj.parent_object_id 
        WHERE str.tenant_id=%s AND str.simulation_id=%s
        GROUP BY str.object_id
        ) num
    on s.id=num.id
    SET s.minimum_count = num.n
    WHERE s.tenant_id=%s AND s.simulation_id=%s''' % (tID, sID, tID, sID)
cursorINP.execute(sqlMC)

#child structures
#sqlCS='''INSERT INTO input.structure
#        ( name, internal_name, minimum_count, parent_structure_id
#        , tenant_id, simulation_id, create_user, create_timestamp)
#    SELECT concat(obj.name,' Child'), concat(obj.internal_name,'Child')
#        , 1, str.id, %s, %s, '%s', CURRENT_TIMESTAMP
#    FROM input.object obj
#    JOIN input.structure str
#    on str.object_id = obj.parent_object_id
#    WHERE str.tenant_id=%s AND str.simulation_id=%s''' % (tID, sID, cUser, tID, sID)
#cursorINP.execute(sqlCS)

#structure_object - connect children objects to their children (not top-level objects, e.g. assets)
# the structures of interest are child structures whose names end with ' Child'
# sqlSO='''INSERT INTO input.structure_object
#         ( object_id, structure_id, tenant_id
#         , simulation_id, create_user, create_timestamp)
#     SELECT o.id, s.id, %s, %s, '%s', CURRENT_TIMESTAMP 
#     FROM input.object o 
#     JOIN input.structure s 
#     on left(s.name,length(s.name)-6)=o.name AND o.simulation_id = s.simulation_id
#     WHERE s.object_id is null 
#     AND o.parent_object_id is not null 
#     AND right(s.name,5) = 'Child'
#     AND s.tenant_id=%s and s.simulation_id=%s;''' % (tID, sID, cUser, tID, sID)
# cursorINP.execute(sqlSO)

# structure object (no parent/child structure relationships)
sqlSO='''INSERT INTO input.structure_object
         ( object_id, structure_id, tenant_id
         , simulation_id, create_user, create_timestamp)
     SELECT co.id, s.id, %s, %s, '%s', CURRENT_TIMESTAMP 
     FROM input.object po 
     JOIN input.structure s 
     on s.object_id=po.id AND po.simulation_id = s.simulation_id
     JOIN input.object co on co.parent_object_id = po.id
     AND s.tenant_id=%s and s.simulation_id=%s''' % (tID, sID, cUser, tID, sID)
cursorINP.execute(sqlSO)

print 'Done with Structure and Structure_Object' , datetime.datetime.now().time().isoformat()
###################################### / END STRUCTURE

###################################### / BEGIN STORAGE TYPE
# Types are hardcoded to 'MaintenanceAndStorage'
sqlST = '''INSERT INTO storage_type (tenant_id, simulation_id, name, internal_name, external_id, create_user, create_timestamp) values (?, ?, ?, ?, ?, ?, ?)''' # just one class right now
cursorINP.execute(sqlST, (tID, sID, 'Maintenance and Storage', 'MaintenanceAndStorage', 1, cUser, datetime.datetime.now().isoformat()))
print "Done with Storage Type" , datetime.datetime.now().time().isoformat()
###################################### / END STORAGE TYPE
###################################### / BEGIN STORAGE
# Each location has one storage
sqlS = '''INSERT INTO input.storage (tenant_id, simulation_id, name, internal_name, location_id, external_id, storage_type_id, default_for_new_buys, create_user, create_timestamp)
select ?, ?, l.name, l.internal_name, l.id, l.external_id, st.id, 0, ?, current_timestamp() FROM location l join storage_type st on st.simulation_id=l.simulation_id where st.external_id = ? and l.tenant_id = ? and l.simulation_id = ?'''
# query to paste into source input
cursorINP.execute(sqlS, (tID, sID, cUser, 1, tID, sID))
print "Done with Storage" , datetime.datetime.now().time().isoformat()
# Add spares to storages
sqlS = '''UPDATE input.object o join input.storage s on s.location_id = o.location_id set o.storage_id = s.id where  asset = 0 and s.tenant_id = ? and s.simulation_id = ? and o.name not like "P%"''' # o.parent_object_id is null and DO I NEED THIS?
cursorINP.execute(sqlS, (tID, sID))
print "Done with Current Inventory" , datetime.datetime.now().time().isoformat()
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
# can't do an executemany (yet) because the many tables have to reference eachother by external_id - probably a way to simplify it (create the integer steps first and then update python rows? might require looping through anyway)
sqlFIE = '''INSERT INTO input.event (tenant_id, simulation_id, external_id, name, event_type_id, create_user, create_timestamp) VALUES (?,?,?,?,?,?,CURRENT_TIMESTAMP)'''
sqlES = '''INSERT INTO input.event_schedule (tenant_id, simulation_id, external_id, name, timestamp_value, event_id, create_user, create_timestamp)
    SELECT ?, ?, ?, e.name, ?, e.id, ?, CURRENT_TIMESTAMP FROM input.event e WHERE e.event_type_id = ? AND e.external_id = ? AND e.tenant_id = ? AND e.simulation_id = ?'''
sqlSES = '''INSERT INTO input.storage_event_schedule (tenant_id, simulation_id, external_id, event_schedule_id, storage_id, location_id, create_user, create_timestamp)
SELECT ?, ?, ?, es.id, s.id, l.id, ?, CURRENT_TIMESTAMP FROM input.event_schedule es JOIN input.storage s on s.simulation_id = es.simulation_id JOIN input.location l on l.id = s.location_id and l.simulation_id = s.simulation_id WHERE l.external_id = ? AND es.external_id = ? AND es.tenant_id = ? AND es.simulation_id = ?'''
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
    cursorINP.execute(sqlSES, (tID, sID, exID ,cUser, row.lEid, exID, tID, sID))
    cursorINP.execute(sqlEP1, (tID, sID, exID, cUser, acquisitionEtId, exID, tID, sID, 1, row.otEid))
    cursorINP.execute(sqlEP2, (tID, sID, exID, int(row.num2acq), cUser, acquisitionEtId, exID, tID, sID, 2, row.otEid))
    exID += 1
print "Done with Future Inventory" , datetime.datetime.now().time().isoformat()
###################################### / END FUTURE INVENTORY

###################################### / BEGIN OPTEMPO
sqlFP = '''SELECT [*Analysis Control Panel].Value FROM [*Analysis Control Panel] WHERE ((([*Analysis Control Panel].[Analysis Mode])='Flying Program'))'''
curSource.execute(sqlFP)
FP = curSource.fetchone()[0].encode()
sqlOpProfNoTime = '''SELECT [*Operation profiles].[Platform type] AS pType, [*Operation profiles].Base, [*Operation profiles].Year, [*Operation profiles].Qtr, [*Operation profiles].[Rfh/t Dist] AS FHRdist, [*Operation profiles].[Rfh/t P1] AS FHRp1, [*Operation profiles].[Rfh/t P2] AS FHRp2, [*Operation profiles].[Rtac/fh Dist] AS TACdist, [*Operation profiles].[Rtac/fh P1] AS TACp1, [*Operation profiles].[Rtac/fh P2] AS TACp2, [*Operation profiles].[Reot/fh Dist] AS EOTdist, [*Operation profiles].[Reot/fh P1] AS EOTp1, [*Operation profiles].[Reot/fh P2] AS EOTp2, [*Operation profiles].[Rwow/fh Dist] AS WOWdist, [*Operation profiles].[Rwow/fh P1] AS WOWp1, [*Operation profiles].[Rwow/fh P2] AS WOWp2
FROM [*Operation profiles] WHERE ((([*Operation profiles].Year)=-1) AND (([*Operation profiles].Qtr)=-1))'''
sqlOpProfSpecYearOnly = '''SELECT [*Operation profiles].[Platform type] AS pType, [*Operation profiles].Base, [*Operation profiles].Year, [*Operation profiles].Qtr, [*Operation profiles].[Rfh/t Dist] AS FHRdist, [*Operation profiles].[Rfh/t P1] AS FHRp1, [*Operation profiles].[Rfh/t P2] AS FHRp2, [*Operation profiles].[Rtac/fh Dist] AS TACdist, [*Operation profiles].[Rtac/fh P1] AS TACp1, [*Operation profiles].[Rtac/fh P2] AS TACp2, [*Operation profiles].[Reot/fh Dist] AS EOTdist, [*Operation profiles].[Reot/fh P1] AS EOTp1, [*Operation profiles].[Reot/fh P2] AS EOTp2, [*Operation profiles].[Rwow/fh Dist] AS WOWdist, [*Operation profiles].[Rwow/fh P1] AS WOWp1, [*Operation profiles].[Rwow/fh P2] AS WOWp2, [*Weeks and Dates].Date
FROM [*Operation profiles] INNER JOIN [*Weeks and Dates] ON [*Operation profiles].Year = [*Weeks and Dates].Year
WHERE ((([*Operation profiles].Qtr)=-1) AND (([*Weeks and Dates].Week)=1))'''
sqlOpProfSpecYearQtr = '''SELECT [*Operation profiles].[Platform type] AS pType, [*Operation profiles].Base, [*Operation profiles].Year, [*Operation profiles].Qtr, [*Operation profiles].[Rfh/t Dist] AS FHRdist, [*Operation profiles].[Rfh/t P1] AS FHRp1, [*Operation profiles].[Rfh/t P2] AS FHRp2, [*Operation profiles].[Rtac/fh Dist] AS TACdist, [*Operation profiles].[Rtac/fh P1] AS TACp1, [*Operation profiles].[Rtac/fh P2] AS TACp2, [*Operation profiles].[Reot/fh Dist] AS EOTdist, [*Operation profiles].[Reot/fh P1] AS EOTp1, [*Operation profiles].[Reot/fh P2] AS EOTp2, [*Operation profiles].[Rwow/fh Dist] AS WOWdist, [*Operation profiles].[Rwow/fh P1] AS WOWp1, [*Operation profiles].[Rwow/fh P2] AS WOWp2, [*Weeks and Dates].Date
FROM [*Operation profiles] INNER JOIN [*Weeks and Dates] ON ([*Operation profiles].Qtr = [*Weeks and Dates].Quarter) AND ([*Operation profiles].Year = [*Weeks and Dates].Year)
WHERE ((([*Weeks and Dates].Week)=1))'''
curSource.execute(sqlOpProfNoTime)
OpProfsNoTime = curSource.fetchall()
curSource.execute(sqlOpProfSpecYearOnly)
OpProfsYrOnly = curSource.fetchall()
curSource.execute(sqlOpProfSpecYearQtr)
OpProfsYrQtr = curSource.fetchall()
#dateInfo.Date.isoformat()
# need to be concerned about General optempo overwriting Specific optempo for simulation
# TODO: handle the other optempos (tac, eot, wow)

################################################# THE OPTEMPO CONSISTENCY CHECKS ARE A TOTAL BEAR SO I'M LEAVING THIS OUT #############################################################################
################################################# # # # I WILL CREATE THE ENTRIES MANUALLY WITH THE SCENARIO EDITOR # # # #############################################################################

# if SimOrDemo ==2: # insert optempo information if it's a demo so it shows up in the Vary Operations Scenario Editor
#     sqlInOpProfES = '''INSERT INTO input.event_schedule (tenant_id, simulation_id, external_id, name, timestamp_value, event_id, distribution_id, create_user, create_timestamp) SELECT ?, ?, ?, 'Operations', ?, e.id, d.id, ?, CURRENT_TIMESTAMP FROM input.event e JOIN input.distribution d ON d.simulation_id = e.simulation_id WHERE e.tenant_id = ? and e.simulation_id = ? and e.name like "Operations" and d.name like "Operations" and d.external_id = ?''' 
#     sqlInOpProfOESGenTypeGenBase = '''INSERT INTO input.object_event_schedule (tenant_id, simulation_id, external_id, event_schedule_id, object_group_id, create_user, create_timestamp) SELECT ?, ?, ?, es.id, og.id, ?, CURRENT_TIMESTAMP FROM input.event_schedule es JOIN input.object_group og on og.simulation_id = es.simulation_id WHERE og.tenant_id = ? AND og.simulation_id = ? AND es.external_id = ? AND og.name = "Asset" AND es.name like "Operations"'''
#     sqlInOpProfOESSpecTypeGenBase = '''INSERT INTO input.object_event_schedule (tenant_id, simulation_id, external_id, event_schedule_id, object_type_id, create_user, create_timestamp) SELECT ?, ?, ?, es.id, ot.id, ?, CURRENT_TIMESTAMP FROM input.event_schedule es JOIN input.object_type ot on ot.simulation_id = es.simulation_id WHERE ot.tenant_id = ? AND ot.simulation_id = ? AND es.external_id = ? AND ot.external_id = ? AND es.name like "Operations"'''
#     sqlInOpProfOESGenTypeSpecBase = '''INSERT INTO input.object_event_schedule (tenant_id, simulation_id, external_id, event_schedule_id, object_group_id, location_id, create_user, create_timestamp) SELECT ?, ?, ?, es.id, og.id, l.id, ?, CURRENT_TIMESTAMP FROM input.event_schedule es JOIN input.object_group og on og.simulation_id = es.simulation_id JOIN input.location l ON l.simulation_id = es.simulation_id WHERE og.tenant_id = ? AND og.simulation_id = ? AND es.external_id = ? AND og.name = "Asset" AND l.external_id = ? AND es.name like "Operations"'''
#     sqlInOpProfOESSpecTypeSpecBase = '''INSERT INTO input.object_event_schedule (tenant_id, simulation_id, external_id, event_schedule_id, object_type_id, location_id, create_user, create_timestamp) SELECT ?, ?, ?, es.id, ot.id, l.id, ?, CURRENT_TIMESTAMP FROM input.event_schedule es JOIN input.object_type ot on ot.simulation_id = es.simulation_id JOIN input.location l ON l.simulation_id = es.simulation_id WHERE ot.tenant_id = ? AND ot.simulation_id = ? AND es.external_id = ? AND ot.external_id = ? AND l.external_id = ? AND es.name like "Operations"'''

#     sqlOperationsDist = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) 
#         SELECT ?, ?, ?, ?, dc.id, dt.id, uom.id, ?, CURRENT_TIMESTAMP FROM lcm.unit_of_measure uom, distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = ? and dc.simulation_id = ? and dt.name = ? AND dc.name like "Operation Profile" AND uom.name like "Operating Hours" AND uom.tenant_id = ?'''
#     sqlOperationsDistParam = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like "Operations%" AND dtp.parameter_number = ?'''

#     exID = 1
#     for row in OpProfsYrQtr:
#         cursorINP.execute(sqlOperationsDist, (tID, sID, exID, "Operations", cUser, tID, sID, row.FHRdist, tID)) # distribution
#         cursorINP.execute(sqlOperationsDistParam, (tID, sID, exID, row.FHRp1, cUser, tID, sID, exID, 1)) # distribution parameter
#         if row.FHRdist in ("Weibull","Normal", "Uniform"): # require two parameters
#             cursorINP.execute(sqlOperationsDistParam, (tID, sID, exID, row.FHRp2, cUser, tID, sID, exID, 2)) # distribution parameter
#         cursorINP.execute(sqlInOpProfES, (tID, sID, exID, row.Date, cUser, tID, sID, exID))
#         # include the object_event_schedule
#         if row.Base == -1 and row.pType == -1: # general general
#             cursorINP.execute(sqlInOpProfOESGenTypeGenBase, (tID, sID, exID, cUser, tID, sID, exID))
#         elif row.Base == -1 and row.pType != -1: # general specific
#             cursorINP.execute(sqlInOpProfOESSpecTypeGenBase, (tID, sID, exID, cUser, tID, sID, exID, row.pType))
#         elif row.Base != -1 and row.pType == -1: # specific general
#             cursorINP.execute(sqlInOpProfOESGenTypeSpecBase, (tID, sID, exID, cUser, tID, sID, exID, row.Base))
#         else:
#             cursorINP.execute(sqlInOpProfOESSpecTypeSpecBase, (tID, sID, exID, cUser, tID, sID, exID, row.pType, row.Base))
#         exID += 1
#     for row in OpProfsYrOnly: # need to enter a row for each quarter - the op tempo rows are duplicated four times in this object to iterate through
#         cursorINP.execute(sqlOperationsDist, (tID, sID, exID, "Operations", cUser, tID, sID, row.FHRdist, tID)) # distribution
#         cursorINP.execute(sqlOperationsDistParam, (tID, sID, exID, row.FHRp1, cUser, tID, sID, exID, 1)) # distribution parameter
#         if row.FHRdist in ("Weibull","Normal", "Uniform"): # require two parameters
#             cursorINP.execute(sqlOperationsDistParam, (tID, sID, exID, row.FHRp2, cUser, tID, sID, exID, 2)) # distribution parameter
#         cursorINP.execute(sqlInOpProfES, (tID, sID, exID, row.Date, cUser, tID, sID, exID))
#         # include the object_event_schedule
#         if row.Base == -1 and row.pType == -1: # general general
#             cursorINP.execute(sqlInOpProfOESGenTypeGenBase, (tID, sID, exID, cUser, tID, sID, exID))
#         elif row.Base == -1 and row.pType != -1: # general specific
#             cursorINP.execute(sqlInOpProfOESSpecTypeGenBase, (tID, sID, exID, cUser, tID, sID, exID, row.pType))
#         elif row.Base != -1 and row.pType == -1: # specific general
#             cursorINP.execute(sqlInOpProfOESGenTypeSpecBase, (tID, sID, exID, cUser, tID, sID, exID, row.Base))
#         else:
#             cursorINP.execute(sqlInOpProfOESSpecTypeSpecBase, (tID, sID, exID, cUser, tID, sID, exID, row.pType, row.Base))
#         exID += 1
#     for row in OpProfsNoTime:
#         # this one should b 1) at the start of the simulation if there's no match in a specific year/qtr and 2) after each specific year/qtr match that isn't followed by another specific year/qtr match.
#         # for now I'm just adding all these to the start of the simulation
#         # there also may be duplicates between platform type and sran due to defaulting mechanisms 
#         cursorINP.execute(sqlOperationsDist, (tID, sID, exID, "Operations", cUser, tID, sID, row.FHRdist, tID)) # distribution
#         cursorINP.execute(sqlOperationsDistParam, (tID, sID, exID, row.FHRp1, cUser, tID, sID, exID, 1)) # distribution parameter
#         if row.FHRdist in ("Weibull","Normal", "Uniform"): # require two parameters
#             cursorINP.execute(sqlOperationsDistParam, (tID, sID, exID, row.FHRp2, cUser, tID, sID, exID, 2)) # distribution parameter
#         cursorINP.execute(sqlInOpProfES, (tID, sID, exID, dateInfo.Date.isoformat(), cUser, tID, sID, exID)) # use start date because these op profiles have no date information
#         # include the object_event_schedule
#         if row.Base == -1 and row.pType == -1: # general general
#             cursorINP.execute(sqlInOpProfOESGenTypeGenBase, (tID, sID, exID, cUser, tID, sID, exID))
#         elif row.Base == -1 and row.pType != -1: # general specific
#             cursorINP.execute(sqlInOpProfOESSpecTypeGenBase, (tID, sID, exID, cUser, tID, sID, exID, row.pType))
#         elif row.Base != -1 and row.pType == -1: # specific general
#             cursorINP.execute(sqlInOpProfOESGenTypeSpecBase, (tID, sID, exID, cUser, tID, sID, exID, row.Base))
#         else:
#             cursorINP.execute(sqlInOpProfOESSpecTypeSpecBase, (tID, sID, exID, cUser, tID, sID, exID, row.pType, row.Base))
#         exID += 1

#     # there is a consistency check to test whether the object event schedule connects to a revenue.  this will prevent cloning and re-baselining, so I need to enter values of zero
#     SQLcurclass1 = '''INSERT INTO currency_value_class (tenant_id, simulation_id, name, create_user, create_timestamp) VALUES (%s, %s, "Revenue", "%s", CURRENT_TIMESTAMP)''' % (tID, sID, cUser)
#     SQLcurclass2 = '''INSERT INTO currency_value_class (tenant_id, simulation_id, name, create_user, create_timestamp) VALUES (%s, %s, "Cost", "%s", CURRENT_TIMESTAMP)''' % (tID, sID, cUser)
#     SQLcurtype = '''INSERT INTO currency_value_type (tenant_id, simulation_id, name, create_user, create_timestamp) VALUES (%s, %s, "Operations Default", "%s", CURRENT_TIMESTAMP)''' % (tID, sID, cUser)
#     SQLcur = '''INSERT INTO currency_value (tenant_id, simulation_id, name, currency_value, unit_of_measure_id, start_timestamp, object_id, object_type_id, object_class_id, object_group_id, location_id, location_type_id, currency_value_type_id, currency_value_class_id, create_user, create_timestamp) SELECT %s, %s, concat("Operations Default ",oes.id), 0, uom.id, es.timestamp_value, oes.object_id, oes.object_type_id, oes.object_class_id, oes.object_group_id, oes.location_id, oes.location_type_id, rt.id, rc.id, "%s", CURRENT_TIMESTAMP FROM object_event_schedule oes JOIN event_schedule es on es.id = oes.event_schedule_id AND es.simulation_id = oes.simulation_id JOIN currency_value_class rc on rc.simulation_id = oes.simulation_id JOIN currency_value_type rt on rt.simulation_id = oes.simulation_id JOIN lcm.unit_of_measure uom ON uom.tenant_id = oes.tenant_id WHERE uom.name like "Dollars" AND oes.tenant_id = %s AND oes.simulation_id = %s AND uom.external_id = 6 AND rv.name = "Operations Default" AND rc.name = "Revenue"''' % (tID, sID, cUser, tID, sID)
#     cursorINP.execute(SQLcurclass1) # currency value class - revenue
#     cursorINP.execute(SQLcurclass2) # currency value class - cost
#     cursorINP.execute(SQLcurtype) # currency value type
#     cursorINP.execute(SQLcur) # currencies
# TODO: make optempo better - needs to assign maximum one optempo per object
###################################### / END OPTEMPO

###################################### / BEGIN SHIPEMENT TIME DISTRIBUTION
# This method for shipping times, and base structure (route) handles defaults for Maintenance Levels (Location Type)
# Bases should not match twice, however they may match both to a default (location type) and a specific case - resupply table will sort this out for display in the scenario editor
# This table is actually pretty useless because it isn't used in a scenario editor
sqlDshiptimeERROR = '''SELECT [*Shipment times].[Tsf Dist] as DistType, [*Shipment times].[From SRAN] as Fsran, [*Shipment times].[To SRAN] as Tsran, [*Shipment times].[Tsf P1] as p1, [*Shipment times].[Tsf P2] as p2
FROM [*Shipment times] WHERE ((([*Shipment times].[Tsf Dist]) like 'lognormal'))'''
sqlDshiptimeSpecSpec = '''SELECT [*Shipment times].[Tsf Dist] as DistType, [*Shipment times].[From SRAN] as Fsran, [*Shipment times].[To SRAN] as Tsran, [*Shipment times].[Tsf P1] as p1, [*Shipment times].[Tsf P2] as p2, Left([From SRAN],1) AS fltEid, Left([To SRAN],1) AS tltEid
FROM [*Shipment times] WHERE ((([*Shipment times].[Tsf Dist]) Not Like 'lognormal') AND ((Right([From SRAN],4))<>0) AND ((Right([To SRAN],4))<>0))'''
sqlDshiptimeSpecGen ='''SELECT [*Shipment times].[Tsf Dist] as DistType, [*Shipment times].[From SRAN] as Fsran, [*Shipment times].[To SRAN] as Tsran, [*Shipment times].[Tsf P1] as p1, [*Shipment times].[Tsf P2] as p2, Left([From SRAN],1) AS fltEid, Left([To SRAN],1) AS tltEid
FROM [*Shipment times] WHERE ((([*Shipment times].[Tsf Dist]) Not Like 'lognormal') AND ((Right([From SRAN],4))<>0) AND ((Right([To SRAN],4))=0))'''
sqlDshiptimeGenSpec ='''SELECT [*Shipment times].[Tsf Dist] as DistType, [*Shipment times].[From SRAN] as Fsran, [*Shipment times].[To SRAN] as Tsran, [*Shipment times].[Tsf P1] as p1, [*Shipment times].[Tsf P2] as p2, Left([From SRAN],1) AS fltEid, Left([To SRAN],1) AS tltEid
FROM [*Shipment times] WHERE ((([*Shipment times].[Tsf Dist]) Not Like 'lognormal') AND ((Right([From SRAN],4))=0) AND ((Right([To SRAN],4))<>0))'''
sqlDshiptimeGenGen ='''SELECT [*Shipment times].[Tsf Dist] as DistType, [*Shipment times].[From SRAN] as Fsran, [*Shipment times].[To SRAN] as Tsran, [*Shipment times].[Tsf P1] as p1, [*Shipment times].[Tsf P2] as p2, Left([From SRAN],1) AS fltEid, Left([To SRAN],1) AS tltEid
FROM [*Shipment times] WHERE ((([*Shipment times].[Tsf Dist]) Not Like 'lognormal') AND ((Right([From SRAN],4))=0) AND ((Right([To SRAN],4))=0))'''

sqlFshipDist = '''INSERT INTO distribution (tenant_id, simulation_id, external_id, name, distribution_class_id, distribution_type_id, unit_of_measure_id, create_user, create_timestamp) 
    SELECT ?, ?, ?, ?, dc.id, dt.id, uom.id, ?, CURRENT_TIMESTAMP FROM lcm.unit_of_measure uom, distribution_class dc join distribution_type dt on dt.simulation_id = dc.simulation_id WHERE dc.tenant_id = ? and dc.simulation_id = ? and dt.name = ? AND dc.name like "Shipment Time" AND uom.name like "Days" AND uom.tenant_id = ?'''
sqlFshipDistParam = '''INSERT INTO distribution_parameter (tenant_id, simulation_id, external_id, parameter_value, distribution_id, distribution_type_parameter_id, create_user, create_timestamp) SELECT ?, ?, ?, ?, d.id, dtp.id, ?, CURRENT_TIMESTAMP FROM distribution_type dt JOIN distribution d on dt.simulation_id = d.simulation_id AND dt.id = d.distribution_type_id JOIN distribution_type_parameter dtp on dtp.simulation_id = d.simulation_id AND dtp.distribution_type_id = dt.id WHERE d.tenant_id = ? AND d.simulation_id = ? AND d.external_id = ? AND d.name like 'Shipment%' AND dtp.parameter_number = ?'''

sqlFshipSpecSpec = '''INSERT INTO shipment_time_distribution (tenant_id, simulation_id, external_id, distribution_id, from_location_id, to_location_id, create_user, create_timestamp) 
    SELECT ?, ?, ?, d.id, fl.id, tl.id, ?, CURRENT_TIMESTAMP FROM input.distribution d JOIN input.location fl ON d.simulation_id = fl.simulation_id JOIN input.location tl on fl.simulation_id = tl.simulation_id WHERE fl.external_id = ? AND tl.external_id = ? AND fl.tenant_id = ? AND fl.simulation_id = ? AND d.external_id = ? AND d.name like "Shipment%"'''
sqlFshipSpecGen = '''INSERT INTO shipment_time_distribution (tenant_id, simulation_id, external_id, distribution_id, from_location_id, to_location_type_id, create_user, create_timestamp) 
    SELECT ?, ?, ?, d.id, fl.id, tlt.id, ?, CURRENT_TIMESTAMP FROM input.distribution d JOIN input.location fl ON d.simulation_id = fl.simulation_id JOIN input.location_type tlt on fl.simulation_id = tlt.simulation_id WHERE fl.external_id = ? AND tlt.external_id = ? AND fl.tenant_id = ? AND fl.simulation_id = ? AND d.external_id = ? AND d.name like "Shipment%"'''
sqlFshipGenSpec = '''INSERT INTO shipment_time_distribution (tenant_id, simulation_id, external_id, distribution_id, from_location_type_id, to_location_id, create_user, create_timestamp) 
    SELECT ?, ?, ?, d.id, flt.id, tl.id, ?, CURRENT_TIMESTAMP FROM input.distribution d JOIN input.location_type flt ON d.simulation_id = flt.simulation_id JOIN input.location tl on flt.simulation_id = tl.simulation_id WHERE flt.external_id = ? AND tl.external_id = ? AND flt.tenant_id = ? AND flt.simulation_id = ? AND d.external_id = ? AND d.name like "Shipment%"'''
sqlFshipGenGen = '''INSERT INTO shipment_time_distribution (tenant_id, simulation_id, external_id, distribution_id, from_location_type_id, to_location_type_id, create_user, create_timestamp) 
    SELECT ?, ?, ?, d.id, flt.id, tlt.id, ?, CURRENT_TIMESTAMP FROM input.distribution d JOIN input.location_type flt ON d.simulation_id = flt.simulation_id JOIN input.location_type tlt on flt.simulation_id = tlt.simulation_id WHERE flt.external_id = ? AND tlt.external_id = ? AND flt.tenant_id = ? AND flt.simulation_id = ? AND d.external_id = ? AND d.name like "Shipment%"'''

curSource.execute(sqlDshiptimeERROR)
if curSource.rowcount > 0:
    lognormalShips = curSource.fetchall()
    print 'Lognormal distributions are not allowed.  These shipping times will be ignored.  This may lead to routes without shipping times, and thus that cannot exist.'
    print 'These shipping time distributions are ignored'
    for row in lognormalShips:
        print 'From Base: ' + row.FRbase + ' To Base: ' + row.TObase
# Loop through four sets of distributions based on the specificity of the SRAN - either a specific location or a level (like depot) that equals a location_type in lcm
exID = 1
# Start with specific & specific
curSource.execute(sqlDshiptimeSpecSpec)
shipDs = curSource.fetchall()
for row in shipDs:
    cursorINP.execute(sqlFshipDist, (tID, sID, exID, "Shipment", cUser, tID, sID, row.DistType, tID)) # distribution
    cursorINP.execute(sqlFshipDistParam, (tID, sID, exID, row.p1, cUser, tID, sID, exID, 1)) # distribution parameter
    if row.DistType in ("Weibull","Normal", "Uniform"): # require two parameters
        cursorINP.execute(sqlFshipDistParam, (tID, sID, exID, row.p2, cUser, tID, sID, exID, 2)) # distribution parameter
    cursorINP.execute(sqlFshipSpecSpec, (tID, sID, exID, cUser, row.Fsran, row.Tsran, tID, sID, exID)) # shipment time distribution
    exID += 1
# Next do specific & general
curSource.execute(sqlDshiptimeSpecGen)
shipDs = curSource.fetchall()
for row in shipDs:
    cursorINP.execute(sqlFshipDist, (tID, sID, exID, "Shipment", cUser, tID, sID, row.DistType, tID)) # distribution
    cursorINP.execute(sqlFshipDistParam, (tID, sID, exID, row.p1, cUser, tID, sID, exID, 1)) # distribution parameter
    if row.DistType in ("Weibull","Normal", "Uniform"): # require two parameters
        cursorINP.execute(sqlFshipDistParam, (tID, sID, exID, row.p2, cUser, tID, sID, exID, 2)) # distribution parameter
    cursorINP.execute(sqlFshipSpecGen, (tID, sID, exID, cUser, row.Fsran, row.tltEid, tID, sID, exID)) # shipment time distribution - note location type id as leftmost digit in SRAN
    exID += 1
# Next do general & specific
curSource.execute(sqlDshiptimeGenSpec)
shipDs = curSource.fetchall()
for row in shipDs:
    cursorINP.execute(sqlFshipDist, (tID, sID, exID, "Shipment", cUser, tID, sID, row.DistType, tID)) # distribution
    cursorINP.execute(sqlFshipDistParam, (tID, sID, exID, row.p1, cUser, tID, sID, exID, 1)) # distribution parameter
    if row.DistType in ("Weibull","Normal", "Uniform"): # require two parameters
        cursorINP.execute(sqlFshipDistParam, (tID, sID, exID, row.p2, cUser, tID, sID, exID, 2)) # distribution parameter
    cursorINP.execute(sqlFshipGenSpec, (tID, sID, exID, cUser, row.fltEid, row.Tsran, tID, sID, exID)) # shipment time distribution
    exID += 1
# Last do general & general
curSource.execute(sqlDshiptimeGenGen)
shipDs = curSource.fetchall()
for row in shipDs:
    cursorINP.execute(sqlFshipDist, (tID, sID, exID, "Shipment", cUser, tID, sID, row.DistType, tID)) # distribution
    cursorINP.execute(sqlFshipDistParam, (tID, sID, exID, row.p1, cUser, tID, sID, exID, 1)) # distribution parameter
    if row.DistType in ("Weibull","Normal", "Uniform"): # require two parameters
        cursorINP.execute(sqlFshipDistParam, (tID, sID, exID, row.p2, cUser, tID, sID, exID, 2)) # distribution parameter
    cursorINP.execute(sqlFshipGenGen, (tID, sID, exID, cUser, row.fltEid, row.tltEid, tID, sID, exID)) # shipment time distribution
    exID += 1
print "Done with Shipment Times and Distributions" , datetime.datetime.now().time().isoformat()
###################################### / END SHIPEMENT TIME DISTRIBUTION

###################################### / BEGIN ROUTE AND ROUTE MAP AND RESUPPLY
# these tables can be Defaulted to all groups, or specific to only one group
#sqlDRoute = '''SELECT %s, %s, '%s', [*Base Structure].[From Base], [*Base Structure].[To Base], %s, %s FROM [*Base Structure]''' % (tID, sID, cUser, tID, sID)
# right now this query doesn't include the reverse route - it only adds one route per base_structure entry

sqlDRoute = '''SELECT [*Base Structure].[From Base] AS FRbase, [*Base Structure].[To Base] as TObase, Group, Probability FROM [*Base Structure]''' 
curSource.execute(sqlDRoute)
routes = curSource.fetchall()
# load into route, route_map
SQLFRoute = '''INSERT INTO input.route (tenant_id, simulation_id, external_id, from_location_id, to_location_id, create_user, create_timestamp) 
    SELECT ?, ?, ?, lf.id, lt.id, ?, CURRENT_TIMESTAMP FROM location lf JOIN location lt on lf.simulation_id = lt.simulation_id WHERE lf.external_id = ? AND lt.external_id = ? AND lf.tenant_id = ? AND lf.simulation_id = ?'''
sqlFRouteMapSpec = '''INSERT INTO input.route_map (tenant_id, simulation_id, external_id, object_class_id, probability, route_id, event_id, create_user, create_timestamp)
    SELECT ?, ?, ?, oc.id, ?, r.id, e.id, ?, CURRENT_TIMESTAMP FROM input.object_class oc JOIN input.route r ON r.simulation_id = oc.simulation_id JOIN input.event e on e.simulation_id = r.simulation_id WHERE left(oc.external_id,3) = ? AND oc.tenant_id = ? and oc.simulation_id = ? AND r.external_id = ? AND e.name like "Shipment"'''
sqlFRouteMapGen = '''INSERT INTO input.route_map (tenant_id, simulation_id, external_id, probability, route_id, event_id, create_user, create_timestamp)
    SELECT ?, ?, ?, ?, r.id, e.id, ?, CURRENT_TIMESTAMP FROM input.route r JOIN input.event e on e.simulation_id = r.simulation_id WHERE r.tenant_id = ? and r.simulation_id = ? AND r.external_id = ? AND e.name like "Shipment"'''
exID = 1
for row in routes:
    cursorINP.execute(SQLFRoute, (tID, sID, exID, cUser, row.FRbase, row.TObase, tID, sID))
    if row.Group == -1: # applies to all objects, leave the object info blank
        cursorINP.execute(sqlFRouteMapGen, (tID, sID, exID, row.Probability, cUser, tID, sID, exID))
    else: # applies to only specific objects - match to classes (group is required for model implementation)
        cursorINP.execute(sqlFRouteMapSpec, (tID, sID, exID, row.Probability, cUser, row.Group, tID, sID, exID))
    exID += 1
print 'Done with Route and Route Map' , datetime.datetime.now().time().isoformat()
###################################### / END ROUTE AND ROUTE MAP

###################################### / BEGIN RESUPPLY
# Need one (and only one) resupply row for each route - object information seems to be duplicated - in both resupply and route_map tables
# I'll steal from shipment distribution
# not sure what supply order or minimum wait are, so I'll give them values of one and zero
SQLResupplySpecSpec = '''INSERT INTO input.resupply (tenant_id, simulation_id, external_id, route_id, distribution_id, supply_order, minimum_wait, create_user, create_timestamp) 
    SELECT ?, ?, r.external_id, r.id, st.distribution_id, ?, ?, ?, CURRENT_TIMESTAMP FROM input.route r JOIN input.shipment_time_distribution st ON st.from_location_id = r.from_location_id AND st.to_location_id = r.to_location_id WHERE r.tenant_id = ? AND r.simulation_id = ?'''
# only add further routes that aren't there yet - need to make general matches through location type external id matches to first digit of the location's external id
SQLResupplyGenSpec = '''INSERT INTO input.resupply (tenant_id, simulation_id, external_id, route_id, distribution_id, supply_order, minimum_wait, create_user, create_timestamp) 
    SELECT ?, ?, r.external_id, r.id, st.distribution_id, ?, ?, ?, CURRENT_TIMESTAMP FROM input.route r JOIN input.shipment_time_distribution st ON r.to_location_id = st.to_location_id JOIN input.location_type flt on flt.id = st.from_location_type_id JOIN input.location fl on fl.id = r.from_location_id AND flt.external_id = left(fl.external_id,1) WHERE r.tenant_id = ? AND r.simulation_id = ? AND r.id not in (SELECT route_id from input.resupply r WHERE tenant_id = ? AND simulation_id = ?)'''
# specific destination is more important than specific source
SQLResupplySpecGen = '''INSERT INTO input.resupply (tenant_id, simulation_id, external_id, route_id, distribution_id, supply_order, minimum_wait, create_user, create_timestamp) 
    SELECT ?, ?, r.external_id, r.id, st.distribution_id, ?, ?, ?, CURRENT_TIMESTAMP FROM input.route r JOIN input.shipment_time_distribution st ON r.from_location_id = st.from_location_id JOIN input.location_type tlt on tlt.id = st.to_location_type_id JOIN input.location tl on tl.id = r.to_location_id AND tlt.external_id = left(tl.external_id,1) WHERE r.tenant_id = ? AND r.simulation_id = ? AND r.id not in (SELECT route_id from input.resupply r WHERE tenant_id = ? AND simulation_id = ?)'''
# both gens
SQLResupplyGenGen = '''INSERT INTO input.resupply (tenant_id, simulation_id, external_id, route_id, distribution_id, supply_order, minimum_wait, create_user, create_timestamp) 
    SELECT ?, ?, r.external_id, r.id, st.distribution_id, ?, ?, ?, CURRENT_TIMESTAMP FROM input.route r JOIN input.shipment_time_distribution st ON r.simulation_id = st.simulation_id JOIN input.location_type tlt on tlt.id = st.to_location_type_id JOIN input.location tl on tl.id = r.to_location_id AND tlt.external_id = left(tl.external_id,1) JOIN input.location_type flt on flt.id = st.from_location_type_id JOIN input.location fl on fl.id = r.from_location_id AND flt.external_id = left(fl.external_id,1) WHERE r.tenant_id = ? AND r.simulation_id = ? AND r.id not in (SELECT route_id from input.resupply r WHERE tenant_id = ? AND simulation_id = ?)'''


cursorINP.execute(SQLResupplySpecSpec, (tID, sID, 1, 0, cUser, tID, sID))
cursorINP.execute(SQLResupplyGenSpec, (tID, sID, 1, 0, cUser, tID, sID, tID, sID))
cursorINP.execute(SQLResupplySpecGen, (tID, sID, 1, 0, cUser, tID, sID, tID, sID))
cursorINP.execute(SQLResupplyGenGen, (tID, sID, 1, 0, cUser, tID, sID, tID, sID))



# print out the routes that don't have resupply values
print 'Done with Resupply' , datetime.datetime.now().time().isoformat()
###################################### / END RESUPPLY

###################################### / BEGIN INITIALIZATION
# some properties were inserted earlier, around event
###################################### / END INITIALIZATION

connSinkLCM.commit()
connSinkInput.commit()

############################################################################ / BEGIN OUTPUT DATA ############################################################################ / BEGIN OUTPUT DATA
############################################################################ / BEGIN OUTPUT DATA ############################################################################ / BEGIN OUTPUT DATA
############################################################################ / BEGIN OUTPUT DATA ############################################################################ / BEGIN OUTPUT DATA
    # Output data requires some information from the LCM schema and some information from the INPUT schema.
    # First put data in the output.simulation table.
    ###################################### / BEGIN SIMULATION
sqlOS = '''INSERT INTO simulation (tenant_id, simulation_id, simulation_name, simulation_type_name, number_of_replications) SELECT DISTINCT %s, %s, s.name, st.name, %s FROM lcm.simulation s, lcm.simulation_type st WHERE s.id = %s AND st.id = %s ''' % (tID, sID, numberOfReps, sID, simTypeID)
cursorOUT.execute(sqlOS)
    ###################################### / END SIMULATION
    # Only enter output data for Demos
if SimOrDemo == 2:
    ###################################### / BEGIN AVAILABILITY
    # outputs will use a different method  - just get all the output
    qtr = ''' 'CALENDAR_QUARTER' '''
    wky = ''' 'CALENDAR_WEEK' '''
    ovl = ''' 'OVERALL' '''
    sqlDOAquarter = '''SELECT %s, %s, %s, %s, [*Weeks and Dates].Date, Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed])*100 AS avail, %s, %s, %s, [out Availability].SRAN, 
    [out Availability].Type, %s FROM [out Availability] INNER JOIN [*Weeks and Dates] ON ([out Availability].Year = [*Weeks and Dates].Year) AND ([out Availability].Qtr = [*Weeks and Dates].Quarter) WHERE ((([*Weeks and Dates].Week)=1)) 
    GROUP BY [*Weeks and Dates].Date, [out Availability].Year, [out Availability].Qtr, [out Availability].SRAN, [out Availability].Type''' % (tID, sID, pID, qtr, pID, tID, qtr, sID)
    sqlDOAweek = '''SELECT %s, %s, %s, %s, [*Weeks and Dates].Date, 
    Sum([out Availability].Availability*[out Availability].[#Deployed])/Sum([out Availability].[#Deployed])*100 AS avail, %s, %s, %s, [out Availability].SRAN, [out Availability].Type, %s
    FROM [out Availability] INNER JOIN [*Weeks and Dates] ON ([*Weeks and Dates].Week = [out Availability].Week) AND ([out Availability].Qtr = [*Weeks and Dates].Quarter) AND ([out Availability].Year = [*Weeks and Dates].Year)
    GROUP BY [*Weeks and Dates].Date, [out Availability].SRAN, [out Availability].Type, [out Availability].Year, [out Availability].Qtr''' % (tID, sID, pID, wky, pID, tID, wky, sID)
    sqlFOA = '''INSERT INTO output.availability (tenant_id, simulation_id, project_name, project_id, asset, spare, interval_unit_id, interval_unit_name, timestamp, value, location_id, location_name, object_type_id, object_type_name)
        SELECT ?, ?, pr.name, ?, 1, 0, iu.id, ?, ?, ?, l.id, l.name, ot.id, ot.name FROM lcm.project pr JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id, lcm.interval_unit iu WHERE pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND l.external_id = ? AND ot.external_id = ? and l.simulation_id = ?'''
    sqlFOAselect = '''SELECT ?, ?, pr.name, ?, 1, 0, iu.id, ?, ?, ?, l.id, l.name, ot.id, ot.name FROM lcm.project pr JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id, lcm.interval_unit iu WHERE pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND l.external_id = ? AND ot.external_id = ?'''

    curSource.execute(sqlDOAquarter)
    qtrAvail = curSource.fetchall()
    curSource.execute(sqlDOAweek)
    wkyAvail = curSource.fetchall()
    # now load data
    cursorOUT.executemany(sqlFOA, qtrAvail)
    print "Done with Quarterly Availability Output" , datetime.datetime.now().time().isoformat()
    cursorOUT.executemany(sqlFOA, wkyAvail)
    print "Done with Weekly Availability Output" , datetime.datetime.now().time().isoformat()

    # DEMAND Pro does not have availability by object this uses the object type name and id
    sqlMoreAvInfo = '''UPDATE output.availability a
            JOIN
        input.object_type ot ON ot.id = a.object_type_id
            JOIN
        input.object_class oc ON oc.id = ot.object_class_id
            JOIN
        input.object_group og ON og.id = oc.object_group_id
            JOIN
        input.location l ON l.id = a.location_id
            JOIN
        input.location_region lr ON lr.id = l.location_region_id
    SET 
        a.object_class_id = oc.id,
        a.object_class_name = oc.name,
        a.object_group_id = og.id,
        a.object_group_name = og.name,
        a.region_id = lr.id,
        a.region_name = lr.name,
        a.object_name = ot.name,
        a.object_serial_number = ot.name,
        a.object_internal_name = ot.internal_name,
        a.object_id = ot.id
    WHERE
        a.tenant_id = %s AND a.simulation_id = %s ''' % (tID, sID)
    cursorOUT.execute(sqlMoreAvInfo)
    connSinkOut.commit()
    print 'Done with Output Availability' , datetime.datetime.now().time().isoformat()
    ###################################### / END AVAILABILITY

    ###################################### / BEGIN COMPONENT EVENTS
    # first do LRU events (ASSUMES ONLY ONE EVENT OF TYPE FAILURE CALLED FAILURE - SINGLE FAILURE MODE)
    eventNameMatch = ''' 'Failure' '''
    sqlDOLruEv = '''SELECT %s, %s, %s, %s, [*Weeks and Dates].Date, [out LRU events].UER, %s, %s, %s, %s, %s, [out LRU events].[SRAN ID] as SRAN, [out LRU events].[Engine type] as Type, %s, %s
    FROM [out LRU events] INNER JOIN [*Weeks and Dates] ON ([out LRU events].Year = [*Weeks and Dates].Year) AND ([out LRU events].Qtr = [*Weeks and Dates].Quarter)
    WHERE ((([*Weeks and Dates].Week)=1))''' % (tID, sID, pID, qtr, tID, sID, pID, tID, qtr, eventNameMatch, sID)
    curSource.execute(sqlDOLruEv)
    failures = curSource.fetchall()
    sqlFOLruEv = '''INSERT INTO output.component_events (tenant_id, simulation_id, project_name, project_id, event_id, event_name, event_type_id, event_type_name, interval_unit_id, interval_unit_name, timestamp, value, location_id, location_name, object_type_id, object_type_name)
        SELECT ?, ?, pr.name, ?, e.id, e.name, et.id, et.name, iu.id, ?, ?, ?, l.id, l.name, ot.id, ot.name FROM input.event e JOIN input.event_type et on et.id = e.event_type_id JOIN lcm.project pr ON e.tenant_id = pr.tenant_id JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id, lcm.interval_unit iu WHERE et.tenant_id = ? and et.simulation_id = ? AND pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND l.external_id = ? AND ot.external_id = ? AND e.name = ? and l.simulation_id = ?'''
    cursorOUT.executemany(sqlFOLruEv, failures)

    sqlMoreCEInfo = '''UPDATE output.component_events a
            JOIN    input.object_type ot ON ot.id = a.object_type_id
            JOIN    input.object_class oc ON oc.id = ot.object_class_id
            JOIN    input.object_group og ON og.id = oc.object_group_id
            JOIN    input.location l ON l.id = a.location_id
            JOIN    input.location_region lr ON lr.id = l.location_region_id
        SET     a.object_class_id = oc.id, a.object_class_name = oc.name, a.object_group_id = og.id,
                a.object_group_name = og.name, a.region_id = lr.id, a.region_name = lr.name, a.object_name = ot.name, a.object_internal_name = ot.internal_name
        WHERE   a.tenant_id = %s AND a.simulation_id = %s ''' % (tID, sID)
    cursorOUT.execute(sqlMoreCEInfo)    
    connSinkOut.commit()
    ## Now do New Buys - both parts that arrive after t=0 (defined in OAI, e.g. buy plan) and new buys (from BUC)
    # 1) Parts that Arrive after T=0
    #    these likely do not span each quarter/type/location, but to plot in analytics tab they must.  
    #    first fill relevant quarters with zero (across all quarters for any matches on type/location)
    #           possible quarter values taken from the distinct ones that exist for that interval unit in component events data inserted above for failures
    #       then update where acquisitions do exist 
    eventNameMatch = ''' 'Acquisition' '''
    # this query only works for arrival times equal to a round year, i.e. years-from-start rather than quarters between
    sqlTLmatch = '''SELECT %s as sID, %s as IUN, %s as tID, %s as sID2, %s as prId, %s as tID2, %s as IUN2, [*Object attributes initial].SRAN as lEid, [*Object attributes initial].[Object type] as otEid, %s as etn, %s as sId3 FROM [*Object attributes initial]
        WHERE ((([*Object attributes initial].[Arrival time ta])>0) AND (([*Object attributes initial].[Parent Pp])=0))
        GROUP BY [*Object attributes initial].[Object type], [*Object attributes initial].SRAN''' % (sID, qtr, tID, sID, pID, tID, qtr, eventNameMatch, sID)
    sqlOAt11 = '''INSERT INTO output.component_events (tenant_id, simulation_id, project_name, project_id, event_id, event_name, event_type_id, event_type_name, interval_unit_id, interval_unit_name, timestamp, value, location_id, location_name, object_type_id, object_type_name)
    SELECT pr.tenant_id, e.simulation_id, pr.name, pr.id, e.id, e.name, et.id, et.name, iu.id, iu.name, allTimes.timestamp, 0, l.id, l.name, ot.id, ot.name FROM input.event e JOIN input.event_type et on et.id = e.event_type_id JOIN lcm.project pr ON e.tenant_id = pr.tenant_id JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id, lcm.interval_unit iu, (SELECT DISTINCT timestamp from output.component_events where simulation_id = ? and interval_unit_name = ?) allTimes WHERE et.tenant_id = ? and et.simulation_id = ? AND pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND l.external_id = ? AND ot.external_id = ? AND e.name = ? and l.simulation_id = ?'''
    curSource.execute(sqlTLmatch)
    aqPairs = curSource.fetchall()
    for pair in aqPairs:
        cursorOUT.execute(sqlOAt11, pair)
    sqlOAt12 = '''UPDATE output.component_events ce JOIN input.object_type ot on ot.id = ce.object_type_id JOIN input.location l on l.id = ce.location_id SET ce.value = ? WHERE ce.tenant_id = ? AND ce.simulation_id = ? AND ce.interval_unit_name = ? AND ot.external_id = ? AND l.external_id = ? AND ce.timestamp = ?'''
    sqlOAtT = '''SELECT count(TreeCode) as count, %s as tenantID, %s as simulationID, %s as intervalUnitName, [object type] as otEid, sran as lEid, [*Weeks and Dates].Date FROM [*object attributes initial], [*Analysis Range] INNER JOIN [*Weeks and Dates] ON [*Analysis Range].[First quarter (1 - 4)] = [*Weeks and Dates].Quarter WHERE ((([*object attributes initial].[parent pp])=0) AND (([*object attributes initial].[Arrival time ta])<>0) AND (([*Weeks and Dates].Week)=1) AND (([*Analysis Range].[First year (>= 1999)]+[*Object attributes initial].[Arrival time ta])=[*Weeks and Dates].[Year])) GROUP BY [*Object attributes initial].SRAN, [*Object attributes initial].[object type], [*Weeks and Dates].Date''' % (tID, sID, qtr)
    curSource.execute(sqlOAtT)
    acquisitions = curSource.fetchall()
    cursorOUT.executemany(sqlOAt12, acquisitions)
    sqlMoreCEInfo = '''UPDATE output.component_events a
            JOIN    input.object_type ot ON ot.id = a.object_type_id
            JOIN    input.object_class oc ON oc.id = ot.object_class_id
            JOIN    input.object_group og ON og.id = oc.object_group_id
            JOIN    input.location l ON l.id = a.location_id
            JOIN    input.location_region lr ON lr.id = l.location_region_id
        SET     a.object_class_id = oc.id, a.object_class_name = oc.name, a.object_group_id = og.id,
                a.object_group_name = og.name, a.region_id = lr.id, a.region_name = lr.name, a.object_name = ot.name, a.object_internal_name = ot.internal_name
        WHERE   a.tenant_id = %s AND a.simulation_id = %s AND event_name = %s''' % (tID, sID, eventNameMatch)
    cursorOUT.execute(sqlMoreCEInfo)   
    connSinkOut.commit()
    # 2) New Buys
    print 'Done with Output: Component Events' , datetime.datetime.now().time().isoformat()
    ###################################### / END COMPONENT EVENTS

    ###################################### / BEGIN SPARE QUANTITY
    sqlDSpQtr = '''SELECT %s, %s, %s, %s, [*Weeks and Dates].Date, [out Uninstalled-Serviceable items].Serviceables, %s, %s, %s, [out Uninstalled-Serviceable items].[SRAN ID] AS SRAN, [out Uninstalled-Serviceable items].[Object type] AS Type, %s, %s
    FROM [out Uninstalled-Serviceable items] INNER JOIN [*Weeks and Dates] ON ([out Uninstalled-Serviceable items].Qtr = [*Weeks and Dates].Quarter) AND ([out Uninstalled-Serviceable items].Year = [*Weeks and Dates].Year) AND ([out Uninstalled-Serviceable items].Week = [*Weeks and Dates].Week)
    WHERE ((([*Weeks and Dates].Week)=1))''' % (tID, sID, pID, qtr, pID, tID, qtr, tID, sID)
    sqlDSpWeek = '''SELECT %s, %s, %s, %s, [*Weeks and Dates].Date, [out Uninstalled-Serviceable items].Serviceables, %s, %s, %s, [out Uninstalled-Serviceable items].[SRAN ID] AS SRAN, [out Uninstalled-Serviceable items].[Object type] AS Type, %s, %s
    FROM [out Uninstalled-Serviceable items] INNER JOIN [*Weeks and Dates] ON ([out Uninstalled-Serviceable items].Week = [*Weeks and Dates].Week) AND ([out Uninstalled-Serviceable items].Year = [*Weeks and Dates].Year) AND ([out Uninstalled-Serviceable items].Qtr = [*Weeks and Dates].Quarter)''' % (tID, sID, pID, wky, pID, tID, wky, tID, sID)
    sqlFOspares = '''INSERT INTO output.spare_quantity (tenant_id, simulation_id, project_name, project_id, interval_unit_id, interval_unit_name, timestamp, value, location_id, location_name, object_type_id, object_type_name, internal_state)
        SELECT ?, ?, pr.name, ?, iu.id, ?, ?, ?, l.id, l.name, ot.id, ot.name, 1 FROM lcm.project pr JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id, lcm.interval_unit iu WHERE pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND l.external_id = ? AND ot.external_id = ? AND ot.tenant_id = ? and ot.simulation_id = ?'''
    curSource.execute(sqlDSpQtr)
    spares = curSource.fetchall()
    try:
        cursorOUT.executemany(sqlFOspares, spares)
    except pyodbc.ProgrammingError:
        print "No Spares Output"
    except:
        print "some unknown error in spares output"
        raise
    # curSource.execute(sqlDSpWeek)  - there's too much data here - it takes forever to load
    # spares = curSource.fetchall()
    # cursorOUT.executemany(sqlFOspares, spares)
    sqlMoreSPInfo = '''UPDATE output.spare_quantity a
            JOIN    input.object_type ot ON ot.id = a.object_type_id
            JOIN    input.object_class oc ON oc.id = ot.object_class_id
            JOIN    input.object_group og ON og.id = oc.object_group_id
            JOIN    input.location l ON l.id = a.location_id
            JOIN    input.location_region lr ON lr.id = l.location_region_id
            JOIN    input.storage s ON s.simulation_id = l.simulation_id AND  s.location_id = l.id
        SET     a.object_class_id = oc.id, a.object_class_name = oc.name, a.object_group_id = og.id,
                a.object_group_name = og.name, a.region_id = lr.id, a.region_name = lr.name, 
                a.storage_name = l.name, a.storage_internal_name = s.internal_name, a.storage_id = s.id
        WHERE   a.tenant_id = %s AND a.simulation_id = %s ''' % (tID, sID)
    cursorOUT.execute(sqlMoreSPInfo)
    connSinkOut.commit()
    print 'Done with Output: Spare Quantity' , datetime.datetime.now().time().isoformat()
    ###################################### / END SPARE QUANTITY

    ###################################### / BEGIN DOWN ASSETS
    sqlDSpQtr = '''SELECT %s, %s, %s, %s, [*Weeks and Dates].Date AS [timestamp], [out AWP items].[in awp status] AS [value], %s, %s, %s, [out AWP items].[Object type] AS Type, [out AWP items].[SRAN ID] AS Sran, %s, %s
    FROM [*Weeks and Dates] INNER JOIN [out AWP items] ON ([*Weeks and Dates].Week = [out AWP items].Week) 
    AND ([*Weeks and Dates].Quarter = [out AWP items].Qtr) AND ([*Weeks and Dates].Year = [out AWP items].Year)
    WHERE ((([*Weeks and Dates].Week)=1))''' % (tID, sID, pID, qtr, pID, tID, qtr, tID, sID)
    sqlDDAwk = '''SELECT %s, %s, %s, %s, [*Weeks and Dates].Date AS [timestamp], [out AWP items].[in awp status] AS [value], %s, %s, %s, [out AWP items].[Object type] AS Type, [out AWP items].[SRAN ID] AS Sran, %s, %s
    FROM [*Weeks and Dates] INNER JOIN [out AWP items] ON ([*Weeks and Dates].Week = [out AWP items].Week) 
    AND ([*Weeks and Dates].Quarter = [out AWP items].Qtr) AND ([*Weeks and Dates].Year = [out AWP items].Year)''' % (tID, sID, pID, wky, pID, tID, wky, tID, sID)

    sqlFOdownassets = '''INSERT INTO output.down_assets_parts_delay (tenant_id, simulation_id, project_name, project_id, interval_unit_id, interval_unit_name, timestamp, value, location_id, location_name, object_type_id, object_type_name)
        SELECT ?, ?, pr.name, ?, iu.id, ?, ?, ?, l.id, l.name, ot.id, ot.name FROM lcm.project pr JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id, lcm.interval_unit iu WHERE pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND ot.external_id = ? AND l.external_id = ? AND ot.tenant_id = ? and ot.simulation_id = ?'''
    curSource.execute(sqlDSpQtr)
    downAssets = curSource.fetchall()
    cursorOUT.executemany(sqlFOdownassets, downAssets)
    print "Done with Quarterly Down Assets Parts Output" , datetime.datetime.now().time().isoformat()
    curSource.execute(sqlDDAwk)
    downAssets = curSource.fetchall()
    cursorOUT.executemany(sqlFOdownassets, downAssets)
    print "Done with Weekly Down Assets Parts Output" , datetime.datetime.now().time().isoformat()
    sqlMoreDAInfo = '''UPDATE output.down_assets_parts_delay a
            JOIN    input.object_type ot ON ot.id = a.object_type_id
            JOIN    input.object_class oc ON oc.id = ot.object_class_id
            JOIN    input.object_group og ON og.id = oc.object_group_id
            JOIN    input.location l ON l.id = a.location_id
            JOIN    input.location_region lr ON lr.id = l.location_region_id
        SET     a.object_class_id = oc.id, a.object_class_name = oc.name, a.object_group_id = og.id,
                a.object_group_name = og.name, a.region_id = lr.id, a.region_name = lr.name
        WHERE   a.tenant_id = %s AND a.simulation_id = %s ''' % (tID, sID)
    cursorOUT.execute(sqlMoreDAInfo)
    connSinkOut.commit()
    print 'Done with Output: Down Assets Part Delay' , datetime.datetime.now().time().isoformat()
    ###################################### / END DOWN ASSETS

    ###################################### / BEGIN EQUIPMENT CRITICALITY
    sqlEqCritOverall = '''SELECT %s, %s, %s, %s, [*Weeks and Dates].Date, [*Analysis Range].[Amount of quarters]*91.25*Total-Sum([out Waiting time and Unavailability].[Waiting Time]*[out Waiting time and Unavailability].Unavailability*[out Waiting time and Unavailability].Requests) AS PTypeTotalUptime, 
    Sum([out Waiting time and Unavailability].[Waiting Time]*[out Waiting time and Unavailability].Unavailability*[out Waiting time and Unavailability].Requests) AS PTypeTotalDowntime, %s, %s, %s, %s, %s, [out Waiting time and Unavailability].[Type being Built], [out Waiting time and Unavailability].[Type Needed], [out Waiting time and Unavailability].SRAN
    FROM (([out Waiting time and Unavailability] INNER JOIN [*Object Type] ON [out Waiting time and Unavailability].[Type being Built] = [*Object Type].[Object type]) INNER JOIN zDisposition ON ([out Waiting time and Unavailability].SRAN = zDisposition.SRAN) AND ([out Waiting time and Unavailability].[Type being Built] = zDisposition.[Object type])) INNER JOIN (SELECT [out Waiting time and Unavailability].[Type being Built], [out Waiting time and Unavailability].SRAN, Sum([out Waiting time and Unavailability].[Waiting Time]*[out Waiting time and Unavailability].Unavailability*[out Waiting time and Unavailability].Requests) AS TotalDowntime, [*Analysis Range].[Amount of quarters]*91.25*Total AS totalDays
    FROM [*Analysis Range], ([out Waiting time and Unavailability] INNER JOIN [*Object Type] ON [out Waiting time and Unavailability].[Type being Built] = [*Object Type].[Object type]) INNER JOIN  zDisposition ON [out Waiting time and Unavailability].SRAN = zDisposition.SRAN AND [out Waiting time and Unavailability].[Type being Built] = zDisposition.[Object type]
    GROUP BY [out Waiting time and Unavailability].[Type being Built], [out Waiting time and Unavailability].SRAN, [*Object Type].[Ind level], [*Analysis Range].[Amount of quarters], zDisposition.Total
    HAVING ((([*Object Type].[Ind level])=1)))  AS aggDT ON ([out Waiting time and Unavailability].[Type being Built] = aggDT.[Type being Built]) AND ([out Waiting time and Unavailability].SRAN = aggDT.SRAN), [*Analysis Range] INNER JOIN [*Weeks and Dates] ON ([*Weeks and Dates].Year = [*Analysis Range].[First year (>= 1999)]) AND ([*Analysis Range].[First quarter (1 - 4)] = [*Weeks and Dates].Quarter)
    GROUP BY [out Waiting time and Unavailability].[Type being Built], [out Waiting time and Unavailability].[Type Needed], [out Waiting time and Unavailability].SRAN, aggDT.TotalDowntime, [*Object Type].[Ind level], [*Analysis Range].[Amount of quarters], zDisposition.Total, [*Weeks and Dates].Date, [*Weeks and Dates].Week
    HAVING ((([*Object Type].[Ind level])=1) AND (([*Weeks and Dates].Week)=1))''' % (tID, sID, pID, ovl, pID, tID, ovl, tID, sID)

    sqlInsertEqCritOvl = '''INSERT INTO output.equipment_criticality_pareto (tenant_id, simulation_id, project_name, project_id, interval_unit_id, interval_unit_name, timestamp, availability, unavailability, location_id, location_name, object_type_id, object_type_name, culprit_type_id, culprit_object_type_name, cause, event_type_id, event_type_name)
        SELECT ?, ?, pr.name, ?, iu.id, ?, ?, ?, ?, l.id, l.name, ot.id, ot.name, cot.id, cot.name, "CM", et.id, et.name FROM lcm.project pr JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id JOIN input.object_type cot on cot.simulation_id = ot.simulation_id JOIN input.event_type et on et.simulation_id = ot.simulation_id, lcm.interval_unit iu WHERE pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND ot.tenant_id = ? and ot.simulation_id = ? AND ot.external_id = ? AND cot.external_id = ? AND l.external_id = ? AND et.name like "Failure"'''
    curSource.execute(sqlEqCritOverall)
    equipCRIT = curSource.fetchall()
    cursorOUT.executemany(sqlInsertEqCritOvl, equipCRIT)
    print "Done with Overall Equipment Criticality Output" , datetime.datetime.now().time().isoformat()

    sqlMoreEQInfo = '''UPDATE output.equipment_criticality_pareto a
            JOIN    input.object_type ot ON ot.id = a.object_type_id
            JOIN    input.object_class oc ON oc.id = ot.object_class_id
            JOIN    input.object_group og ON og.id = oc.object_group_id
            JOIN    input.location l ON l.id = a.location_id
            JOIN    input.location_region lr ON lr.id = l.location_region_id
            JOIN    input.object_type cot ON cot.id = a.culprit_type_id
            JOIN    input.object_class coc ON coc.id = cot.object_class_id
            JOIN    input.object_group cog ON cog.id = coc.object_group_id
            JOIN    input.event e ON a.event_type_id = e.event_type_id
        SET     a.object_class_id = oc.id, a.object_class_name = oc.name, a.object_group_id = og.id,
                a.object_group_name = og.name, a.region_id = lr.id, a.region_name = lr.name,
                a.culprit_class_id = coc.id, a.culprit_class_name = coc.name,
                a.culprit_group_id = cog.id, a.culprit_group_name = cog.name, a.event_id = e.id, a.event_name = e.name
        WHERE   a.tenant_id = %s AND a.simulation_id = %s ''' % (tID, sID)
    cursorOUT.execute(sqlMoreEQInfo)
    connSinkOut.commit()
    print 'Done with Output: Equipment Criticality' , datetime.datetime.now().time().isoformat()
    ###################################### / END EQUIPMENT CRITICALITY

    ###################################### / BEGIN TOTAL CRITICALITY
    sqlInsertTotCritOvl = '''INSERT INTO output.total_criticality_pareto (tenant_id, simulation_id, project_name, project_id, interval_unit_id, interval_unit_name, timestamp, availability, unavailability, location_id, location_name, object_type_id, object_type_name, object_id, object_name, culprit_type_id, culprit_object_type_name, cause, event_type_id, event_type_name)
        SELECT ?, ?, pr.name, ?, iu.id, ?, ?, ?, ?, l.id, l.name, ot.id, ot.name, ot.id, ot.name, cot.id, cot.name, "CM", et.id, et.name FROM lcm.project pr JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id JOIN input.object_type cot on cot.simulation_id = ot.simulation_id JOIN input.event_type et on et.simulation_id = ot.simulation_id, lcm.interval_unit iu WHERE pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND ot.tenant_id = ? and ot.simulation_id = ? AND ot.external_id = ? AND cot.external_id = ? AND l.external_id = ? AND et.name like "Failure"'''
    cursorOUT.executemany(sqlInsertTotCritOvl, equipCRIT)
    print "Done with Overall Total Criticality Output" , datetime.datetime.now().time().isoformat()
    sqlMoreTCInfo = '''UPDATE output.total_criticality_pareto a
        JOIN    input.object_type ot ON ot.id = a.object_type_id
        JOIN    input.object_class oc ON oc.id = ot.object_class_id
        JOIN    input.object_group og ON og.id = oc.object_group_id
        JOIN    input.location l ON l.id = a.location_id
        JOIN    input.location_region lr ON lr.id = l.location_region_id
        JOIN    input.object_type cot ON cot.id = a.culprit_type_id
        JOIN    input.object_class coc ON coc.id = cot.object_class_id
        JOIN    input.object_group cog ON cog.id = coc.object_group_id
        JOIN    input.event e ON a.event_type_id = e.event_type_id
    SET     a.object_class_id = oc.id, a.object_class_name = oc.name, a.object_group_id = og.id, a.object_id = ot.id, a.object_name = ot.name,
            a.object_group_name = og.name, a.region_id = lr.id, a.region_name = lr.name,
            a.culprit_class_id = coc.id, a.culprit_class_name = coc.name,
            a.culprit_group_id = cog.id, a.culprit_group_name = cog.name, a.event_id = e.id, a.event_name = e.name
    WHERE   a.tenant_id = %s AND a.simulation_id = %s ''' % (tID, sID)
    cursorOUT.execute(sqlMoreTCInfo)
    print 'Done with Output: Total Criticality' , datetime.datetime.now().time().isoformat()
    ###################################### / END TOTAL CRITICALITY

    ############################################################################ / BEGIN COST
    # currency value class and type should already be defined in input schema. simply add currency value

    ###################################### / BEGIN SHIPMENT COST
    # there is no shipments output table, and I can't use component events because there is only one sran (not to or from) so I have to go straight to cost :(
    sqlInsertShipmentCost = '''INSERT INTO output.operations_support_cost (tenant_id, simulation_id, project_name, project_id, interval_unit_id, interval_unit_name, timestamp, value, location_id, location_name, to_location_id, to_location_name, object_type_id, object_type_name, object_id, object_name, cost_id, cost_name, cost_type_id, cost_type_name)
        SELECT pr.tenant_id, l.simulation_id, pr.name, pr.id, iu.id, iu.name, ?, ?*cv.currency_value, l.id, l.name, l_t.id, l_t.name, ot.id, ot.name, ot.id, ot.name, cv.id, cv.name, cvt.id, cvt.name
        FROM lcm.project pr JOIN input.location l on l.tenant_id = pr.tenant_id JOIN input.location_type lt on lt.id = l.location_type_id JOIN input.object_type ot ON ot.simulation_id = l.simulation_id JOIN input.location l_t on l_t.simulation_id = l.simulation_id JOIN input.location_type lt_t on lt_t.id = l_t.location_type_id JOIN input.currency_value cv on cv.simulation_id = ot.simulation_id and cv.object_class_id = ot.object_class_id and cv.from_location_type_id = lt.id and cv.to_location_type_id = lt_t.id JOIN input.currency_value_type cvt on cvt.id = cv.currency_value_type_id, lcm.interval_unit iu 
        WHERE pr.id = ? AND pr.tenant_id = ? AND iu.name like ? AND ot.tenant_id = ? and ot.simulation_id = ? AND ot.external_id = ? AND l.external_id = ? AND l_t.external_id = ? AND lt.external_id = ? AND lt_t.external_id = ? AND cvt.name like "Shipment"'''
    sqlShipmentCostSource = '''SELECT [*Weeks and Dates].Date, [out Shipments].Shipments, %s as proj, %s as tenant, %s as iun, %s as tenant2, %s as sim, [out Shipments].Type, [out Shipments].[From SRAN], [out Shipments].[To SRAN], int(Left([From SRAN],1)) AS lt_f, int(Left([To SRAN],1)) AS lt_t
        FROM [*Weeks and Dates] INNER JOIN [out Shipments] ON ([*Weeks and Dates].Quarter = [out Shipments].Qtr) AND ([*Weeks and Dates].Year = [out Shipments].Year)
        WHERE ((([*Weeks and Dates].Week)=1))''' % (pID, tID, qtr, tID, sID)
    curSource.execute(sqlShipmentCostSource)
    shipments = curSource.fetchall()
    cursorOUT.executemany(sqlInsertShipmentCost, shipments) # took 2+ hours for ~70K rows

    sqlMoreOSCInfo = '''UPDATE output.operations_support_cost a
        JOIN    input.object_type ot ON ot.id = a.object_type_id
        JOIN    input.object_class oc ON oc.id = ot.object_class_id
        JOIN    input.object_group og ON og.id = oc.object_group_id
        JOIN    input.location l ON l.id = a.location_id
        JOIN    input.location_region lr ON lr.id = l.location_region_id
        JOIN    input.location_type lt on lt.id = l.location_type_id
        JOIN    input.object_type cot ON cot.id = a.object_type_id
        JOIN    input.object_class coc ON coc.id = cot.object_class_id
        JOIN    input.object_group cog ON cog.id = coc.object_group_id
        JOIN    input.location l_t ON a.to_location_id = l_t.id
        JOIN    input.location_type lt_t on l_t.location_type_id = lt_t.id
        JOIN    input.currency_value cv on cv.id = a.cost_id
        JOIN    input.currency_value_type cvt on cvt.id = cv.currency_value_type_id
    SET     a.object_class_id = oc.id, a.object_class_name = oc.name,
            a.object_group_id = og.id, a.object_group_name = og.name,
            a.region_id = lr.id, a.region_name = lr.name, a.from_location_id = a.location_id,
            a.from_location_name = a.location_name, a.from_location_type_id = lt.id,
            a.from_location_type_name = lt.name, a.to_location_type_id = lt_t.id,
            a.to_location_type_name = lt_t.name
    WHERE   a.tenant_id = %s AND a.simulation_id = %s AND a.cost_type_name = "Shipment"''' % (tID, sID)
    cursorOUT.execute(sqlMoreOSCInfo)
    connSinkOut.commit()
    ###################################### / BEGIN STORAGE COST


    ############################################################################ / END COST

connSinkLCM.commit()
connSinkInput.commit()
connSinkOut.commit()