##### NEW November 2014
import os
import pyodbc
import csv
import time
import xml.etree.ElementTree as et
adoc = et.parse('C:/Users/tbaer/Desktop/m777/data/IETM_PARSING/removal.xml')

# get the file
source_mdb = 'C:/Users/tbaer/Desktop/m777/data/IETM_PARSING/removalrequirements.accdb'
# open up a connection, then a cursor
conn = pyodbc.connect('Driver={{Microsoft Access Driver (*.mdb, *.accdb)}};DBQ={p};'.format(p=source_mdb))
cur = conn.cursor()

# Fields are:  Part Title, Task, Type, and Value, e.g. Cannon Assembly / Removal / Maintainers Required / 2

# title
titleparent = adoc.findall('wpidinfo')
tasktitle = titleparent[0][1].text
taskandtitle = tasktitle.split(',') # this usually returns two elements: the Title and the Task, but it may return many.  in that case, use the last part as the Task and everything before as the Title (dfcs)
	# sometimes there are no commas - use all as title and then empty string as task
if type(taskandtitle) is str: # then there was no comma
	taskandtitle = list(taskandtitle,"") # I"ll sort that out manually later
elif len(taskandtitle) > 2 # more than one comma
	task = taskandtitle.pop(-1) # use the last piece for the task (and the others for title)
	taskandtitle = ''.join(taskandtitle)

# tools
toolsparent = adoc.findall('wpinfo')[0][0] # tools is first child of wpinfo
alltasktools = []
for atool in toolsparent:
	alltasktools.append(atool[0].text)
	# enter tool requirements in database
	taskandtitle.extend(("Tool Required",atool[0].text))
	cur.execute('INSERT INTO RemovalRequirements values (?,?,?,?)', taskandtitle)

# personnel
personnelparent = adoc.findall('wpinfo//qty') # personnel requirement is a child of wpinfo
personnel = personnelparent[0].text # Marine Corps requirement is first element
taskandtitle = tasktitle.split(',')
# enter tool requirements in database
taskandtitle.extend(("2131s Required",personnel))
cur.execute('INSERT INTO RemovalRequirements values (?,?,?,?)',taskandtitle)

# previous tasks
previoustasksparent = adoc.findall('wpinfo//condition')
allprevioustasks = []
for oneprevioustask in previoustasksparent:
	# allprevioustasks.append(previoustasksparent[0].text  + previoustasksparent[0].findall('xref')[0].attrib.get('callout')) # previous tasks are split into texts and links, with callout attributes - 
		# these are many different formats and I can't even get the text in elements with multiple links, so I'm just going to store the entire xml
	allprevioustasks.append(et.tostring(oneprevioustask))
	# enter previous taks requirements in database (in xml format)
	taskandtitle = tasktitle.split(',')
	taskandtitle.extend(("Prev. Task Required",et.tostring(oneprevioustask)))
	cur.execute('INSERT INTO RemovalRequirements values (?,?,?,?)',taskandtitle)

# output all these elements into a database - look at that one case where I put things in the database for the M1A1 doe


conn.commit()
cur.close()
conn.close()
 


# another way?
tree = et.parse("C:/Users/tbaer/Desktop/m777/data/IETM_PARSING/removal.xml")
root = tree.getroot()


# make into sequence


# get all part-task filenames with xml
for root, dirs, files in os.walk('C:/MCM777/XML/TM25_P/PreKit'):
     for file in files:
         if file.find("xml") > -1:
             print os.path.join(root,file)


# how to only include relevant things?

	#1) within XML folder, ignore bdar, bulletins, Parts, STTE, TM_10, shipping, apps, pmcs, prelim, TS_2, fce01, fce03,  folders because they aren't parts - I'll delete these from the file structure instead

	#2) relevant file names
		# removal
		# install
		# insp_repair
		# assembly
		# disasse
		# check
		# bleed
	 # and others...

	 #3) if it includes a  condition element - but what about a task that doesn't need to do anything first

for root, dirs, files in os.walk('C:/MCM777/XML/TM25_P/PreKit'):
     for file in files:
         if file.find("xml") > -1:
             thetree = et.parse(os.path.join(root,file))
             if len(thetree.findall('wpinfo//condition')) > 0:
             	print thetree.



# here goes
def understandtheXML():
	conn = pyodbc.connect('Driver={{Microsoft Access Driver (*.mdb, *.accdb)}};DBQ={p};'.format(p=source_mdb))
	cur = conn.cursor()
	delete_list = ["&bull;", "&plusmn;", "&deg;", "&minus;", "&ldquo;", "&rdquo;", "&ordm;", "&frac12;", "&ndash;", "&shy;", "&rsquo;"] # these cannot be parsed by et so I need to remove them
	cur.execute('DELETE RemovalRequirements.* FROM RemovalRequirements')
	for root, dirs, files in os.walk('C:/Users/tbaer/Desktop/m777/data/IETM_PARSING/XML/TM25_P'): # DFCS goes all the way through
		for file in files:
			if file.find(".xml") > -1:
				print 'checking ' + os.path.join(root,file) 
				# there are many elements that et cannot parse, so we need to remove them first
				fin = open(os.path.join(root,file), "r")
				data = []
				for line in fin:
					for word in delete_list:
						line = line.replace(word, "")
					data.append(line)
				fin.close() # close connection to xml doc
				fout = open(os.path.join(root,file),"w") # truncate the file and rewrite without those awful html words
				for line in data:
					fout.write(line)
				fout.close() # close this now
				thetree = et.parse(os.path.join(root,file))
				# put the elements in the database
				#titleparent = thetree.findall('wpidinfo')
				#tasktitle = titleparent[0][1].text
				# title
				title = thetree.findall('wpidinfo//title')
				try:
					tasktitle = title[0].text
					taskandtitle = tasktitle.split(',') # this usually returns two elements: the Title and the Task, but it may return many.  in that case, use the last part as the Task and everything before as the Title (dfcs)
					# sometimes there are no commas - use all as title and then empty string as task
				except:
					tasktitle = title[0][0].text # sometimes it's down an element (bellows removal within carriage elevation)
					taskandtitle = tasktitle.split(',')
				if len(taskandtitle) == 1 : # then there was no comma
					taskandtitle = [tasktitle,""] # I'll sort that out manually later
				elif len(taskandtitle) > 2: # more than one comma
					task = taskandtitle.pop(-1) # use the last piece for the task (and the others for title)
					taskandtitle = [''.join(taskandtitle),task]
				elementsToInsert = list(taskandtitle) # make a copy
				# tools
				toolsparent = thetree.findall('wpinfo//tools-setup-item')
				# if no tools, use a none
				if len(toolsparent)==0:
					elementsToInsert.extend(("Tool Required",'None or No Match'))
					elementsToInsert.insert(0,os.path.join(root,file))
					cur.execute('INSERT INTO RemovalRequirements values (?,?,?,?,?)', elementsToInsert)
				for atool in toolsparent:
					elementsToInsert = list(taskandtitle)
					elementsToInsert.extend(("Tool Required",atool[0].text))
					elementsToInsert.insert(0,os.path.join(root,file))
					cur.execute('INSERT INTO RemovalRequirements values (?,?,?,?,?)', elementsToInsert)
				# personnel
				personnelparent = thetree.findall('wpinfo//persnreq-setup-item//qty') # personnel requirement is a child of wpinfo
				try: 
					personnel = personnelparent[0].text # Marine Corps requirement is first element
				except:
					personnel = 'N/A' # sometimes there's no personnel requirement listed (OFCE/fcemaint/06bb)
				elementsToInsert = list(taskandtitle)
				# enter tool requirements in database
				elementsToInsert.extend(("Maintainers Required",personnel))
				elementsToInsert.insert(0,os.path.join(root,file)) # path
				cur.execute('INSERT INTO RemovalRequirements values (?,?,?,?,?)',elementsToInsert)
				# previous tasks
				previoustasksparent = thetree.findall('wpinfo//condition')
				for oneprevioustask in previoustasksparent:
					elementsToInsert = list(taskandtitle)
					# enter previous taks requirements in database (in xml format)
					elementsToInsert.extend(("Prev. Task Required",et.tostring(oneprevioustask)))
					elementsToInsert.insert(0,os.path.join(root,file))
					cur.execute('INSERT INTO RemovalRequirements values (?,?,?,?,?)',elementsToInsert)
	conn.commit()
	cur.close()
	conn.close()

# some errors in the xmls
#1)
#IOError: [Errno 2] No such file or directory: 'C:/Users/tbaer/Desktop/m777/data/IETM_PARSING/XML/TM25_P/DFCS/Antenna/.minstall.xml' 

#In [209]: thetree = et.parse(path + '/minstall.xml')
#  File "<string>", line unknown
#ParseError: not well-formed (invalid token): line 76, column 26

# because this line had no space between M60301_remove and callout=
 # <xref wpid="M60301_remove" callout=" removed" hovertext="RADIO ANTENNA (ANT), REMOVAL" push="1" />

 #2) DFCS system
# in w8 install, there are bullets that et cannot read (line 125ish)
#3) DFCS system
# in Nitrogen insp_repair, there are plus/minus that et cannot read (line 149)