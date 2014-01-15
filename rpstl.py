import csv

inf = open('TxtSlimFromTxtExportThroughFig7.txt', 'rb') # open input file for reading
#outI = open('rpstlInt.csv', 'wb') # open output file for writing
outf = open('rpstlOut.csv', 'wb') # open output file for writing
outw = csv.writer(outf) # open csv output writer

outl = ['Group', 'Group Name', 'SubGroup', 'SubGroup Name', 'STAR', 'Number','SMRC', 'NSN', 'CAGEC', 'PN', 'TEXT', 'QTY', 'UOC', 'Figure', 'Figure Name', ] # header row
outw.writerow( outl ) # write header row

# inf is of type "file"
# l is of type "str"
subgroup = ""
subgroupName = ""
uoc = ""

# initialize these outside the loop
y = 0 # switch to know if the loop will find something useful in the row; all useful types begin with a group and end with an "end of figure"
g = 0 # previous row is a group (to find rows that spill onto the next line)
sg = 0 # previous row is a subgroup
f = 0 # previous row is a figure
p = 0 # previous row is a part row
z = 0 # test

for l in inf:
# loop through lines in input file
	#print g
    l = l.lstrip() # eliminate initial whitespace
    l = l.replace( '\r', '' ) # strip carriage returns from input line
    l = l.replace( '\n', '' ) # strip newline characters from input line
    words = l.split( ' ' ) # split line into space-separated words
	
    print l
    #print len(words)

    star = 0 # no star unless it finds one
    if l.startswith( 'GROUP' ):
        print "found group"
        y = 1
        g = 1
        # found line for a new group
        group = words[1] # group is second word in line
        groupName = ''
        rng = range(2, len(words))
        for x in rng:
            groupName += words[x] + ' ' # rest is group name
        groupName = groupName.strip()
    elif l.startswith( 'END OF FIGURE'):
        # found the end of a table
        y = 0
        outl = [group, groupName, subgroup, subgroupName, star, fignum, smrc, nsn, cage, pn, txt, qty, uoc, figure, figureName]
        outw.writerow( outl ) # write output line
    elif l.startswith( 'UOC'):
        # used-on code
        uoc = l.split(":")[1] # UOC is one long line of text; splitting on the colon and taking 2nd argument gives everything after the UOC
        outl = [group, groupName, subgroup, subgroupName, star, fignum, smrc, nsn, cage, pn, txt, qty, uoc, figure, figureName]
        outw.writerow( outl ) # write output line
        p = 0 # UOC is just one line; no more part
    elif l.startswith( 'FIG' ):
        if y == 1: # if it's after a group
            print "found figure"
            f = 1
            g = 0 # no longer after a group
            figure = words[1]
            figure = figure.strip('.')
            figureName = ''
            rng = range(2, len(words))
            for x in rng:
                figureName += words[x] + ' ' # rest is figure name
            figureName = figureName.strip()
        else:
		    pass
    elif words[0].isdigit():
        # if this is some other line then ignore it
        if y==1:
		    # if first word is numeric, found a part line or a subgroup
            if g==1: # if the previous row is a group, this one is a subgroup
                print "found subgroup"
                subgroup = words[0]
                subgroupName = ''
                rng = range(2, len(words))
                for x in rng:
                    groupName += words[x] + ' ' # rest is group name
                subgroupName = subgroupName.strip()
                g = 0
                sg = 1			
            elif p==1: # if the previous row is a part and this one is a part, output a row before reading this one
                # make list of values for output line
                print "partPRINT"
                outl = [group, groupName, subgroup, subgroupName, star, fignum, smrc, nsn, cage, pn, txt, qty, uoc, figure, figureName]
                txt = ""
                nsn = ""
                cage = ""
                pn = ""
                qty = ""
                outw.writerow( outl ) # write output line
            else:	#it's a part
                star = 0
                p = 1
                s1 = 0 # need to add 1 to all counters in case the first "word" is a star
                fignum = words[0+s1] # number is 1st or second word
                smrc = words[1+s1] # SMRC is second or third word 
                nxtWord = 2+s1 # NSN is either third or fourth word
                nsn = words[nxtWord] # NSN is next
                # not every part has an NSN listed.  Test if value isn't an NSN.
                if len(nsn) != 13 or not nsn.isdigit():
                    nsn = '' # Not an NSN; set NSN to blank.
                else:
                    nxtWord += 1 # got NSN; index for next word (CAGE code)
                cage = words[nxtWord] # then CAGE code
                if cage.startswith('.'):
                    # missing cage code and pn
                    cage = ''
                    pn = ''
                else:
                    if not cage.isdigit() or len(cage) > 5:
                        # missing just cage code
                        pn = cage
                        cage = ''
                    else:
                        nxtWord += 1 # got cage code
                        while len(cage) < 5:
                            cage += words[nxtWord] # for cage code with spaces in it
                            nxtWord += 1
						
                    pn = words[nxtWord] # PN may be multiple words.  Get first word...
                    nxtWord += 1 # index of next word (more PN, or start of TEXT)

                    if len(words[nxtWord]) == 0:
                        # skip empty strings
                        nxtWord += 1
					
  #                  while words[nxtWord][0].isdigit():
  #                      # if next word starts with a digit, assume it's more PN
  #                      pn += ' ' + words[nxtWord] # add back space to PN
  #                      nxtWord += 1 # TEXT then starts with next word
                # TEXT is next word to second-to-last word
                # rng is range of ints from nxtWord to (# of words - 1)
                rng = range( nxtWord, len(words) - 1 ) 
                txt = '' # txt holds TEXT (start with empty string)
                for x in rng:
                    txt += words[x] + ' ' # add each word in range, and a space
                txt = txt[ : len(txt) - 1] # shave off trailing space
                # quantity is last word in line (second-to-last word index)
                qty = words[ len(words) - 2]
                print qty
                if not qty.isdigit():
                    txt += ' ' + qty  # if last word isn't numeric, is part of TEXT
                    qty = ''

                # test if start of TEXT contains part of PN
                if not txt.startswith('.') and txt.find(' .') != -1:
                    # if TEXT contains . but doesn't start with ., starts with PN part
                    dotidx = txt.find('.')
                    pn += txt[:dotidx] # before . is rest of PN
                    txt = txt[dotidx:] # rest is TEXT

                while txt.startswith('-'):
                    # if TEXT starts with a hyphen, starts with PN part
                    spaceidx = txt.find(' ') # index of first space
                    pn += ' ' + txt[:spaceidx] # before first space is rest of PN
                    txt = txt[spaceidx:].strip() # rest is TEXT (strip lead/trail space)

                if not txt.startswith('.'):
                    testing = True
                    while testing:
                        firstWord = txt[:txt.find(' ')] # get first word of TEXT
                        # strip hyphens and periods
                        firstWord = firstWord.replace('-','')
                        firstWord = firstWord.replace('.','')
                        if firstWord.find(',') != -1:
                            break
                        morePN = False # set true if first word of TEXT contains digit
                        for char in firstWord:
                            if char.isdigit():
                                morePN = True
                                break
                        if morePN:
                            spaceidx = txt.find(' ')
                            pn += ' ' + txt[:spaceidx]
                            txt = txt[spaceidx:].strip()
                        else:
                            testing = False # first word contains no digits

                txt = txt.strip('`')
                txt = txt.upper() # uppercase
                txt = txt.strip() # strip leading/trailing whitespace
                txt = txt.strip('.')
            g = 0
        else:
            pass # its a row to ignore
    elif l.startswith('*'):
        # it's a part row
        # if this is some other line then ignore it
        if y==1:
		    # if first word is numeric, found a part line or a subgroup
            if g==1: # if the previous row is a group, this one is a subgroup
                subgroup = words[0]
                subgroupName = ''
                rng = range(2, len(words))
                for x in rng:
                    groupName += words[x] + ' ' # rest is group name
                subgroupName = subgroupName.strip()
                g = 0
                sg = 1			
            elif p==1: # if the previous row is a part and this one is a part, output a row before reading this one
                # make list of values for output line
                outl = [group, groupName, subgroup, subgroupName, star, fignum, smrc, nsn, cage, pn, txt, qty, uoc, figure, figureName]
                outw.writerow( outl ) # write output line
            else:	#it's a part
                print "FOUND A STARRRRRRRRRRR"
                star = 1
                p = 1
                s1 = 1 # need to add 1 to all counters in case the first "word" is a star
                fignum = words[0+s1] # number is 1st or second word
                smrc = words[1+s1] # SMRC is second or third word (second SMRC code in line)
                nxtWord = 2+s1 # NSN is either third or fourth word
                nsn = words[nxtWord] # NSN is next
                # not every part has an NSN listed.  Test if value isn't an NSN.
                if len(nsn) != 13 or not nsn.isdigit():
                    nsn = '' # Not an NSN; set NSN to blank.
                else:
                    nxtWord += 1 # got NSN; index for next word (CAGE code)

                cage = words[nxtWord] # then CAGE code

                if cage.startswith('.'):
                    # missing cage code and pn
                    cage = ''
                    pn = ''
                else:
                    if not cage.isdigit() or len(cage) > 5:
                        # missing just cage code
                        pn = cage
                        cage = ''
                    else:
                        nxtWord += 1 # got cage code
                        while len(cage) < 5:
                            cage += words[nxtWord] # for cage code with spaces in it
                            nxtWord += 1
						
                    pn = words[nxtWord] # PN may be multiple words.  Get first word...
                    nxtWord += 1 # index of next word (more PN, or start of TEXT)

                    if len(words[nxtWord]) == 0:
                        # skip empty strings
                        nxtWord += 1
						
            #        while words[nxtWord][0].isdigit():
            #            # if next word starts with a digit, assume it's more PN
            #            pn += ' ' + words[nxtWord] # add back space to PN
            #            nxtWord += 1 # TEXT then starts with next word
				
                # TEXT is next word to second-to-last word
                # rng is range of ints from nxtWord to (# of words - 1)
                rng = range( nxtWord, len(words) - 1 ) 
                txt = '' # txt holds TEXT (start with empty string)
                for x in rng:
                    txt += words[x] + ' ' # add each word in range, and a space
                txt = txt[ : len(txt) - 1] # shave off trailing space
                # quantity is last word in line (second-to-last word index)
                qty = words[ len(words) - 1]
                if not qty.isdigit():
                    txt += ' ' + qty  # if last word isn't numeric, is part of TEXT
                    qty = ''

                # test if start of TEXT contains part of PN
                if not txt.startswith('.') and txt.find(' .') != -1:
                    # if TEXT contains . but doesn't start with ., starts with PN part
                    dotidx = txt.find('.')
                    pn += txt[:dotidx] # before . is rest of PN
                    txt = txt[dotidx:] # rest is TEXT

                while txt.startswith('-'):
                    # if TEXT starts with a hyphen, starts with PN part
                    spaceidx = txt.find(' ') # index of first space
                    pn += ' ' + txt[:spaceidx] # before first space is rest of PN
                    txt = txt[spaceidx:].strip() # rest is TEXT (strip lead/trail space)

                if not txt.startswith('.'):
                    testing = True
                    while testing:
                        firstWord = txt[:txt.find(' ')] # get first word of TEXT
                        # strip hyphens and periods
                        firstWord = firstWord.replace('-','')
                        firstWord = firstWord.replace('.','')
                        if firstWord.find(',') != -1:
                            break
                        morePN = False # set true if first word of TEXT contains digit
                        for char in firstWord:
                            if char.isdigit():
                                morePN = True
                                break
                        if morePN:
                            spaceidx = txt.find(' ')
                            pn += ' ' + txt[:spaceidx]
                            txt = txt[spaceidx:].strip()
                        else:
                            testing = False # first word contains no digits

                txt = txt.strip('`')
                txt = txt.upper() # uppercase
                txt = txt.strip() # strip leading/trailing whitespace
                txt = txt.strip('.')
            g = 0
        else:
            pass # its a row to ignore

    elif len(words)==1: #check for empty row
        #print "here"
        pass
    else: # a row that starts with text other than group, figure, UOC, or end of figure (could be extension of a name)
        if y ==1:
            if p ==1: # it's an extension of a part row
                txt += " " + l # add the text to the end of the part text
                txt = txt.strip('.') # strip trailing dots
            elif g ==1: # it's an extension of  a group row
                groupName += " " + l
                g=0
            elif f ==1: # it's an extension of  a figure row
                figureName += " " + l
                f=0
        else: 
            pass # (skip all other lines
    
inf.close()
outf.close()
