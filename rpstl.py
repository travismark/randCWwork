import csv

inf = open('TxtSlimFromTxtExportThroughFig79.txt', 'rb') # open input file for reading
#outI = open('rpstlInt.csv', 'wb') # open output file for writing
outf = open('rpstlOut.csv', 'wb') # open output file for writing
outw = csv.writer(outf) # open csv output writer

outl = ['Group', 'Group Name', 'SubGroup', 'SubGroup Name', 'STAR', 'Number','SMRC', 'NSN', 'CAGEC', 'PN', 'TEXT', 'QTY', 'UOC', 'Figure', 'Figure Name', ] # header row
outw.writerow( outl ) # write header row

# inf is of type "file"
# l is of type "str"

'''
# eliminate initial whitespace and empty 
for l in inf:
    #l = l.replace( '\r', '' ) # strip carriage returns from input line
    #l = l.replace( '\n', '' ) # strip newline characters from input line
    #words = l.split( ' ' ) # split line into space-separated words (empty lines will have len(words) = 1)
    l = l.lstrip()
    outI.write(l)
	

    if len(words)>1:
        ii = 0
        for linewords in words:
            if linewords[ii]==" ":
                print "space"
                ii = ii + 1
            else:
                print linewords[ii]
                print ii
                ii = ii + 1
    else: # empty row
        pass # do nothing
		#if len(words)>10:   # words are actual words
    #    print words[10]
    outw.writerow(l)
	
'''



for l in inf:
    l = l.lstrip() # eliminate initial whitespace
    l = l.replace( '\r', '' ) # strip carriage returns from input line
    l = l.replace( '\n', '' ) # strip newline characters from input line

    words = l.split( ' ' ) # split line into space-separated words
	
# loop through lines in input file
    y = 0 # switch to know if the loop will find something useful in the row
    g = 0 # switch to know if the loop just came from a group (to find rows that spill onto the next line)
    sg = 0 # previous row is a subgroup
    p = 0 # switch to know if the loop just came from a part row
    star = 0 # no star unless it finds one
    if l.startswith( 'Group' ):
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
    elif l.startswith( 'UOC'):
        # used-on code
        uoc = l.split(":")[1] # UOC is one long line of text; splitting on the colon and taking 2nd argument gives everything after the UOC
        outl = [group, groupName, subgroup, subgroupName, star, num, smrc, nsn, cage, pn, txt, qty, uoc, figure, figureName]
        outw.writerow( outl ) # write output line
        p = 0 # UOC is just one line; no more part
    elif l.startswith('Fig'):
        if y == 1: # if it's after a group
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
                outl = [group, groupName, subgroup, subgroupName, star, num, smrc, nsn, cage, pn, txt, qty, uoc, figure, figureName]
                outw.writerow( outl ) # write output line
            else:	#it's a part
                p = 1
                s1 = 0 # need to add 1 to all counters in case the first "word" is a star
'''
                if words[0] == "*": # a star		
                    star = 1
                    s1 = 1
                '''
                num = words[0+s1] # number is 1st or second word
                smrc = words[1+s1] # SMRC is second or third word (second SMRC code in line)
	'''
                if not smrc.isalpha():
                # some lines only have SMRC as second word (missing second SMRC)
                    smrc = words[1]
                    nxtWord = 2
                else:
                    # got 2 SMRC's; NSN is word at index 3
                    nxtWord = 3
'''			
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
						
                    while words[nxtWord][0].isdigit():
                        # if next word starts with a digit, assume it's more PN
                        pn += ' ' + words[nxtWord] # add back space to PN
                        nxtWord += 1 # TEXT then starts with next word
				
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
                outl = [group, groupName, subgroup, subgroupName, star, num, smrc, nsn, cage, pn, txt, qty, uoc, figure, figureName]
                outw.writerow( outl ) # write output line
            else:	#it's a part
                p = 1
                s1 = 1 # need to add 1 to all counters in case the first "word" is a star
'''
                if words[0] == "*": # a star		
                    star = 1
                    s1 = 1
                '''
                num = words[0+s1] # number is 1st or second word
                smrc = words[1+s1] # SMRC is second or third word (second SMRC code in line)
	'''
                if not smrc.isalpha():
                # some lines only have SMRC as second word (missing second SMRC)
                    smrc = words[1]
                    nxtWord = 2
                else:
                    # got 2 SMRC's; NSN is word at index 3
                    nxtWord = 3
'''			
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
						
                    while words[nxtWord][0].isdigit():
                        # if next word starts with a digit, assume it's more PN
                        pn += ' ' + words[nxtWord] # add back space to PN
                        nxtWord += 1 # TEXT then starts with next word
				
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
    else:
        pass # (skip all other lines
    
inf.close()
outf.close()
'''