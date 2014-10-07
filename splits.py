# split excel files into different csvs

import xlrd
import csv

def csv_from_excel(wbname,shname):
    wb = xlrd.open_workbook(wbname+'.xlsx')
    sh = wb.sheet_by_name(shname)
    your_csv_file = open(shname + '.csv', 'wb')
    wr = csv.writer(your_csv_file, quoting=csv.QUOTE_ALL)
    for rownum in xrange(sh.nrows):
        wr.writerow(sh.row_values(rownum))
    your_csv_file.close()


wb = xlrd.open_workbook('C:\\Users\\tbaer\\Desktop\\m1a1\\M1A1 Alion RAW EROs.xlsx')
for ii in wb.sheets():
    sh = wb.sheet_by_name(ii.name)
    the_csv_file = open(ii.name + '.csv', 'wb')
    wr = csv.writer(the_csv_file, quoting=csv.QUOTE_ALL)
    for rownum in xrange(sh.nrows):
            wr.writerow(sh.row_values(rownum))
    the_csv_file.close() 


##########################################################################
# a script to loop over files in a directory and pull something from excel

#from win32com.client import Dispatch
import os
import xlrd
import csv

#xlApp = Dispatch("Excel.Application")
path = 'C:/Users/tbaer/Desktop/m1a1/readiness'

listing = os.listdir(path) # listing will be a list of strings
# Make a list of xls files found in chosen path
xls_list = []

for filename in listing:
    name_parts = filename.split('.')
    ext = name_parts.pop() 
    if ext == 'xls' or ext == 'xlsx':
        xls_list.append(filename)

# some of these reports have 'tank' in the third line, like favorites = pdm tanks, can't search on this

the_csv_file = open('readinessPut.csv', 'wb')
wr = csv.writer(the_csv_file, quoting=csv.QUOTE_ALL)
wr.writerow(['filename','year','month','day','r-rating','1stBn','2ndBn','4thBn'])
        
for filename in xls_list:
    filepath = path + '/' + filename
    wb = xlrd.open_workbook(filepath)
    sh1 = wb.sheet_by_index(0) # the summary sheet
    #sh2 = wb.sheet_by_name('E1888') # the m1a1 sheet
    sh2 = wb.sheet_by_index(1)
    print filename
    # find the m1a1 row
    ii = 0
    for ii in range(1,10):
        if type(sh1.cell(ii,0).value)==float:
            if sh1.cell(ii,0).value > 40000:
                # it's a date, record
                sheetdatetup=xlrd.xldate_as_tuple(sh1.cell(ii,0).value,wb.datemode)
                print xlrd.xldate_as_tuple(sh1.cell(ii,0).value,wb.datemode)
                break
    for jj in range(ii+1,20): # now look after this for the tank
        try: 
            rowb=sh1.cell(jj,0).value.upper().find('TANK')
        except: # do nothing
            rowb=-1
        if rowb > 0:
            break
    print jj
    if rowb + rowc == -2: # did not match
        jj = 0# do nothing
    else:
        print sh1.cell(jj,8)
    # second sheet
    a1 = 0
    a2 = 0
    a4 = 0
    for ii in range(sh2.nrows):
        if type(sh2.cell(ii,0).value)==unicode:
            if sh2.cell(ii,0).value.upper().find('1ST TANK BN')>=0:
                a1 += 1
    for ii in range(sh2.nrows):
        if type(sh2.cell(ii,0).value)==unicode:
            if sh2.cell(ii,0).value.upper().find('2D TANK BN')>=0:
                a2 += 1
    for ii in range(sh2.nrows):
        if type(sh2.cell(ii,0).value)==unicode:
            if sh2.cell(ii,0).value.upper().find('4TH TANK BN')>=0:
                a4 += 1  
    wr.writerow([filename,sheetdatetup[0],sheetdatetup[1],sheetdatetup[2],sh1.cell(jj,8).value,a1,a2,a4])

the_csv_file.close()