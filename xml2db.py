from AccessManipulator import AccessManipulator
import argparse
from queries import qry_platform_insert, qry_weibull_insert
from xmlparsing import get_init_document, get_platform_rows, get_weibull_rows

# parse the command line
parser = argparse.ArgumentParser(description='Take initialization info from xml file and store in ATLAST Access database')
parser.add_argument('-i','--initfile', help='Path to xml initialization table', required=True)
parser.add_argument('-db','--database', help='ATLAST Access database to store initialization table', required=True)
args = vars(parser.parse_args())

# parse the xml document
print "parsing", args['initfile']
init_document = get_init_document(args['initfile'])
platform_rows = get_platform_rows(init_document)
weibull_rows = get_weibull_rows(init_document)

# store in database
db = AccessManipulator(args['database'])
print "storing", len(platform_rows), "platforms"
db.executemany(qry_platform_insert, platform_rows)
print "storing", len(weibull_rows), "weibulls"
db.executemany(qry_weibull_insert, weibull_rows)
db.close_connection()