# Python Version: 3.6.1
"""
Program uses RA and DEC measurements for WISE catalog objects and matches them with objects in the SIMBAD catalog.
Each match result is stored in the following format:
[SIMBAD object type ID, associated WISE RA measurement, associated WISE DEC measurement]
"""
import astropy
from astroquery.simbad import Simbad
from astropy.coordinates import SkyCoord
import astropy.units as u
import csv
import os
import glob

Simbad.reset_votable_fields()

WISE_positions = r'C:\Users\Joe\Documents\Astro_Research_2017\ALLWISE_RA_DEC' #path to file containing WISE ra and dec text file(s)
outputfile = r'C:\Users\Joe\Documents\Astro_Research_2017\matches_original.csv'        #csv file where match results written to

# loop through all RA and DEC text files in given directory
coordinates = []
path =  WISE_positions
for infile in glob.glob( os.path.join(path, '*.txt') ):
    # import RA and DEC measurements from WISE text file where measurements are separated by a comma
    with open(infile) as inputfile:
        for line in inputfile:
            coordinates.append(line.strip().split(','))

simbad_matches = []
real_match_data = [] #contains match results only, disregards "NoneType" i.e. no match
coordinate_save = [] #corresponding ALLWISE (RA,DEC) for SIMBAD match
i = 0
for row in coordinates:
    coord = row # single RA and DEC measurement
    simbad_matches.append(Simbad.query_region(SkyCoord(coord[0], coord[1], unit=(u.deg, u.deg), frame='fk5'), radius = 1*u.arcsec))
    #discard results with no matches
    if(type(simbad_matches[i]) == astropy.table.table.Table): 
        real_match_data.append(simbad_matches[i])
        coordinate_save.append(coord) # save coordinates for future
    i+=1

#get only MAIN_ID, RA, and DEC from SIMBAD matches and store it in id_ra_dec list
j = 0
id_ra_dec = []
for table_result in real_match_data:
    temp = []
    decoded_id = table_result['MAIN_ID', 'RA', 'DEC'].as_array()[0][0].decode() #decode SIMBAD ID from byte type to string
    temp.append(decoded_id) # MAIN_ID
    temp.append(coordinate_save[j][0]) #RA
    temp.append(coordinate_save[j][1]) #DEC
    
    #these two lines are for storing the SIMBAD RA and DEC instead of the WISE ones
    #temp.append(table_result['MAIN_ID', 'RA', 'DEC'].as_array()[0][1]) #RA
    #temp.append(table_result['MAIN_ID', 'RA', 'DEC'].as_array()[0][2]) #DEC
    
    id_ra_dec.append(temp)
    j += 1
    
#write id_ra_dec to csv file
with open(outputfile, 'w') as f:
    writer = csv.writer(f)
    writer.writerows(id_ra_dec)