# Python Version: 3.6.1
"""
Program takes in RA and DEC measurements associated with SIMBAD matches, and uses
IRSA astroquery to get ALLWISE mpro, sigmpro, snr, and rchi2 for W1,W2,W3, and W4
"""

from astroquery.irsa import Irsa
from astropy.coordinates import SkyCoord
import astropy.units as u
import csv

SIMBAD_positions = r'C:\Users\Joe\Documents\Astro_Research_2017\SIMBAD_Matches_1 - matches.csv' #path to dir containing SIMBAD ra and dec csv file
WISE_info_output = r'C:\Users\Joe\Documents\Astro_Research_2017\allwise_info.csv'

obj_ra_dec = []
with open(SIMBAD_positions) as radec:
    readCSV = csv.reader(radec, delimiter=',')
    for row in readCSV:
        obj_ra_dec.append(row)

# Using ra and dec from obj_ra_dec list, get WISE information for each object
# and append to WISE_info list
i = 0
WISE_info = []
for row in obj_ra_dec:
    RA = float(row[1])
    DEC = float(row[2])
    query = Irsa.query_region(SkyCoord(RA, DEC, unit=(u.deg, u.deg), frame='fk5'), radius = 1*u.arcsec,catalog=('allwise_p3as_psd'))
    i += 1
    print(i)
    WISE_info.append(query)
    

# Get only specific information from each WISE object and append it to new list
WISE_specifics = []
for table_result in WISE_info:
    temp_info = table_result['w1mpro','w1sigmpro','w1snr','w1rchi2',
                             'w2mpro','w2sigmpro','w2snr','w2rchi2',
                             'w3mpro','w3sigmpro','w3snr','w3rchi2',
                             'w4mpro','w4sigmpro','w4snr','w4rchi2'].as_array()[0]
    WISE_specifics.append(temp_info)

with open(WISE_info_output, 'w') as f:
    writer = csv.writer(f)
    writer.writerows(WISE_specifics)