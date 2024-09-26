
# import testcm4
# testcm4.hello()

# import numpy as np
# from datetime import datetime, timedelta

# def datetime_to_mjd2000(date_str):
#     # Parse the input string into a datetime object
#     dt = datetime.strptime(date_str, '%Y-%m-%d %H:%M:%S')
    
#     # Define the MJD2000 epoch start date
#     mjd2000_epoch = datetime(2000, 1, 1, 0, 0, 0)
    
#     # Calculate the difference in days between the input date and the epoch
#     delta = dt - mjd2000_epoch
    
#     # Return the number of days as MJD2000 (including fractional part for time of day)
#     return delta.days + delta.seconds / 86400

# def mjd2000_to_year_decimal(t):
#     # Convert MJD2000 to calendar date
#     mjd_epoch = datetime(2000, 1, 1) + timedelta(days=t)
#     iy = mjd_epoch.year
#     im = mjd_epoch.month
#     id = mjd_epoch.day
#     ih = mjd_epoch.hour
#     minute = mjd_epoch.minute
#     sec = mjd_epoch.second

#     # Days of the month at the beginning of each month
#     dof0 = np.array([0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334])

#     # Check if it's a leap year
#     ifleapyr = (iy % 4 == 0) & ((iy % 100 != 0) | (iy % 400 == 0))

#     # Adjust for leap year
#     i2 = (im > 2) & ifleapyr

#     # Calculate UT in decimal years
#     ut = iy + (dof0[im - 1] + id - 1 + i2 + (ih + minute / 60 + sec / 3600) / 24) / (365 + ifleapyr)
    
#     return ut
# # import numpy as np
# cm4_cof_path = np.array([
#     "/Users/coka4389/Downloads/CM4/umdl.CM4",
#     "/Users/coka4389/Library/CloudStorage/OneDrive-UCB-O365/Desktop/CM4_Wrapper/CM4/lib/fake_dst.txt",
#     "/Users/coka4389/Library/CloudStorage/OneDrive-UCB-O365/Desktop/CM4_Wrapper/CM4/lib/fake_f107.txt"
# ])

# # cm4_cof_path = '/Users/coka4389/Downloads/CM4/umdl.CM4'
# UNIT = np.array([10,20,30])
# LOAD = np.array([True,False, False])
# INDX = np.array([False, False])
# GMUT = True#np.array([False, False])
# UT = 505544.50847222 #Modified Julian Date 51544.50847222 of 2000-01-01 12:12:12
# UT_time = mjd2000_to_year_decimal(datetime_to_mjd2000('1964-07-01 01:01:01'))

# MUT = 51544.50847222

# CORD = 1
# PRED = np.array([1,1,1,1,1,1])
# CURR = True
# COEF = False
# NHMF = np.array([13,45])
# NLMF = np.array([1,14])
# pos_len = 10
# alt = np.ones(pos_len)*100
# alt = 400
# theta = np.linspace(1,179,pos_len)
# theta = 2
# phi = np.linspace(-180,180, pos_len)
# phi = 180
# dst = -50
# f107 = 100
# bmdl = np.empty((3,7))
# jmdl = np.empty((3,4))
# gmdl = np.empty((3,7))
# perr = 1
# oerr = 50
# cerr = 0

# # out = testcm4.cm4field(cm4_cof_path, UNIT, LOAD, INDX, GMUT, CORD, PRED, CURR, COEF,NHMF, NLMF,UT_time, MUT, theta, phi, alt, dst, f107, bmdl, jmdl, gmdl, perr, oerr, cerr)
# # print(bmdl, jmdl, gmdl)
# # print(out)
# import importlib

import cm4_py310
import numpy as np
UT = 1964.49738353192675
thet = 44.0152  # Initialize if needed, e.g. theta = [(i, i=1, pos_len)]
phi = - 62.7482 
alt = 20
dst = 7
f107 = 723
out_arr_b = np.zeros((3,7))
out_arr_j = np.zeros((3,7))

out = cm4_py310.call_cm4(UT, thet, phi, alt, dst, f107)
print('here???',out)
print(UT)