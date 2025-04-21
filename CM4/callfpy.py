import os
from datetime import datetime, timedelta

from geomaglib.util import geod_to_geoc_lat, calc_dec_year, calc_dec_year_array
import numpy as np
from CM4 import cm4field
from CM4 import cm4field_arr

# import geomaglib
# Force reimport of the module
# importlib.reload(cm4_py310)
# import cm4_py310
import time

import re

curr_dir = os.path.dirname(__file__)
COF_PATH = os.path.join(curr_dir, "umdl.CM4")




def datetime_to_mjd2000(date_str):
    # Parse the input string into a datetime object
    dt = datetime.strptime(date_str, '%Y-%m-%d %H:%M:%S')
    
    # Define the MJD2000 epoch start date
    mjd2000_epoch = datetime(2000, 1, 1, 0, 0, 0)
    
    # Calculate the difference in days between the input date and the epoch
    delta = dt - mjd2000_epoch
    
    # Return the number of days as MJD2000 (including fractional part for time of day)
    return delta.days + delta.seconds / 86400

def jd2000(iy, im, id, ut=0):
    """
    jd2000 = jd2000(iy, im, id, ut)
    jd2000 = jd2000(iy, im, id)

    Input:
        iy : array-like or scalar
            Year [yyyy]
        im : array-like or scalar
            Month [1-12]
        id : array-like or scalar
            Day [1-31]
        ut : array-like or scalar, optional
            Time in hours [0-24] (default is 0)

    Output:
        jd2000 : numpy array
            Modified Julian Day 2000 (JD2000)
            (0.0 = January 1, 2000, 00:00 UTC)
    """
    
    # Convert inputs to numpy arrays for element-wise operations
    iy = np.asarray(iy)
    im = np.asarray(im)
    id = np.asarray(id)
    ut = np.asarray(ut)
    
    # Determine the maximum shape of input arrays
    max_shape = np.broadcast(iy, im, id, ut).shape
    
    # Expand inputs to the same shape if they are scalars
    if iy.size == 1:
        iy = np.full(max_shape, iy)
    if im.size == 1:
        im = np.full(max_shape, im)
    if id.size == 1:
        id = np.full(max_shape, id)
    if ut.size == 1:
        ut = np.full(max_shape, ut)
    
    # Check that all input arrays are of the same shape
    if not (iy.shape == im.shape == id.shape == ut.shape):
        raise ValueError("Variables must be of equal size (or scalars)")

    # Convert the date and time to Modified Julian Day 2000 (JD2000)
    jd2000_array = []
    for year, month, day, hours in zip(iy.flat, im.flat, id.flat, ut.flat):
        # Ensure Python int is used for timedelta
        year = int(year)
        month = int(month)
        day = int(day)
        hours = float(hours)  # This handles fractional hours properly
        
        # Compute datetime object and adjust for hours
        date = datetime(year, month, day) + timedelta(hours=hours)

        # Calculate the difference from MJD 2000 (January 1, 2000 00:00 UTC)
        jd2000 = (date - datetime(2000, 1, 1)).days + (date - datetime(2000, 1, 1)).seconds / 86400.0
        jd2000_array.append(jd2000)


    # Convert to numpy array to keep structure similar to input
    return np.array(jd2000_array).reshape(max_shape)


def mjd2000_to_ut(t):
    # t is MJD2000 (Modified Julian Date relative to Jan 1, 2000)
    
    # Define days offset for each month (for a non-leap year)
    dof0 = np.array([0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334])
    
    # Add 730486 to MJD2000 to get the date in standard MJD format
    mjd = t + 730486
    
    # Convert MJD to datetime
    reference_date = datetime(2000, 1, 1)  # Reference date for MJD
    date_time = reference_date + timedelta(days=mjd)
    
    # Extract year, month, day, hour, minute, second from datetime
    iy = date_time.year
    im = date_time.month
    id = date_time.day
    ih = date_time.hour
    minute = date_time.minute
    sec = date_time.second
    print("in here",iy, im, id, ih)
    # Determine if the year is a leap year
    ifleapyr = (iy % 4 == 0)  # 1 if leap year, 0 otherwise
    # Check if the month is greater than February and it is a leap year
    i2 = (im > 2) and ifleapyr
    
    # Compute UT in years
    ut = iy + (dof0[im - 1] + id - 1 + i2 + (ih + minute / 60 + sec / 3600) / 24) / (365 + ifleapyr)
    return ut - 2000
    # return (ut - 2000.002739726)/1.00000029 +0.000577188811121232

# Example usage
# t = 1964000  # Replace with the actual MJD2000 value
# tmp = jd2000(1964,7,1,1)
# ut = mjd2000_to_ut(tmp)
# print(ut,'ut')
def mjd2000_to_year_decimal(t):
    # Convert MJD2000 to calendar date
    mjd_epoch = datetime(2000, 1, 1) + timedelta(days=t)
    iy = mjd_epoch.year
    im = mjd_epoch.month
    id = mjd_epoch.day
    ih = mjd_epoch.hour
    minute = mjd_epoch.minute
    sec = mjd_epoch.second

    # Days of the month at the beginning of each month
    dof0 = np.array([0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334])

    # Check if it's a leap year
    ifleapyr = (iy % 4 == 0) & ((iy % 100 != 0) | (iy % 400 == 0))

    # Adjust for leap year
    i2 = (im > 2) & ifleapyr

    # Calculate UT in decimal years
    ut = iy + (dof0[im - 1] + id - 1 + i2 + (ih + minute / 60 + sec / 3600) / 24) / (365 + ifleapyr)
    
    return ut
# import numpy as np


def parse_time(time_str):

    year = int(time_str[0:4])
    month = int(time_str[4:6])
    day = int(time_str[6:8])
    # if(day == 0):
        # day =1
    hour = int(time_str[8:10])
    # if(hour ==0):
        # hour = 1
    minute = int(time_str[10:12])
    # second = int(time_str[12:14])
    
    return year, month, day, hour, minute
#Core read answers


def call_py_cm4(ymd_time, alt, lat_geod, lon, dst, f107, nmin, nmax, geoc_coord_flag, pred_flag_arr):
    #Change yyyymmddhhmmss time to Year decimal time
    year, month, day, hour, minute = parse_time(ymd_time)
    tmp = jd2000(year,month,day, hour + minute/60)
    UT = mjd2000_to_ut(tmp)
    # UT = calc_dec_year(year, month, day, hour = hour, minutes = minute)
    
    #Change geodetic lat/radius into geocentric
    if(geoc_coord_flag):
        r_geoc ,thet_geoc= geod_to_geoc_lat(np.deg2rad(lat_geod), alt)
        thet_geoc = np.rad2deg(thet_geoc)
        r_geoc = r_geoc-6371.2
    else:
        r_geoc, thet_geoc = alt, lat_geod

    out_b, out_j = cm4field.call_cm4(UT, thet_geoc , lon, r_geoc, dst, f107,
                                      pred_flag_arr[0],pred_flag_arr[1],pred_flag_arr[2],pred_flag_arr[3],pred_flag_arr[4],pred_flag_arr[5]
                                      ,geoc_coord_flag,
                                      nmax[0],nmax[1], nmin[0],nmin[1], COF_PATH)
    out_b = np.array(out_b)
    out_j = np.array(out_j)
    # print('core z,x,y \n with x and z with flipped signs\n----------------------------------\n',-out_b[2,0], -out_b[0,0],out_b[1,0])
    return out_b,out_j
    

# def Core_unit_test(filepath = 'test_values/Core_unittest_inputs.csv'):
#     time = '196407011059'
#     alt = 0
#     lat = 43
#     lon = 34
#     dst = 7
#     f107 = 465
#     nmin = np.array([1,14])
#     nmax =np.array([13,45])
#     pred = np.array([True,True,True,True,True,True])
#     cord = True
#     print('hello!!!')
#     call_py_cm4(time,alt,lat,lon,dst,f107,nmin,nmax,cord,pred)
#     print('finished!!!')
# def read_csv(filepath = 'Core_unittest_inputs.csv'):

def py_mat_cm4(alt, lat_geod, lon, dst, f107,geodflag = 1,ymd_time = None, MJD_time = None):
    if MJD_time is None and ymd_time is None:raise ValueError("a time input must be provided")
    #Change yyyymmddhhmmss time to Year decimal time
    print("py_mat_cm4_arr should be used even with scalars")
    #raise ValueError
    if ymd_time is not None:
        year, month, day, hour, minute = parse_time(ymd_time)
        # hour = hour - 1

        # tmp = jd2000(year,month,day, hour + minute/60)
        # UT = mjd2000_to_ut(tmp)
        # UT = calc_dec_year(year, month, day, hour = hour, minutes = minute)
        UT = calc_dec_year(year, month, day, hour, minute)

        # print(f"calc UT time", UT)
    else:
        UT = MJD_time

    # print(UT,1.990326027397260e3)
    # UT = 1.990326027397260e3
    #Change geodetic lat/radius into geocentric
    if(geodflag):
        r_geoc ,thet_geoc= geod_to_geoc_lat(np.deg2rad(lat_geod), alt)
        thet_geoc = np.rad2deg(thet_geoc)
        r_geoc = r_geoc-6371.2
        # r_geoc = #6.367857428238093e+03 - 6371.2
    else:
        r_geoc = alt
        thet_geoc = lat_geod
    # print(r_geoc, thet_geoc)
    nmin = np.array([1,14])
    nmax =np.array([13,45])
    pred = np.array([True,True,True,True,True,True])
    cord = False
    out_b, out_j = cm4field.call_cm4(UT, thet_geoc , lon, r_geoc, dst, f107,
                                      pred[0],pred[1],pred[2],pred[3],pred[4],pred[5]
                                      ,cord,
                                      nmax[0],nmax[1], nmin[0],nmin[1], COF_PATH)
    out_b = np.array(out_b)
    out_j = np.array(out_j)
    ionoshere = np.array([-out_b[2,4]-out_b[2,5], -out_b[0,4]-out_b[0,5],out_b[1,4]+out_b[1,5]])
    magnetosphere = np.array([-out_b[2,2]-out_b[2,3], -out_b[0,2]-out_b[0,3],out_b[1,2]+out_b[1,3]])
    core = np.array([-out_b[2,0], -out_b[0,0],out_b[1,0]])
    # print('core',core, "f", np.sqrt(core[0]**2 + core[1]**2 + core[2]**2))
    # print('magnetosphere',magnetosphere)
    # print('ionoshere', ionoshere)
    # print('raw', out_b)
    # print('core z,x,y \n with x and z with flipped signs\n----------------------------------\n',-out_b[2,0], -out_b[0,0],out_b[1,0])
    return out_b,out_j, core, magnetosphere, ionoshere

def py_mat_cm4_arr(alt, lat_geod, lon, dst, f107,pred = None, core_nmin = 1, core_nmax = 13, crust_nmin = 14, crust_nmax = 45, geodflag = 1,year = None, month = None, day = None, hour = None, minute = None, MJD_time = None):
    if MJD_time is None and year is None:raise ValueError("a time input must be provided")
    #Change yyyymmddhhmmss time to Year decimal time

    if pred is None:
        pred = np.array([True,True,True,True,True,True])

    if year is not None:
        # year, month, day, hour, minute = parse_time(ymd_time)
        # hour = hour - 1

        # tmp = jd2000(year,month,day, hour + minute/60)
        # UT = mjd2000_to_ut(tmp)

        UT = calc_dec_year_array(np.array(year), np.array(month), np.array(day), np.array(hour), np.array(minute))


        # print(f"calc UT time", UT)
    else:
        UT = MJD_time

    # print(UT,1.990326027397260e3)
    # UT = 1.990326027397260e3
    #Change geodetic lat/radius into geocentric
    if(geodflag):
        r_geoc ,thet_geoc= geod_to_geoc_lat(np.deg2rad(lat_geod), alt)
        thet_geoc = np.rad2deg(thet_geoc)
        r_geoc = r_geoc-6371.2
    else:
        r_geoc = alt
        thet_geoc = lat_geod
    # print(r_geoc, thet_geoc)
    
    nmin = np.array([core_nmin,crust_nmin])

    nmax =np.array([core_nmax,crust_nmax])
    # pred = np.array([True,True,True,True,True,True])
    cord = False

    out_b, out_j = cm4field_arr.call_cm4(UT, thet_geoc , lon, r_geoc, dst, f107,

                                      pred[0],pred[1],pred[2],pred[3],pred[4],pred[5]
                                      ,cord,
                                      nmax[0],nmax[1], nmin[0],nmin[1], len(UT), COF_PATH)
    out_b = np.array(out_b)
    out_j = np.array(out_j)
    ionoshere = np.array([-out_b[2,4]-out_b[2,5], -out_b[0,4]-out_b[0,5],out_b[1,4]+out_b[1,5]])
    magnetosphere = np.array([-out_b[2,2]-out_b[2,3], -out_b[0,2]-out_b[0,3],out_b[1,2]+out_b[1,3]])
    core = np.array([-out_b[2,0], -out_b[0,0],out_b[1,0]])
    crust = np.array([-out_b[2,1], -out_b[0,1],out_b[1,1]])
    # print('core',core, "f", np.sqrt(core[0]**2 + core[1]**2 + core[2]**2))
    # print('magnetosphere',magnetosphere)
    # print('ionoshere', ionoshere)
    # print('raw', out_b, np.shape(out_b))
    # print('core z,x,y \n with x and z with flipped signs\n----------------------------------\n',-out_b[2,0], -out_b[0,0],out_b[1,0])
    return out_b,out_j, core,crust, magnetosphere, ionoshere

def format_value(value):
    # Ensure values are two digits if less than 10
    return f"{int(value):02d}"

def create_YYYYMMDDHHMM(a, i):
    # Format each part of the date and time with leading zeros if needed
    year = format_value(a[i]['year'])
    month = format_value(a[i]['month'])
    day = format_value(a[i]['day'])
    hour = format_value(a[i]['hour'])
    minute = format_value(a[i]['min'])

    # Combine into the desired format
    YYYYMMDDHHMM = year + month + day + hour + minute
    return YYYYMMDDHHMM
