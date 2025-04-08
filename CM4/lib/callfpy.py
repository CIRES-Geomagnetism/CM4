from datetime import datetime, timedelta
import geomaglib.util
import numpy as np
import importlib
import csv
import cm4_py310
import cm4_py310_arr
# import geomaglib
# Force reimport of the module
importlib.reload(cm4_py310)
# import cm4_py310
import time

import re

def parse_bmdl_output(file_name):
    # Initialize an empty list to hold all BMDL values
    bmdl_values = []
    # Open and read the file
    with open(file_name, 'r') as file:
        # Read through the file line by line
        for line in file:
            # Search for the BMDL Output line using regex
            match = re.search(r'BMDL Output:\s*(.*)', line)
            if match:
                # Extract the numbers from the matched line
                bmdl_line = match.group(1)
                # Convert the string of numbers into a list of floats
                bmdl_numbers = list(map(float, bmdl_line.split()))
                # Append these numbers to the bmdl_values list
                bmdl_values.append(bmdl_numbers)

    # Convert the list of BMDL values into a numpy array
    bmdl_array = np.array(bmdl_values)

    return bmdl_array



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
        #roste
        a = 3
        print(a)
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
    
    # Determine if the year is a leap year
    ifleapyr = (iy % 4 == 0)  # 1 if leap year, 0 otherwise
    
    # Check if the month is greater than February and it is a leap year
    i2 = (im > 2) and ifleapyr
    
    # Compute UT in years
    ut = iy + (dof0[im - 1] + id - 1 + i2 + (ih + minute / 60 + sec / 3600) / 24) / (365 + ifleapyr)
    
    return (ut - 2000.002739726)/1.00000029 +0.000577188811121232

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
cm4_cof_path = np.array([
    "/Users/coka4389/Downloads/CM4/umdl.CM4",
    "/Users/coka4389/Library/CloudStorage/OneDrive-UCB-O365/Desktop/CM4_Wrapper/CM4/lib/fake_dst.txt",
    "/Users/coka4389/Library/CloudStorage/OneDrive-UCB-O365/Desktop/CM4_Wrapper/CM4/lib/fake_f107.txt"
])

def geod2geoc(alpha, h, *args):
    """
    Conversion from geodetic latitude to geocentric radius and colatitude.
    
    Parameters:
    alpha : float
        Geodetic latitude in radians.
    h : float
        Altitude in kilometers.
    X, Z : float, optional
        Geodetic magnetic field components.
        
    Returns:
    r : float
        Geocentric radius in kilometers.
    theta : float
        Geocentric colatitude in radians.
    B_r, B_theta : float, optional
        Geocentric magnetic field components.
    """
    
    # Ellipsoid parameters after GRS 80
    a = 6378.14  # km
    b = 6356.75  # km
    
    sin_alpha_2 = np.sin(alpha)**2
    cos_alpha_2 = np.cos(alpha)**2
    
    tmp = h * np.sqrt(a**2 * cos_alpha_2 + b**2 * sin_alpha_2)
    beta = np.arctan((tmp + b**2) / (tmp + a**2) * np.tan(alpha))
    theta = np.pi / 2 - beta
    
    r = np.sqrt(h**2 + 2 * tmp + (a**2 * (1 - (1 - (b/a)**4) * sin_alpha_2)) / (1 - (1 - (b/a)**2) * sin_alpha_2))
    
    # If X and Z are provided, convert magnetic components as well
    if len(args) == 2:
        X = args[0]
        Z = args[1]
        psi = np.sin(alpha) * np.sin(theta) - np.cos(alpha) * np.cos(theta)
        B_r = -np.sin(psi) * X - np.cos(psi) * Z
        B_theta = -np.cos(psi) * X + np.sin(psi) * Z
        return r, theta, B_r, B_theta
    
    return r, theta
def parse_time(time_str):
    print(time_str, type(time_str))
    year = int(time_str[0:4])
    month = int(time_str[4:6])
    day = int(time_str[6:8])
    if(day == 0):
        day =1
    hour = int(time_str[8:10])
    if(hour ==0):
        hour = 1
    minute = int(time_str[10:12])
    # second = int(time_str[12:14])
    
    return year, month, day, hour, minute
#Core read answers
def read_answers(filepath = 'CM4/lib/testvalBcore.csv'):
    data = []
    with open(filepath, mode='r') as file:
        reader = csv.reader(file)
        for row_index, row in enumerate(reader):
            data.append(row)
            data[-1][1] = data[-1][1][1:]
            data[-1][2] = data[-1][2][1:]
            
            # print(data[-1])
            for i in range(0,len(data[-1])):
                data[-1][i] = float(data[-1][i])
    return data

def read_answers_ext(filepath = 'CM4/lib/testvalBcore.csv'):
    data = []
    with open(filepath, mode='r') as file:
        reader = csv.reader(file)
        for row_index, row in enumerate(reader):
            data.append(row)
            # data[-1][1] = data[-1][1]
            # data[-1][2] = data[-1][2]
            
            # print(data[-1])
            for i in range(0,len(data[-1])):
                data[-1][i] = float(data[-1][i])
    return data


# UT = 1964.49738353192675
def py_mat_cm4_unittest_core(ymd_time, alt, lat_geod, lon, dst, f107):
    #Change yyyymmddhhmmss time to Year decimal time
    year, month, day, hour, minute = parse_time(ymd_time)
    tmp = jd2000(year,month,day, hour + minute/60)
    UT = mjd2000_to_ut(tmp)
    
    #Change geodetic lat/radius into geocentric
    r_geoc ,thet_geoc= geod2geoc(np.deg2rad(lat_geod), alt)
    thet_geoc = np.rad2deg(thet_geoc)
    r_geoc = r_geoc-6371.2
    nmin = np.array([1,14])
    nmax =np.array([13,45])
    pred = np.array([True,True,True,True,True,True])
    cord = False
    out_b, out_j = cm4_py310.call_cm4(UT, thet_geoc , lon, r_geoc, dst, f107,
                                      pred[0],pred[1],pred[2],pred[3],pred[4],pred[5]
                                      ,cord,
                                      nmax[0],nmax[1], nmin[0],nmin[1])
    out_b = np.array(out_b)
    out_j = np.array(out_j)
    # print('core z,x,y \n with x and z with flipped signs\n----------------------------------\n',-out_b[2,0], -out_b[0,0],out_b[1,0])
    return out_b,out_j

    
def py_mat_cm4_unittest_ext(ymd_time, alt, lat_geod, lon, dst, f107,geodflag = 1):
    #Change yyyymmddhhmmss time to Year decimal time
    year, month, day, hour, minute = parse_time(ymd_time)
    hour = hour - 1

    tmp = jd2000(year,month,day, hour + minute/60)
    UT = mjd2000_to_ut(tmp)

    # print(UT,1.990326027397260e3)
    # UT = 1.990326027397260e3
    #Change geodetic lat/radius into geocentric
    if(geodflag):
        r_geoc ,thet_geoc= geod2geoc(np.deg2rad(lat_geod), alt)
        thet_geoc = np.rad2deg(thet_geoc)
        r_geoc = r_geoc-6371.2
        # r_geoc = #6.367857428238093e+03 - 6371.2
    else:
        r_geoc = alt
        thet_geoc = lat_geod
    nmin = np.array([1,14])
    nmax =np.array([13,45])
    pred = np.array([True,True,True,True,True,True])
    cord = False
    out_b, out_j = cm4_py310.call_cm4(UT, thet_geoc, lon, r_geoc, dst, f107,
                                      pred[0],pred[1],pred[2],pred[3],pred[4],pred[5]
                                      ,cord,
                                      nmax[0],nmax[1], nmin[0],nmin[1])
    out_b = np.array(out_b)
    out_j = np.array(out_j)
    # print('core z,x,y \n with x and z with flipped signs\n----------------------------------\n',-out_b[2,0], -out_b[0,0],out_b[1,0])
    return out_b,out_j

def call_py_cm4(ymd_time, alt, lat_geod, lon, dst, f107, nmin, nmax, geoc_coord_flag, pred_flag_arr):
    #Change yyyymmddhhmmss time to Year decimal time
    year, month, day, hour, minute = parse_time(ymd_time)
    tmp = jd2000(year,month,day, hour + minute/60)
    UT = mjd2000_to_ut(tmp)
    
    #Change geodetic lat/radius into geocentric
    if(geoc_coord_flag):
        r_geoc ,thet_geoc= geod2geoc(np.deg2rad(lat_geod), alt)
        thet_geoc = np.rad2deg(thet_geoc)
        r_geoc = r_geoc-6371.2
    else:
        r_geoc, thet_geoc = alt, lat_geod

    out_b, out_j = cm4_py310.call_cm4(UT, thet_geoc , lon, r_geoc, dst, f107,  
                                      pred_flag_arr[0],pred_flag_arr[1],pred_flag_arr[2],pred_flag_arr[3],pred_flag_arr[4],pred_flag_arr[5]
                                      ,geoc_coord_flag,
                                      nmax[0],nmax[1], nmin[0],nmin[1] )
    out_b = np.array(out_b)
    out_j = np.array(out_j)
    # print('core z,x,y \n with x and z with flipped signs\n----------------------------------\n',-out_b[2,0], -out_b[0,0],out_b[1,0])
    return out_b,out_j
    
# time = '196407011059'
# alt = 0
# lat = 43
# lon = 34
# dst = 7
# f107 = 465
# nmin = np.array([1,14])
# nmax =np.array([13,45])
# pred = np.array([True,True,True,True,True,True])
# cord = True
# print('hello!!!')
# call_py_cm4(time,alt,lat,lon,dst,f107,nmin,nmax,cord,pred)
# print('finished!!!')
def Core_unit_test(filepath = '/Users/coka4389/Library/CloudStorage/OneDrive-UCB-O365/Desktop/CM4_Wrapper/CM4/lib/Core_unittest_inputs.csv'):
    data = []
        
    with open(filepath, mode='r') as file:
        reader = csv.reader(file)
        for row_index, row in enumerate(reader):
            if 1 <= row_index <= 14:  # Rows 1 to 14 (inclusive)
                data.append(row[1:4])  # Columns 1 to 3 (inclusive)
                for val in range(0,2):
                    data[-1][val] = float(data[-1][val])
                data[-1][2] = float(data[-1][2])
                data[-1][2] = str(data[-1][2])
                # print(data[-1])
    answers = read_answers()
    for i in range(0,len(data)):
        lat = data[i][0]
        lon = data[i][1]
        yyyymmddhhmm = data[i][2]
        out_b,out_j = py_mat_cm4_unittest_core(yyyymmddhhmm,0,lat,lon,7,723)
        core = np.array([-out_b[2,0], -out_b[0,0],out_b[1,0]])
        # print(answers[i], core)
        if(not np.all(np.isclose(answers[i],core,rtol = 1e-4))):
            print('Do matlab and py cm4_core dont agree to 5 digits',np.isclose(answers[i],core,rtol = 1e-4), answers[i], core)
        else:
            print('core unittest passed')

        # else:
        #     print('core agrees to 5 digits')
#Core_unit_test()#=======================

def Magnetosphere_Unit_test(filepath = '/Users/coka4389/Library/CloudStorage/OneDrive-UCB-O365/Desktop/CM4_Wrapper/CM4/lib/Core_unittest_inputs.csv'):
    data = []
        
    with open(filepath, mode='r') as file:
        reader = csv.reader(file)
        for row_index, row in enumerate(reader):
            if 1 <= row_index <= 14:  # Rows 1 to 14 (inclusive)
                data.append(row[1:4])  # Columns 1 to 3 (inclusive)
                for val in range(0,2):
                    data[-1][val] = float(data[-1][val])
                data[-1][2] = float(data[-1][2])
                data[-1][2] = str(data[-1][2])
                # print(data[-1])
    answers = read_answers_ext('CM4/lib/testvalB_magn.csv')
    # print()
    for i in range(0,len(data)):
        lat = data[i][0]
        lon = data[i][1]
        yyyymmddhhmm = data[i][2]
        dst = np.array([-4,-4,-4,-4,-4,-4,-84,-84,-84,-84,-84,-84,-84])
        f107 = np.array([63.2,63.2,63.2,63.2,63.2,63.2,171.3,171.3,171.3,171.3,171.3,171.3,171.3])
        # f107 = np.ones(len(f107))*10
        # dst = np.ones(len(f107))*-40
        
        out_b,out_j = py_mat_cm4_unittest_ext(yyyymmddhhmm,0,lat,lon,dst[i],f107[i],1)
        magnetosphere = np.array([-out_b[2,2]-out_b[2,3], -out_b[0,2]-out_b[0,3],out_b[1,2]+out_b[1,3]])
        if(not np.all(np.isclose(answers[i],magnetosphere,rtol = 1e-4))):
            print('Do matlab and py cm4_magnetosphere dont agree to 5 digits',np.isclose(answers[i],magnetosphere,rtol = 1e-4), answers[i], magnetosphere)
        else:
            print('magnetosphere unittest passed')
            

def Ionosphere_Unit_test(filepath = '/Users/coka4389/Library/CloudStorage/OneDrive-UCB-O365/Desktop/CM4_Wrapper/CM4/lib/Core_unittest_inputs.csv'):
    data = []
        
    with open(filepath, mode='r') as file:
        reader = csv.reader(file)
        for row_index, row in enumerate(reader):
            if 1 <= row_index <= 14:  # Rows 1 to 14 (inclusive)
                data.append(row[1:4])  # Columns 1 to 3 (inclusive)
                for val in range(0,2):
                    data[-1][val] = float(data[-1][val])
                data[-1][2] = float(data[-1][2])
                data[-1][2] = str(data[-1][2])
    answers = read_answers_ext('CM4/lib/testvalB_iono.csv')
    for i in range(0,len(data)):
        lat = data[i][0]
        lon = data[i][1]
        yyyymmddhhmm = data[i][2]
        dst = np.array([-4,-4,-4,-4,-4,-4,-84,-84,-84,-84,-84,-84,-84])
        f107 = np.array([63.2,63.2,63.2,63.2,63.2,63.2,171.3,171.3,171.3,171.3,171.3,171.3,171.3])
        # f107 = np.ones(len(f107))*10
        # dst = np.ones(len(f107))*-40
        print(yyyymmddhhmm, type(yyyymmddhhmm))
        out_b,out_j = py_mat_cm4_unittest_ext(yyyymmddhhmm,0,lat,lon,dst[i],f107[i],1)
        ionosphere = np.array([-out_b[2,4]-out_b[2,5], -out_b[0,4]-out_b[0,5],out_b[1,4]+out_b[1,5]])
        print(ionosphere)
        if(not np.all(np.isclose(answers[i],ionosphere,rtol = 1e-4))):
            print('Do matlab and py cm4_Ionosphere dont agree to 5 digits',np.isclose(answers[i],ionosphere,rtol = 1e-4), answers[i], magnetosphere)
        else:
            print('Ionosphere unittest passed')
            
        # if(i >= len(data)-2 or i < 1):
        #     print(f'==============={i}=============')
        #     print(out_b)
        #     print(magnetosphere)
        #     print('cm4^matv')
        #     print(answers[i])#, core)
        #     print('Do matlab and py cm4_magnetosphere agree to 5 digits',np.isclose(answers[i],magnetosphere,rtol = 1e-4))
   
def py_mat_cm4(alt, lat_geod, lon, dst, f107,geodflag = 1,ymd_time = None, MJD_time = None):
    if MJD_time is None and ymd_time is None:raise ValueError("a time input must be provided")
    #Change yyyymmddhhmmss time to Year decimal time

    if ymd_time is not None:
        year, month, day, hour, minute = parse_time(ymd_time)
        # hour = hour - 1

        tmp = jd2000(year,month,day, hour + minute/60)
        UT = mjd2000_to_ut(tmp)
        # print(f"calc UT time", UT)
    else:
        UT = MJD_time

    # print(UT,1.990326027397260e3)
    # UT = 1.990326027397260e3
    #Change geodetic lat/radius into geocentric
    if(geodflag):
        r_geoc ,thet_geoc= geod2geoc(np.deg2rad(lat_geod), alt)
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
    out_b, out_j = cm4_py310.call_cm4(UT, thet_geoc , lon, r_geoc, dst, f107,
                                      pred[0],pred[1],pred[2],pred[3],pred[4],pred[5]
                                      ,cord,
                                      nmax[0],nmax[1], nmin[0],nmin[1])
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

def py_mat_cm4_arr(alt, lat_geod, lon, dst, f107,core_nmin = 1, core_nmax = 13, crust_nmin = 14, crust_nmax = 45, geodflag = 1,year = None, month = None, day = None, hour = None, minute = None, MJD_time = None):
    if MJD_time is None and year is None:raise ValueError("a time input must be provided")
    #Change yyyymmddhhmmss time to Year decimal time

    if year is not None:
        # year, month, day, hour, minute = parse_time(ymd_time)
        # hour = hour - 1

        tmp = jd2000(year,month,day, hour + minute/60)
        UT = mjd2000_to_ut(tmp)
        # print(f"calc UT time", UT)
    else:
        UT = MJD_time

    # print(UT,1.990326027397260e3)
    # UT = 1.990326027397260e3
    #Change geodetic lat/radius into geocentric
    if(geodflag):
        r_geoc ,thet_geoc= geod2geoc(np.deg2rad(lat_geod), alt)
        thet_geoc = np.rad2deg(thet_geoc)
        r_geoc = r_geoc-6371.2
        # r_geoc = #6.367857428238093e+03 - 6371.2
    else:
        r_geoc = alt
        thet_geoc = lat_geod
    # print(r_geoc, thet_geoc)
    nmin = np.array([core_nmin,crust_nmin])
    nmax =np.array([core_nmax,crust_nmax])
    pred = np.array([True,True,True,True,True,True])
    cord = False
    out_b, out_j = cm4_py310_arr.call_cm4(UT, thet_geoc , lon, r_geoc, dst, f107,
                                      pred[0],pred[1],pred[2],pred[3],pred[4],pred[5]
                                      ,cord,
                                      nmax[0],nmax[1], nmin[0],nmin[1], len(UT))
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
def parse_survey_file(filename):
    with open(filename, 'r') as file:
        # Read the first line as headers
        headers = file.readline().strip().split()
        
        # Read the remaining lines as data
        data = []
        for line in file:
            values = line.strip().split()
            entry = dict(zip(headers, values))
            data.append(entry)
    
    return data
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

def parse_file_to_dict(filename):
    data = {}

    with open(filename, 'r') as file:
        for line in file:
            # Split the line by spaces and extract year, month, and value
            parts = line.strip().split()
            if len(parts) == 3:
                year, month, value = parts
                year, month = int(year), int(month)
                value = float(value)
                
                # Store the value in a dictionary where the key is a tuple of (year, month)
                if (year, month) not in data:
                    data[(year, month)] = []
                data[(year, month)].append(value)
    
    return data

# Example function to retrieve values for a given year and month
def get_values(data, year=None, month=None):
    if year is not None and month is not None:
        # Retrieve values for the specific year and month
        return data.get((year, month), [])
    elif year is not None:
        # Retrieve all months for a given year
        return {m: data.get((year, m), []) for m in range(1, 13)}
    elif month is not None:
        # Retrieve all values for a given month across all years
        return {y: data.get((y, month), []) for y in set(year for year, m in data.keys())}
    return {}
def fortran_unit_test():
    Num_elements = 100
    lats = list(np.linspace(-90.0, 90.0, Num_elements))
    lons = np.linspace(-180.0, 360.0, Num_elements)
    dyear = np.linspace(1970, 2000, Num_elements)
    hours = np.linspace(1,22,Num_elements)  
    height = np.linspace(1,22,Num_elements)
    dst = np.linspace(0,30,Num_elements)
    
    # f107 =[133.07838542 ,133.04515625 ,133.01192708 ,132.96208333 ,132.92885417, 132.895625,   132.426325,   132.390335,   132.354345,   132.30036   ]
    f107 = np.linspace(10, 300, Num_elements)
    lats = np.asfortranarray(lats)
    lons = np.asfortranarray(lons)
    dyear = np.asfortranarray(dyear)
    hours = np.asfortranarray(hours)
    height = np.asfortranarray(height)
    f107 = np.asfortranarray(f107)
    dst = np.asfortranarray(dst)
    time1 = time.time()
    # out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(height,lats,lons, dst, f107, MJD_time = dyear,geodflag=0)
    print("runtime", time.time() - time1)

    for i in range(0,Num_elements):
        out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(height[i],lats[i],lons[i], dst[i], f107[i], MJD_time = dyear[i],geodflag=0)
        print("inputs", height[i],lats[i],lons[i], dst[i], f107[i],  dyear[i])
        it = 0
        for val in range(0,7):
            for dim in range(0,3):
                if(np.abs(out_b[dim,val] - ans[i][it]) > .00000001):
                    print(i, val, dim, out_b[dim,val], ans[i][it])
                it+=1
            
    print("runtime", time.time() - time1)
    
def run_input_bound_test():
    Num_elements = 10
    for Num_elements in 2**np.arange(1):#2**np.arange(18):
        lats = np.ones(Num_elements)*50
        lons = np.ones(Num_elements)*50
        dyear = np.linspace(2014.202739, 2014.219178, Num_elements)
        dyear = np.linspace(2000.202739, 2009.219178, Num_elements)
        # dyear = np.ones(Num_elements)*1960.00001
        dyear = np.ones(Num_elements)*1990
        import geomaglib

        # print("datetime",geomaglib.util.decimalYearToDateTime(dyear[0]))

        # dyear = np.linspace(2009.502739, 1990.519178, Num_elements)

        hours = np.linspace(1,22,Num_elements)  
        height = np.linspace(1,1,Num_elements)
        dst = np.linspace(0,30,Num_elements)
        
        # f107 =[133.07838542 ,133.04515625 ,133.01192708 ,132.96208333 ,132.92885417, 132.895625,   132.426325,   132.390335,   132.354345,   132.30036   ]
        f1071_val = 10
        f107 = np.linspace(f1071_val, f1071_val, Num_elements)
        iono = []
        iono_temp = []
        time1 = time.time()
        out_b,out_j, core, crust,magnetosphere, ionoshere = py_mat_cm4_arr(height,lats,lons, dst, f107, crust_nmax= 65, MJD_time = dyear,geodflag=0)
        print(f"runtime for {Num_elements}", time.time()- time1)
    raise ValueError
    for i in range(0,Num_elements):
        out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(height[i],lats[i],lons[i], dst[i], f107[i], MJD_time = dyear[i],geodflag=0)
        iono_temp.append(ionoshere)
        print("inputs", height[i],lats[i],lons[i], dst[i], f107[i],  dyear[i])
        print(out_b)
    # iono.append(iono_temp)

    raise ValueError
def run_unit_tests():
    Ionosphere_Unit_test()
    Magnetosphere_Unit_test()
    Core_unit_test()

# run_unit_tests()
if __name__ == '__main__':
    # run_unit_tests()
    run_input_bound_test()

    ans = parse_bmdl_output("fortran_CM4_test_values.txt")
    UT = 1964.49738353192675

    Num_elements = 200
    
    Num_elements = 200
    lats = list(np.linspace(48.024, 48.024, Num_elements))
    lons = np.linspace(2.259, 2.259, Num_elements)
    dyear = np.linspace(2014.202739, 2014.219178, Num_elements)
    dyear = np.linspace(2009.202739, 2009.219178, Num_elements)
    dyear = np.linspace(2011.502739, 2010.519178, Num_elements)

    hours = np.linspace(1,22,Num_elements)  
    height = np.linspace(0,0,Num_elements)
    dst = np.linspace(0,30,Num_elements)
    
    # f107 =[133.07838542 ,133.04515625 ,133.01192708 ,132.96208333 ,132.92885417, 132.895625,   132.426325,   132.390335,   132.354345,   132.30036   ]
    f1071_val = 10
    f107 = np.linspace(f1071_val, f1071_val, Num_elements)

    time1 = time.time()
    # out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(height,lats,lons, dst, f107, MJD_time = dyear,geodflag=0)
    print("runtime", time.time() - time1)
    iono = []
    iono_temp = []
    for i in range(0,Num_elements):
        out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(height[i],lats[i],lons[i], dst[i], f107[i], MJD_time = dyear[i],geodflag=0)
        iono_temp.append(ionoshere)
        print("inputs", height[i],lats[i],lons[i], dst[i], f107[i],  dyear[i])

    
    # f107 =[133.07838542 ,133.04515625 ,133.01192708 ,132.96208333 ,132.92885417, 132.895625,   132.426325,   132.390335,   132.354345,   132.30036   ]
    f1072_val = 100
    f107 = np.linspace(f1072_val, f1072_val, Num_elements)
    iono_temp = []

    time1 = time.time()
    # out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(height,lats,lons, dst, f107, MJD_time = dyear,geodflag=0)
    print("runtime", time.time() - time1)
    for i in range(0,Num_elements):
        out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(height[i],lats[i],lons[i], dst[i], f107[i], MJD_time = dyear[i],geodflag=0)
        iono_temp.append(ionoshere)
        print("inputs", height[i],lats[i],lons[i], dst[i], f107[i],  dyear[i])

    iono.append(iono_temp)

    # f107 =[133.07838542 ,133.04515625 ,133.01192708 ,132.96208333 ,132.92885417, 132.895625,   132.426325,   132.390335,   132.354345,   132.30036   ]
    f107 = np.linspace(137.5, 151.4, Num_elements)
    iono_temp = []

    time1 = time.time()
    # out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(height,lats,lons, dst, f107, MJD_time = dyear,geodflag=0)
    print("runtime", time.time() - time1)
    for i in range(0,Num_elements):
        out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(height[i],lats[i],lons[i], dst[i], f107[i], MJD_time = dyear[i],geodflag=0)
        iono_temp.append(ionoshere)
        print("inputs", height[i],lats[i],lons[i], dst[i], f107[i],  dyear[i])

    iono.append(iono_temp)
    import matplotlib.pyplot as plt
    print(np.shape(iono))
    ###########
    #Plot stuff
    iono = np.array(iono)

    fig, axs = plt.subplots(3, 1, sharex=True, figsize=(10, 8))
    axes = ["Z", "X", "Y"]
    for component in range(3):
        ax = axs[component]
        ax.plot(dyear, iono[0, :, component], label=f"f107 = {f1071_val}")
        ax.plot(dyear, iono[1, :, component], label=f"f107 = {f1072_val}")
        ax.plot(dyear, iono[2, :, component], label=f"f107 = \"2014 lin-interp\"")

        ax.set_ylabel(f"component {axes[component]} [nT]")
        ax.legend(loc="upper right")
        ax.grid(True)
    axs[0].set_title(f"CM4 ionosphere output over 6 days from {int(dyear[0])}Mar16-{int(dyear[0])}Mar21")
    axs[-1].set_xlabel("dyear")
    plt.tight_layout()
    plt.show()
    
    print("runtime", time.time() - time1)
    # alt = 1
    # thet = 44.0152  
    # phi = - 62.7482
    # dst = 7
    # f107 = 723
    # py_mat_cm4(alt, thet, phi, dst, f107, MJD_time=UT, geodflag=0)
    
    
    raise ValueError
    a = parse_survey_file('CM4/lib/CM4_test_values.txt')
    print(a[0])
    for i in range (0,2000000,10000):
        print('iteration ', i)
        YYYYMMDDHHMM = create_YYYYMMDDHHMM(a,i)
        print(YYYYMMDDHHMM)
        DST = float(a[i]['D_s'])
        lat = float(a[i]['lat'])
        lon = float(a[i]['lon'])
        f107 = 12
        out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(0,lat,lon, DST, f107, ymd_time= YYYYMMDDHHMM)
        round_core = float(core[2])
        round_core = np.round(round_core*10)/10
        if(round_core != float(a[i]["B_core"])):
            print("core mismatch",core[2],round_core,a[i]["B_core"])
        if(np.round(10*magnetosphere[2])/10 != float(a[i]["B_magn"])):
            print("magnetosphere mismatch", np.round(10*magnetosphere[2])/10 , a[i]["B_magn"])
        if(ionoshere[2] != float(a[i]["B_iono"])):
            print("ionosphere mismatch", ionoshere , a[i]["B_iono"])
        # print(f"second----")
        # YYYYMMDDHHMM = create_YYYYMMDDHHMM(a,i)
        # print(YYYYMMDDHHMM)
        # DST = float(a[i]['dst_i'])
        # lat = float(a[i]['lat'])
        # lon = float(a[i]['lon'])
        # f107 = float(a[i]['kp_i'])
        # out_b,out_j, core, magnetosphere, ionoshere = py_mat_cm4(0,lat,lon, DST, f107, ymd_time=YYYYMMDDHHMM)
        # if(core[2] != a[i]["B_core"]):
        #     print("core mismatch")
        # if(magnetosphere[2] != a[i]["B_magn"]):
        #     print("magnetosphere mismatch", magnetosphere , a[i]["B_magn"])
        # if(ionoshere[2] != a[i]["B_iono"]):
        #     print("ionosphere mismatch", ionoshere , a[i]["B_iono"])
        
    print(type(a), a[0])
    print(a[0]['year'] + a[0]['month'] + a[0]['day'] + a[0]['hour'] + a[0]['min'] )
    YYYYMMDDHHMM = '197911092159'

    DST = -35
    f107 = 12
    f107 = 2041/10
    lat = 35.0211
    lon = -119.9975

    py_mat_cm4(0,lat,lon, DST, f107, ymd_time=YYYYMMDDHHMM)
    print("first test")
    f107_data = parse_file_to_dict("CM4/lib/MONTHPLT_matlab.ABS")

    DST = 33

    f107 = get_values(f107_data, year=1979, month=11)[0]/10
    print(type(f107))

    lat = 35.0211
    lon = -119.9975


    py_mat_cm4(YYYYMMDDHHMM,0,lat,lon, DST, f107)


# Example usage:
# print(get_values(f107_data, year=1979, month=11))  # Retrieve values for November 1979
# print(get_values(f107_data, year=1979))  # Retrieve all values for the year 1979
# print(get_values(f107_data, month=11)) 

# thet = 44.0152 
# alt = 0

# r_geoc ,thet_geoc= geod2geoc(np.deg2rad(thet), alt)

# thet_geoc = np.rad2deg(thet_geoc)
# r_geoc = r_geoc-6371.2
# phi = -62.7482 
# dst = 7
# f107 = 723
# print("Time, lon, lat, alt")
# print(UT, thet_geoc , phi, r_geoc)

# out_b, out_j = cm4_py310.call_cm4(UT, thet_geoc , phi, r_geoc, dst, f107)
# out_b = np.array(out_b)
# out_j = np.array(out_j)
# print('core z,x,y \n with x and z with flipped signs\n----------------------------------\n',-out_b[2,0], -out_b[0,0],out_b[1,0])
# known = np.array([-5.256421228848400e+04, -1.580177473288996e+04, -6.655557020094913e+03])
# actual = np.array([-out_b[2,0], -out_b[0,0],out_b[1,0]])
# print('error: ', known-actual)


# import numpy as np
# from scipy.optimize import least_squares

# # Function that takes lat, lon and returns B_z, B_x, B_y
# def magnetic_field(lat, lon):
#     UT = 1964.49738353192675
#     alt = -4
#     dst = 7
#     f107 = 723
#     out_b, out_j = cm4_py310.call_cm4(UT, lat, lon, alt, dst, f107)

#     return np.array([-out_b[2, 0], -out_b[0, 0], out_b[1, 0]])

# # The target values of B_z, B_x, B_y (known solution)
# target_values = np.array([-5.256e4, -1.5802e4, -6.556e3])

# # Objective function: difference between target and actual output
# def objective_function(lat_lon):
#     lat, lon = lat_lon
#     print(magnetic_field(lat, lon) - target_values)
#     return magnetic_field(lat, lon) - target_values

# # Initial guess for lat, lon
# initial_guess = [35, 10]  # Mid-point of the domain

# # Call the least-squares minimizer
# solution = least_squares(objective_function, initial_guess, bounds=([0, -180], [180, 180]))

# if solution.success:
#     lat_sol, lon_sol = solution.x
#     print(f"Solution found: lat = {lat_sol}, lon = {lon_sol}")
# else:
#     print("Solution not found.")




# import numpy as np
# from scipy.optimize import minimize

# # Function that converts geodetic coordinates to geocentric
# def geod2geoc(lat, alt):
#     # Implement your conversion logic here
#     # For example purposes, we'll return dummy values
#     r_geoc = 6371.2 + alt  # Dummy radius for geocentric
#     thet_geoc = lat        # Dummy transformation for theta
#     return r_geoc, thet_geoc

# # Function that calculates magnetic field components
# def magnetic_field(UT):
#     thet = 44.0152  # Latitude in degrees
#     alt = 0         # Altitude in km

#     r_geoc, thet_geoc = geod2geoc(np.deg2rad(thet), alt)
#     thet_geoc = np.rad2deg(thet_geoc)
#     r_geoc -= 6371.2  # Adjust for Earth radius

#     phi = -62.7482  # Longitude in degrees
#     dst = 7         # DST index
#     f107 = 723      # F10.7 solar index
#     # Call the Fortran function (cm4_py310.call_cm4)
#     out = cm4_py310.call_cm4(UT, thet_geoc, phi, r_geoc, dst, f107)
#     out_b, out_j = out[0],out[1]
    
#     # Return magnetic field components as a NumPy array
#     return np.array([-out_b[2, 0], -out_b[0, 0], out_b[1, 0]])

# # Target values of B_z, B_x, B_y (known solution)
# target_values = np.array([-5.256e4, -1.5802e4, -6.556e3])

# # Objective function: calculate the difference between the output and target values
# # def objective_function(UT):
# #     # Calculate the magnetic field components
# #     return magnetic_field(UT) - target_values

# def objective_function(UT):
#     return magnetic_field(UT)[0] - target_values[0]
#     print(magnetic_field(UT) - target_values) 
#     return np.sum((magnetic_field(UT) - target_values) ** 2)

# # Initial guess for UT
# initial_guess = 1964.49738353192675  # You can adjust this as needed
# bounds = [(1964, 2010)]
# # Call the root finder
# solution = minimize(objective_function, initial_guess,bounds = bounds)

# # Check the result
# if solution.success:
#     ut_sol = solution.x[0]  # Extract the scalar solution
#     print(f"Solution found: UT = {ut_sol}")
# else:
#     print("Solution not found.")
def old_fortran_to_forget():
    """
    IF(pred1.ge.0.5) THEN
            pred1 = .true.
        ELSE
            pred1 = .false.
        END IF

        IF(pred2.ge.0.5) THEN
            pred2 = .true.
        ELSE
            pred2 = .false.
        END IF
        IF(pred3.ge.0.5) THEN
            pred3 = .true.
        ELSE
            pred3 = .false.
        END IF
        IF(pred4.ge.0.5) THEN
            pred4 = .true.
        ELSE
            pred4 = .false.
        END IF
        IF(pred5.ge.0.5) THEN
            pred5 = .true.
        ELSE
            pred5 = .false.
        END IF
        IF(pred6.ge.0.5) THEN
            pred6 = .true.
        ELSE
            pred6 = .false.
        END IF
        IF(CORD.ge.0.5) THEN
            CORD = .true.
        ELSE
            CORD = .false.
        END IF

    """
    do = 'nothing'
    return do