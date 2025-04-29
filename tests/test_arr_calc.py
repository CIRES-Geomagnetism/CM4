from datetime import datetime, timedelta
import os
import geomaglib.util
import numpy as np
import importlib
import csv
from CM4.callfpy import py_mat_cm4, py_mat_cm4_arr
from CM4 import cm4field, cm4field_arr
# import geomaglib
# Force reimport of the module
# importlib.reload(cm4_py310)
# import cm4_py310
import time

import re

curr_dir = os.path.dirname(__file__)
top_dir = os.path.dirname(curr_dir)
COF_PATH = os.path.join(top_dir, "CM4", "umdl.CM4")
TESTVAL_DIR = os.path.join(curr_dir, "test_values")


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
    print("in here", iy, im, id, ih)
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

    sin_alpha_2 = np.sin(alpha) ** 2
    cos_alpha_2 = np.cos(alpha) ** 2

    tmp = h * np.sqrt(a ** 2 * cos_alpha_2 + b ** 2 * sin_alpha_2)
    beta = np.arctan((tmp + b ** 2) / (tmp + a ** 2) * np.tan(alpha))
    theta = np.pi / 2 - beta

    r = np.sqrt(
        h ** 2 + 2 * tmp + (a ** 2 * (1 - (1 - (b / a) ** 4) * sin_alpha_2)) / (1 - (1 - (b / a) ** 2) * sin_alpha_2))

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


# Core read answers
def read_answers(filepath='test_values/testvalBcore.csv'):
    data = []
    with open(filepath, mode='r') as file:
        reader = csv.reader(file)
        for row_index, row in enumerate(reader):
            data.append(row)
            data[-1][1] = data[-1][1][1:]
            data[-1][2] = data[-1][2][1:]

            # print(data[-1])
            for i in range(0, len(data[-1])):
                data[-1][i] = float(data[-1][i])
    return data


def read_answers_ext(filepath='CM4/lib/testvalBcore.csv'):
    data = []
    with open(filepath, mode='r') as file:
        reader = csv.reader(file)
        for row_index, row in enumerate(reader):
            data.append(row)
            # data[-1][1] = data[-1][1]
            # data[-1][2] = data[-1][2]

            # print(data[-1])
            for i in range(0, len(data[-1])):
                data[-1][i] = float(data[-1][i])
    return data



def calc_dec_year(year: int, month: int, day: int, hour: int = 0, minutes: int = 0, seconds: int = 0) -> float:
    """
    Takes year, month, and day and calculates the decimal year from those inputs

    Parameters:
    year (int): The year fully written for example 2024
    month (int): The month from 1-12 where is 1 is January and 12 is December
    day (int): The day of the month from 1-31
    hour(int): The hour of the day from 0-23
    minutes (int): The current minutes of the hour 0-59
    seconds (int): The current seocnds 0-59

    Returns:
    (float): The decimal year

    """
    num_days_year = 365
    if calendar.isleap(year):
        num_days_year = 366
    date = dt.datetime(year, month, day)
    day_of_year = date.timetuple().tm_yday
    day_frac = ((day_of_year - 1) / num_days_year)
    hour_frac = (1 / 24) * (1 / num_days_year) * hour
    min_frac = (1 / 24) * (1 / 60) * (1 / num_days_year) * minutes
    sec_frac = (1 / 24) * (1 / 60) * (1 / 60) * (1 / num_days_year) * seconds
    return year + day_frac + hour_frac + min_frac + sec_frac


def calc_dec_year_array(year: np.ndarray[int], month: np.ndarray[int], day: np.ndarray[int],
                        hour: np.ndarray[int] = None, minute: np.ndarray[int] = None,
                        second: np.ndarray[int] = None) -> np.ndarray:
    """
    Takes the array of year, month, and day and outputs the decimal year from those inputs

    Parameters:
    year (int): The year fully written for example 2024
    month (int): The month from 1-12 where is 1 is January and 12 is December
    day (int): The day of the month from 1-31

    Returns:
    (float): The decimal year
    adjusted calc_dec_year to accept vectors by Collin 01/25
    """
    dec_year = []
    # if(year.size == 1):
    #     num_days_year = 365
    #     if calendar.isleap(year):
    #         num_days_year = 366

    #     date = dt.datetime(year, month, day)
    #     day_of_year = date.timetuple().tm_yday
    #     dec_year.append(year + ((day_of_year - 1) / num_days_year))
    # else:

    N = year.size
    if hour is None:
        hour = [0] * N

    if minute is None:
        minute = [0] * N

    if second is None:
        second = [0] * N

    for i in range(0, year.size):
        decYear = calc_dec_year(year[i], month[i], day[i], hour[i], minute[i], second[i])
        dec_year.append(decYear)

    return np.array(dec_year)




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

def run_input_bound_test():
    Num_elements = 10
    for Num_elements in 2 ** np.arange(1):  # 2**np.arange(18):
        lats = np.ones(Num_elements) * 50
        lons = np.ones(Num_elements) * 50
        dyear = np.linspace(2014.202739, 2014.219178, Num_elements)
        dyear = np.linspace(2000.202739, 2009.219178, Num_elements)
        # dyear = np.ones(Num_elements)*1960.00001
        dyear = np.ones(Num_elements) * 1990
        import geomaglib

        # print("datetime",geomaglib.util.decimalYearToDateTime(dyear[0]))

        # dyear = np.linspace(2009.502739, 1990.519178, Num_elements)

        hours = np.linspace(1, 22, Num_elements)
        height = np.linspace(1, 1, Num_elements)
        dst = np.linspace(0, 30, Num_elements)

        # f107 =[133.07838542 ,133.04515625 ,133.01192708 ,132.96208333 ,132.92885417, 132.895625,   132.426325,   132.390335,   132.354345,   132.30036   ]
        f1071_val = 10
        f107 = np.linspace(f1071_val, f1071_val, Num_elements)
        iono = []
        iono_temp = []
        time1 = time.time()
        out_b, out_j, core, crust, magnetosphere, ionoshere = py_mat_cm4_arr(height, lats, lons, dst, f107,
                                                                             crust_nmax=65, MJD_time=dyear, geodflag=0)
        print(f"runtime for {Num_elements}", time.time() - time1)
    raise ValueError
    for i in range(0, Num_elements):
        out_b, out_j, core, magnetosphere, ionoshere = py_mat_cm4(height[i], lats[i], lons[i], dst[i], f107[i],
                                                                  MJD_time=dyear[i], geodflag=0)
        iono_temp.append(ionoshere)
        print("inputs", height[i], lats[i], lons[i], dst[i], f107[i], dyear[i])
        print(out_b)
    # iono.append(iono_temp)

    raise ValueError


def read_testval_inputs():
    filepath = os.path.join(TESTVAL_DIR, "Core_unittest_inputs.csv")
    data = []

    with open(filepath, mode='r') as file:

        reader = csv.reader(file)
        for row_index, row in enumerate(reader):
            if 1 <= row_index <= 14:  # Rows 1 to 14 (inclusive)
                data.append(row[1:4])  # Columns 1 to 3 (inclusive)
                for val in range(0, 2):
                    data[-1][val] = float(data[-1][val])
                data[-1][2] = float(data[-1][2])
                data[-1][2] = str(data[-1][2])
    year, month, day, hour, minute = [], [], [], [], []
    for i in range(0, len(data)):
        y, m, d, h, mi = parse_time(data[i][2])
        year.append(y)
        if d == 0:
            d = 30
            m -= 1
        month.append(m)
        day.append(d)
        hour.append(h)
        minute.append(mi)
    return np.array(data)[:, 0], np.array(data)[:, 1], year, month, day, hour, minute


def arr_calc_unittests():
    lat, lon, year, month, day, hour, minute = read_testval_inputs()
    lat = np.array(lat, dtype='f')
    lon = np.array(lon, dtype='f')
    core_testval = os.path.join(TESTVAL_DIR, "testvalBcore.csv")
    answers = read_answers(core_testval)
    out_b, out_j, core, crust, magnetosphere, ionoshere = py_mat_cm4_arr(np.zeros(len(lat)), np.array(lat, dtype='f'),
                                                                         np.array(lon, dtype='f'), np.ones(len(lat)),
                                                                         np.ones(len(lat)), pred=np.array(
            [True, False, False, False, False, False]), year=year, month=month, day=day, hour=hour, minute=minute)
    core = np.transpose(core)
    all_pass = True
    for i in range(0, len(lat)):
        if (not np.all(np.isclose(answers[i], core[i], rtol=1e-4))):
            raise AssertionError('Do matlab and py cm4_core dont agree to 5 digits', np.isclose(answers[i], core, rtol=1e-4),
                  answers[i], core[i])
            all_pass = False

    if all_pass: print('core unittest passed')
    # END CORE UNITTEST
    # BEGIN IONO UNITTEST
    dst = np.array([-4, -4, -4, -4, -4, -4, -84, -84, -84, -84, -84, -84, -84])
    f107 = np.array([63.2, 63.2, 63.2, 63.2, 63.2, 63.2, 171.3, 171.3, 171.3, 171.3, 171.3, 171.3, 171.3])
    iono_testval = os.path.join(TESTVAL_DIR, "testvalB_iono.csv")
    answers = read_answers_ext(iono_testval)
    out_b, out_j, core, crust, magnetosphere, ionoshere = py_mat_cm4_arr(np.zeros(len(lat)), np.array(lat, dtype='f'),
                                                                         np.array(lon, dtype='f'), dst, f107,
                                                                         pred=np.array(
                                                                             [True, True, True, True, True, True]),
                                                                         year=year, month=month, day=day, hour=hour,
                                                                         minute=minute)
    ionoshere = np.transpose(ionoshere)
    all_pass = True
    for i in range(0, len(lat)):
        if (not np.all(np.isclose(answers[i], ionoshere[i], rtol=1e-4))):
            raise AssertionError('Do matlab and py cm4_ionosphere dont agree to 5 digits',
                  np.isclose(answers[i], ionoshere[i], rtol=1e-4), answers[i], ionoshere[i])
            all_pass = False
    if all_pass: print('ionosphere unittest passed')
    # END IONO UNITTEST
    # BEGIN MAGNETO UNITTEST
    dst = np.array([-4, -4, -4, -4, -4, -4, -84, -84, -84, -84, -84, -84, -84])
    f107 = np.array([63.2, 63.2, 63.2, 63.2, 63.2, 63.2, 171.3, 171.3, 171.3, 171.3, 171.3, 171.3, 171.3])
    magneto_testval = os.path.join(TESTVAL_DIR, "testvalB_magn.csv")
    answers = read_answers_ext(magneto_testval)
    out_b, out_j, core, crust, magnetosphere, ionoshere = py_mat_cm4_arr(np.zeros(len(lat)), np.array(lat, dtype='f'),
                                                                         np.array(lon, dtype='f'), dst, f107,
                                                                         pred=np.array(
                                                                             [True, True, True, True, False, False]),
                                                                         year=year, month=month, day=day, hour=hour,
                                                                         minute=minute)
    magnetosphere = np.transpose(magnetosphere)
    all_pass = True
    for i in range(0, len(lat)):
        if (not np.all(np.isclose(answers[i], magnetosphere[i], rtol=1e-4))):
            raise AssertionError('Do matlab and py cm4_core dont agree to 5 digits',
                  np.isclose(answers[i], magnetosphere[i], rtol=1e-4), answers[i], magnetosphere[i])
            all_pass = False
    if all_pass: print('magnetosphere unittest passed')


# run_unit_tests()
if __name__ == '__main__':

    arr_calc_unittests()
