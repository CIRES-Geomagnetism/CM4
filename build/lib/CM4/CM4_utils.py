import numpy as np
from datetime import datetime, timedelta
import geomaglib
import cm4_py310_arr
import calendar
import datetime as dt

def parse_survey_file(filename):
    with open(filename, 'r') as file:
        # Read header
        headers = file.readline().strip().split()

        # Initialize dict of lists
        data = {key: [] for key in headers}

        # Read each line and append values to corresponding lists
        for line in file:
            values = line.strip().split()
            for key, value in zip(headers, values):
                data[key].append(value)

    return data



def calc_dec_year(year: int, month: int, day:int, hour:int = 0, minutes:int=0, seconds:int=0 ) -> float:
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
    hour_frac = (1/24) * (1/num_days_year) * hour
    min_frac = (1/24) * (1/60) * (1/num_days_year) * minutes
    sec_frac = (1/24) * (1/60) * (1/60) * (1/num_days_year) * seconds
    return year + day_frac + hour_frac + min_frac + sec_frac

def calc_dec_year_array(year: np.ndarray[int], month: np.ndarray[int], day:np.ndarray[int], hour:np.ndarray[int]=None, minute:np.ndarray[int]=None, second:np.ndarray[int]=None ) -> np.ndarray:
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


    if not second:
        second = [0]*N
        
    for i in range(0,year.size):

        decYear = calc_dec_year(year[i], month[i], day[i], hour[i], minute[i], second[i])
        dec_year.append(decYear)

    return np.array(dec_year)

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
    
    # Determine if the year is a leap year
    ifleapyr = (iy % 4 == 0)  # 1 if leap year, 0 otherwise
    
    # Check if the month is greater than February and it is a leap year
    i2 = (im > 2) and ifleapyr
    
    # Compute UT in years
    ut = iy + (dof0[im - 1] + id - 1 + i2 + (ih + minute / 60 + sec / 3600) / 24) / (365 + ifleapyr)
    return (ut - 2000.002739726)/1.00000029 +0.000577188811121232


def py_mat_cm4_arr(alt, lat_geod, lon, dst, f107,core_nmin = 1, core_nmax = 13, crust_nmin = 14, crust_nmax = 45, geodflag = 1,year = None, month = None, day = None, hour = None, minute = None, MJD_time = None):
    if MJD_time is None and year is None:raise ValueError("a time input must be provided")
    #Change yyyymmddhhmmss time to Year decimal time

    if year is not None:
        # year, month, day, hour, minute = parse_time(ymd_time)
        # hour = hour - 1

        # tmp = jd2000(year,month,day, hour + minute/60)
        # UT = mjd2000_to_ut(tmp)
        UT = calc_dec_year_array(year, month, day, hour = hour, minute = minute)
        # print(UT, year, month, day, hour, minute)
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
if __name__ == '__main__':


    test_value_dict = parse_survey_file('CM4_test_values.txt')
    num_elements = 100
    print(test_value_dict["day"][0:1], test_value_dict.keys())
    out_b,out_j, core, crust, magnetosphere, ionoshere = py_mat_cm4_arr(np.zeros(num_elements),
    np.float32(test_value_dict['lat'][0:num_elements]),
    np.float32(test_value_dict['lon'][0:num_elements]),
    np.float32(test_value_dict['dst_i'][0:num_elements]),
    10*np.ones(num_elements),
    year = np.int32(test_value_dict['year'][0:num_elements]),
    month = np.int32(test_value_dict['month'][0:num_elements]),
    day = np.int32(test_value_dict['day'][0:num_elements]),
    hour = np.int32(test_value_dict['hour'][0:num_elements]),
    minute = np.int32(test_value_dict['min'][0:num_elements]))
    print(np.shape(core[2]))
    round_core = np.round(core[2]*10)/10
    for i in range(0, num_elements):
        # print(test_value_dict["B_core"][i])
        if(round_core[i] != float(test_value_dict["B_core"][i])):
            print("core mismatch",core[2],round_core[i],test_value_dict["B_core"][i])
        if(np.round(10*magnetosphere[2][i])/10 != float(test_value_dict["B_magn"][i])):
            print("magnetosphere mismatch", np.round(10*magnetosphere[2][i])/10 , test_value_dict["B_magn"][i])
        # if(ionoshere[2] != float(test_value_dict["B_iono"][i])):
        #     print("ionosphere mismatch", ionoshere , test_value_dict["B_iono"][i])