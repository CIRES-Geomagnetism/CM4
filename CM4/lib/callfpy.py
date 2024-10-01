from datetime import datetime, timedelta
import numpy as np

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
    
    # Determine if the year is a leap year
    ifleapyr = (iy % 4 == 0)  # 1 if leap year, 0 otherwise
    
    # Check if the month is greater than February and it is a leap year
    i2 = (im > 2) and ifleapyr
    
    # Compute UT in years
    ut = iy + (dof0[im - 1] + id - 1 + i2 + (ih + minute / 60 + sec / 3600) / 24) / (365 + ifleapyr)
    
    return ut - 2000

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
import numpy as np

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
def read_answers(filepath = 'testvalBcore.csv'):
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
import importlib
import csv
import cm4_py310

# Force reimport of the module
importlib.reload(cm4_py310)
# import cm4_py310


# UT = 1964.49738353192675
def py_mat_cm4_unittest(ymd_time, alt, lat_geod, lon, dst, f107):
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
    cord = True
    out_b, out_j = cm4_py310.call_cm4(UT, thet_geoc , lon, r_geoc, dst, f107,
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
    
time = '196407011059'
alt = 0
lat = 43
lon = 34
dst = 7
f107 = 465
nmin = np.array([1,14])
nmax =np.array([13,45])
pred = np.array([True,True,True,True,True,True])
cord = True
print('hello!!!')
call_py_cm4(time,alt,lat,lon,dst,f107,nmin,nmax,cord,pred)
print('finished!!!')
def read_csv(filepath = 'Core_unittest_inputs.csv'):
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
        out_b,out_j = py_mat_cm4_unittest(yyyymmddhhmm,0,lat,lon,7,723)
        core = np.array([-out_b[2,0], -out_b[0,0],out_b[1,0]])
        # print(answers[i], core)
        print('Do matlab and py cm4_core agree to 5 digits',np.isclose(answers[i],core,rtol = 1e-4))
read_csv()


read_answers()
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