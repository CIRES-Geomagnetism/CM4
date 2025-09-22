import os
import geomaglib.util
import numpy as np

from cm4 import cm4field_arr

curr_dir = os.path.dirname(__file__)
COF_PATH = os.path.join(curr_dir, "umdl.CM4")            
   
def py_mat_cm4_arr(alt: list[float], lat: list[float], lon: list[float], dst: list[float], f107: list[float],pred: list[bool] = None, 
                   core_nmin: int = 1, core_nmax: int = 13, crust_nmin: int = 14, crust_nmax: int = 45, geodflag: int = 1,
                   year: int = None, month: int = None, day: int = None, hour: int = None, minute: int = None, MJD_time: list[float] = None) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """
    if geodflag is falsy (0, False, etc) inputs are interpreted as geocentric latitude and radial altitude (radius-earth radius), 
    and outputs return in geocentric spherical (e.g. core[0] is B_r, core[1] is B_theta, core[2] is B_phi)
    
    if geodflag is truthy (1, True, etc) inputs are interpreted as geodetic lat and altitude above ellipsoid
    and outputs return in geodetic up, south, east (e.g. core[0] is B_up, core[1] is B_south, core[2] is B_east)
    (note that this is the geodetic equivalent of r,theta and phi directions). This implicitly uses 
    CM4's internal geodetic <-> geocentric subroutines which use an outdated ellipsoid
    """
    if MJD_time is None and year is None:raise ValueError("a time input must be provided")
    #Change yyyymmddhhmmss time to Year decimal time

    N = len(alt)

    if (len(lat) != N or len(lon) != N or len(dst) != N or len(f107) != N):
        raise ValueError("alt, lat, lon, dst, and f107 must all be the same length")

    if pred is None:
        pred = [True, True, True, True, True, True]

    if year is not None:

        UT = geomaglib.util.calc_dec_year_array(np.array(year), np.array(month), np.array(day), np.array(hour), np.array(minute))

    else:
        UT = MJD_time

    colat = [90.-l for l in lat]  # Convert latitude to colatitude

    cord = False
    if(geodflag):
        cord = True


    nmin = [core_nmin,crust_nmin]
    nmax = [core_nmax,crust_nmax]

    out_b = cm4field_arr.call_cm4(UT, colat , lon, alt, dst, f107,
                                      pred[0],pred[1],pred[2],pred[3],pred[4],pred[5]
                                      ,cord,
                                      nmax[0],nmax[1], nmin[0],nmin[1], N, COF_PATH)



    ionoshere = np.array([-out_b[2,4]-out_b[2,5], -out_b[0,4]-out_b[0,5],out_b[1,4]+out_b[1,5]])
    magnetosphere = np.array([-out_b[2,2]-out_b[2,3], -out_b[0,2]-out_b[0,3],out_b[1,2]+out_b[1,3]])
    core = np.array([-out_b[2,0], -out_b[0,0],out_b[1,0]])
    crust = np.array([-out_b[2,1], -out_b[0,1],out_b[1,1]])
    # print('core',core, "f", np.sqrt(core[0]**2 + core[1]**2 + core[2]**2))
    # print('magnetosphere',magnetosphere)
    # print('ionoshere', ionoshere)
    # print('raw', out_b, np.shape(out_b))
    # print('core z,x,y \n with x and z with flipped signs\n----------------------------------\n',-out_b[2,0], -out_b[0,0],out_b[1,0])
    return out_b, core,crust, magnetosphere, ionoshere
