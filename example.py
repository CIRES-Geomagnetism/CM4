import sys
import os
from cm4.callfpy import py_mat_cm4_arr

import numpy as np
if __name__ == '__main__':

    Num_elements = 20
    #co_lats = np.ones(Num_elements)*50
    #lons = np.ones(Num_elements)*50
    #dyear = np.linspace(2014.202739, 2014.219178, Num_elements)
    #dyear = np.linspace(2000.202739, 2009.219178, Num_elements)

    # dyear = np.ones(Num_elements)*1960.00001
    #dyear = np.ones(Num_elements)*1990

    # print("datetime",geomaglib.util.decimalYearToDateTime(dyear[0]))

    # dyear = np.linspace(2009.502739, 1990.519178, Num_elements)

    #hours = np.linspace(1,22,Num_elements)
    #height = np.linspace(1,1,Num_elements)
    #dst = np.linspace(0,30,Num_elements)

    # f107 =[133.07838542 ,133.04515625 ,133.01192708 ,132.96208333 ,132.92885417, 132.895625,   132.426325,   132.390335,   132.354345,   132.30036   ]
    #f1071_val = 10
    #f107 = np.linspace(f1071_val, f1071_val, Num_elements)
    #iono = []
    #iono_temp = []
    print("""out_b is the raw output containing the outputs:*     BMDL(3,7,Num_elements)
           Dble   O     Array storing computed B field vectors from various
            sources (nT):
                                                                
            Row label:
            (1) X.
            (2) Y.
            (3) Z.
                                                                
            Column label:
            (1) Main field 1.
            (2) Main field 2.
            (3) Primary magnetospheric field.
            (4) Induced magnetospheric field.
            (5) Primary ionospheric field.
            (6) Induced ionospheric field.
            (7) Toroidal field.""")
    print("""core, crust, magnetosphere, ionoshere contain
          the described fields with sign convention
          shifted for r, theta, phi (in that order) from X,Y,Z which 
          is the order the values are contained in out_b
          out_j contains currents and is probably won't be used
          """)

    dyear = [2008.683172, 2002.141925, 2008.37822, 2002.366282, 2003.482011, 2007.231835, 2005.512113, 2008.083823,
           2001.404037, 2005.302225]
    co_lats = [12.197895, -64.920192, 39.123596, -16.128974, -53.190919, -37.852486, -22.62258, 54.89387, -81.873783,
               -53.111882]
    lons =  [276.973566, -46.773446, 22.659406, -58.793632, -116.546062, 103.615024, 275.867462, 228.114543,
                -166.461759, 244.579773]
    height = [2.544375, 20.215407, 53.71395, 77.795141, 26.904681, 63.624273, 40.215226, 67.78177, 11.903506,
               23.380162]
    dst = [-7.95, -9.0, 4.0, -52.316667, -17.4, -16.733333, 3.6, 14.0, -3.0, -8.033333]
    f107 = [67.109877, 202.55679, 67.792593, 173.58642, 125.818519, 73.533333, 94.032099, 72.34, 161.197531, 92.544444]


    out_b, core,crust, magnetosphere, ionoshere = py_mat_cm4_arr(height, co_lats, lons, dst, f107, crust_nmax= 45, MJD_time = dyear,geodflag=0)
    #out_b, out_j, core, magnetosphere, ionoshere = py_mat_cm4(height[0], co_lats[0], lons[0], dst[0], f107[0],
     #                                                                     MJD_time = dyear[0])



    print(core)
    