import sys
import os
# Remove the local dir from sys.path to force import from site-packages

from CM4.callfpy import py_mat_cm4_arr





#from python_CM4.callfpy import py_mat_cm4_arr
import numpy as np
if __name__ == '__main__':

    Num_elements = 2
    co_lats = np.ones(Num_elements)*50
    lons = np.ones(Num_elements)*50
    #dyear = np.linspace(2014.202739, 2014.219178, Num_elements)
    #dyear = np.linspace(2000.202739, 2009.219178, Num_elements)

    # dyear = np.ones(Num_elements)*1960.00001
    dyear = np.ones(Num_elements)*1990

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
    
    out_b,out_j, core,crust, magnetosphere, ionoshere = py_mat_cm4_arr(height,co_lats,lons, dst, f107, crust_nmax= 65, MJD_time = dyear,geodflag=0)
    print(out_b)
    