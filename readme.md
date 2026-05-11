# CM4 Python API

This package wraps CM4 fortran code in Python 3.11 to 3.14

## To install CM4 Python API 

Please select the wheel based on your python version and platform from the latest GitHub Releases. 
It supports macOS and x86_64 architecture for linux and Windows OS.

### For Linux users

Select the wheels which have the `manylinux` tag in the name. 
The `cp3**` tag indicates the compatible Python version.
It is compatible with older version of Linux distributions such as `Ubuntu 18.04`, `Debian 10` and `REHL 8`.

Then install it using pip:
```commandline
pip install cm4-<latest_version>-cp313-cp313-manylinux_2_27_x86_64.manylinux_2_28_x86_64.whl
```

### For MacOS users
It is compatible with macOS 10.15 and later versions.

- For x86_64 architecture, select the wheels which have the `macosx_15_0_x86_64` tag in the name. 
- For Apple Silicon (arm64) architecture, select the wheels which have the `macosx_15_0_arm64` tag in the name.


The `cp3**` tag indicates the compatible Python version.
### For Windows users

Select the wheels which have the `win_amd64` tag in the name. It is compatible with `Windows 10` and later versions.
The `cp3**` tag indicates the compatible Python version.
## Quick Start

```python
from callfpy import py_mat_cm4_arr
import numpy as np
if __name__ == '__main__':

    Num_elements = 2
    co_lats = np.ones(Num_elements)*50
    lons = np.ones(Num_elements)*50
  
    dyear = np.ones(Num_elements)*1990
    hours = np.linspace(1,22,Num_elements)  
    height = np.linspace(1,1,Num_elements)
    dst = np.linspace(0,30,Num_elements)

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
    
```

## Source codes
- The `cm4field_.F` contains the original function CM4_FIELD. 
- The `call_cm4field_array.f90` file is a Fortran 90 wrapper that allows the original CM4_FIELD function to be called with an array of inputs.
- The `ccm4.c` and `c_wrapper.c` is the C wrapper that allows the Fortran code to be called from Python using the `ctypes` library.
- The `callfpy.py` file contains the Python wrapper that provides a user-friendly interface to call the CM4 function from Python.