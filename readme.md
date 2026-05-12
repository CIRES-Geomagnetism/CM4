# CM4 Python API

This package wraps CM4 Fortran code in Python 3.11 to 3.14.

## Installation

Please select the wheel based on your Python version and platform from the latest [GitHub Releases](https://github.com/CIRES-Geomagnetism/CM4/releases).  
It supports macOS (x86_64 and arm64) and Linux x86_64 and Windows x86_64.

### Linux

Select the wheel with the `manylinux` tag. The `cp3**` tag indicates the Python version.  
Compatible with distributions using glibc ≥ 2.27, such as Ubuntu 18.04+, Debian 10+, and RHEL 8+.

```bash
pip install cm4-1.0.6-cp313-cp313-manylinux_2_27_x86_64.manylinux_2_28_x86_64.whl
```

### macOS

- **x86_64 (Intel):** wheel has `macosx_*_x86_64` in the name.
- **arm64 (Apple Silicon):** wheel has `macosx_*_arm64` in the name.

The `cp3**` tag indicates the compatible Python version. Compatible with macOS 10.15 and later.

```bash
pip install cm4-1.0.6-cp313-cp313-macosx_14_0_arm64.whl
```

### Windows
Select the wheel with `win_amd64` in the name.

## Quick Start

```python
from cm4.callfpy import py_mat_cm4_arr
import numpy as np

if __name__ == '__main__':
    # Decimal year inputs
    dyear  = [2008.683172, 2002.141925, 2008.37822, 2002.366282, 2003.482011]
    lats   = [12.197895, -64.920192, 39.123596, -16.128974, -53.190919]   # geographic latitude (degrees)
    lons   = [276.97, -46.77, 22.66, -58.79, -116.55]                     # longitude (degrees)
    height = [2.5, 20.2, 53.7, 77.8, 26.9]                                # altitude (km)
    dst    = [-7.95, -9.0, 4.0, -52.3, -17.4]                             # Dst index (nT)
    f107   = [67.1, 202.6, 67.8, 173.6, 125.8]                            # F10.7 solar flux index

    out_b, core, crust, magnetosphere, ionosphere = py_mat_cm4_arr(
        height, lats, lons, dst, f107,
        MJD_time=dyear,   # decimal year
        geodflag=0        # 0 = geocentric (r, theta, phi); 1 = geodetic (up, south, east)
    )

    print("Core field [Br, Bθ, Bφ] (nT):")
    print(core)
```

### Return values

| Variable | Description |
|---|---|
| `out_b` | Raw `BMDL(3, 7, N)` array — all field components from all sources (nT) |
| `core` | Core field `[Br, Bθ, Bφ]` (columns 1–2 of `out_b`) |
| `crust` | Crustal field `[Br, Bθ, Bφ]` |
| `magnetosphere` | Magnetospheric field `[Br, Bθ, Bφ]` (primary + induced) |
| `ionosphere` | Ionospheric field `[Br, Bθ, Bφ]` (primary + induced) |

`out_b` column labels:

| Column | Source |
|---|---|
| 1 | Main field 1 |
| 2 | Main field 2 |
| 3 | Primary magnetospheric field |
| 4 | Induced magnetospheric field |
| 5 | Primary ionospheric field |
| 6 | Induced ionospheric field |
| 7 | Toroidal field |

Row labels: `(1) X`, `(2) Y`, `(3) Z`.

## Source Files

| File | Description |
|---|---|
| `cm4field_.F` | Original `CM4_FIELD` Fortran subroutine |
| `call_cm4field_array.f90` | Fortran 90 wrapper enabling array-valued inputs |
| `ccm4.c` / `c_wrapper.c` | C wrapper exposing the Fortran code to Python via `ctypes` |
| `callfpy.py` | Python wrapper providing the `py_mat_cm4_arr` user API |
