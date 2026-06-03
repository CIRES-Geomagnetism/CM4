import math
import os
import copy
import csv
import numpy as np
from collections import defaultdict
import geomaglib
from cm4.callfpy import py_mat_cm4_arr


def geodetic_position_to_geocentric(gdlat: list[float],
                                    h_ellip_km: list[float]):
    """Convert a position described by
    geodetic latitude and ellipsoidal height (units of kilometers) to
    geocentric latitude and radial altitude (also kilometers)
    (distance of location from the center of the earth - earth radius)
    """
    earth_radius_km = 6371.2
    radius_km, gclat = geomaglib.util.geod_to_geoc_lat(np.asarray(gdlat), np.asarray(h_ellip_km))
    gclat = gclat.flatten().tolist()
    radial_alt_km = (radius_km - earth_radius_km).flatten().tolist()
    return gclat, radial_alt_km


def spherical_vector_to_geodetic(B_r: list[float],
                                 B_theta: list[float],
                                 B_phi: list[float],
                                 geocentric_lat: list[float],
                                 geodetic_lat: list[float]):
    """Convert a magnetic field vector represented by components
    along the radial, colatitudinal (theta), and eastward/azimuthal (phi) directions
    to the equivalent vector represented in the geodetic northward,
    eastward and nadir (vertical down) directions"""
    # spherical to south,east,up
    B_south, B_east, B_up = geomaglib.magmath.rotate_magvec(np.asarray(B_theta),
                                                  np.asarray(B_phi),
                                                  np.asarray(B_r),
                                                  np.asarray(geocentric_lat),
                                                  np.asarray(geodetic_lat))

    # XYZ frame is North, East, Down, flip signs
    Bx = (-1. * B_south).flatten().tolist()  # North
    By = B_east.flatten().tolist()
    Bz = (-1. * B_up).flatten().tolist()  # Down
    return Bx, By, Bz


def read_inputs(filename: str):
    inp_keys = ["date", "latitude", "longitude", "altitude", "dst", "f107"]
    out_keys = ["Bx", "By", "Bz"]

    inputs = defaultdict(list)
    outputs = defaultdict(list)

    with open(filename, 'r') as file:
        file.readline()
        for line in file:
            vals = line.strip().split(',')

            for key in inp_keys:
                inputs[key].append(float(vals[inp_keys.index(key)]))

            for key in out_keys:
                outputs[key].append(float(vals[out_keys.index(key) + len(inp_keys)]))

    return inputs, outputs


def measure_diff(true_vals, pred_vals, out_file, tol=1e-2):
    keys = ["Bx", "By", "Bz"]
    N = len(pred_vals["Bx"])
    diffs = [0] * N

    with open(out_file, "w") as f:
        f.write("key,max_diff,ave_diff,rmse\n")
        for key in keys:
            max_diff_ind = -1
            max_diff = 0.0
            ave_diff = 0.0
            for i in range(N):
                diff = abs(true_vals[key][i] - pred_vals[key][i])
                if diff > max_diff:
                    max_diff = diff
                    max_diff_ind = i
                ave_diff += diff
                diffs[i] = diff
                #if diff > tol:
                    #raise ValueError(f"Difference between {key} and {max_diff_ind} is {diff}")
                # f.write(f"{key},{true_vals[key][i]},{pred_vals[key][i]}\n")
                #    raise ValueError(f"In {out_file}, Difference for {key} at index {i} exceeds tolerance: {diff} > {tol}. True: {true_vals[key][i]} Pred: {pred_vals[key][i]}")
                # else:
                #    f.write(f"{key},{true_vals[key][i]},{pred_vals[key][i]}\n")

            ave_diff = ave_diff / N

            rmse = math.sqrt(sum((diff - ave_diff) ** 2 for diff in diffs) / N)

            keydiffstr = f"{key},{max_diff},{max_diff_ind},{ave_diff},{rmse}\n"
            f.write(keydiffstr)
            print(keydiffstr)

            #if rmse > 1:
            #    raise ValueError(f"RMSE at {key} is {rmse}, which exceeds tolerance of 1")


def write_python_output(outputs: dict, out_filename: str):
    """Write out a CSV file from outputs dictionary
    Keys should be column names and values should lists of floats
    (value of that column for all rows)"""
    with open(out_filename, 'w') as csvfile:
        output_cols = [column_name for column_name in outputs.keys()]
        fieldnames = output_cols
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        nrows = len(outputs[output_cols[0]])
        for i in range(nrows):
            row = {}
            for key in output_cols:
                row[key] = outputs[key][i]
            writer.writerow(row)


def generate_python_output(inputs: dict, field: str):
    """Mirror the C pipeline in create_cm4_arr_results.c exactly:
      1. geod_to_geocentric  (same WGS-84 constants as the C code)
      2. call CM4 with geocentric colatitude + radial altitude, geodflag=0
      3. spherical_to_geodetic_vec  (same rotation as C spherical_vector_to_geodetic)
    """
    outputs = copy.deepcopy(inputs)

    preds = [True, True, True, True, True, True]

    geoc_lats, radial_alt = geodetic_position_to_geocentric(inputs["latitude"], inputs["altitude"])

    out_b, core, crust, magnetosphere, ionosphere = py_mat_cm4_arr(radial_alt,
                                                                   geoc_lats,
                                                                   inputs["longitude"],
                                                                   inputs["dst"],
                                                                   inputs["f107"],
                                                                   pred=preds,
                                                                   crust_nmax=45,
                                                                   MJD_time=inputs["date"],
                                                                   geodflag=0)

    '''if field == "core":
        res = {"Bx": -core[1], "By": core[2], "Bz": -core[0]}
    elif field == "crust":
        res = {"Bx": -crust[1], "By": crust[2], "Bz": -crust[0]}
    elif field == "magneto":
        res = {"Bx": -magnetosphere[1], "By": magnetosphere[2], "Bz": -magnetosphere[0]}
    elif field == "iono":
        res = {"Bx": -ionosphere[1], "By": ionosphere[2], "Bz": -ionosphere[0]}
    else:
        raise ValueError("Invalid field specified. Choose from 'core', 'crust', 'magnetosphere', or 'ionosphere'.")'''

    if field == "core":
        Bx, By, Bz = spherical_vector_to_geodetic(core[0], core[1], core[2], geoc_lats, inputs["latitude"])
    elif field == "crust":
        Bx, By, Bz = spherical_vector_to_geodetic(crust[0], crust[1], crust[2], geoc_lats, inputs["latitude"])
    elif field == "magneto":
        Bx, By, Bz = spherical_vector_to_geodetic(magnetosphere[0], magnetosphere[1], magnetosphere[2], geoc_lats, inputs["latitude"])
    elif field == "iono":
        Bx, By, Bz = spherical_vector_to_geodetic(ionosphere[0], ionosphere[1], ionosphere[2], geoc_lats, inputs["latitude"])
    else:
        raise ValueError("Invalid field specified. Choose from 'core', 'crust', 'magneto', or 'iono'.")

    outputs['Bx'] = Bx
    outputs['By'] = By
    outputs['Bz'] = Bz

    return outputs


def compare_results(fortran_outputs: dict, python_outputs: dict, stat_results_file: str):
    measure_diff(fortran_outputs, python_outputs, stat_results_file)


def main():
    # Compares output from Python and C/Fortran interfaces for same inputs

    # Run after calling Fortran CM4 via C (create_cm4_arr_results.c)
    # and generating _TestValues CSV files

    curr_dir = os.path.dirname(os.path.abspath(__file__))
    if not os.path.exists(os.path.join(curr_dir, "results")):
        os.mkdir(os.path.join(curr_dir, "results"))

    testval_dict = {"core": "geoc_cm4arr_core_TestValues.csv",
                    "crust": "geoc_cm4arr_crust_TestValues.csv",
                    "magneto": "geoc_cm4arr_magneto_TestValues.csv",
                    "iono": "geoc_cm4arr_iono_TestValues.csv"}

    for key, filename in testval_dict.items():
        testval_filename = os.path.join(curr_dir, "test_values", filename)
        pyoutputs_filename = os.path.join(curr_dir, "test_values", f"cm4py_{key}_TestValues.csv")
        inputs, fortran_outputs = read_inputs(testval_filename)
        # Create python outputs
        python_outputs = generate_python_output(inputs, field=key)
        write_python_output(python_outputs, pyoutputs_filename)
        # Compare Python and C/Fortran outputs for same inputs
        results_filename = os.path.join(curr_dir, "results", f"{key}_results.csv")
        print(key)
        compare_results(fortran_outputs, python_outputs, results_filename)


if __name__ == "__main__":
    main()