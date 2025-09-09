import math
import os
import copy
    
import numpy as np
import pandas as pd
from collections import defaultdict

import cm4
from cm4.callfpy import py_mat_cm4_arr

def read_inputs(filename: str):

    inp_keys = ["date","latitude", "longitude", "altitude", "dst", "f107"]
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
    diffs = [0]*N

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
                    #f.write(f"{key},{true_vals[key][i]},{pred_vals[key][i]}\n")
                #    raise ValueError(f"In {out_file}, Difference for {key} at index {i} exceeds tolerance: {diff} > {tol}. True: {true_vals[key][i]} Pred: {pred_vals[key][i]}")
                #else:
                #    f.write(f"{key},{true_vals[key][i]},{pred_vals[key][i]}\n")

            ave_diff = ave_diff / N

            rmse = math.sqrt(sum((diff - ave_diff)**2 for diff in diffs) / N)

            f.write(f"{key},{max_diff},{max_diff_ind},{ave_diff},{rmse}\n")

def generate_python_output(inputs: dict, field: str):
    
    outputs = copy.deepcopy(inputs)

    preds = [True, True, True, True, True, True]

    out_b, core, crust, magnetosphere, ionosphere = py_mat_cm4_arr(inputs["altitude"], 
                                                                   inputs["latitude"], 
                                                                   inputs["longitude"], 
                                                                   inputs["dst"],
                                                                   inputs["f107"], 
                                                                   pred=preds, 
                                                                   MJD_time=inputs["date"], 
                                                                   geodflag=1)
    
    if field == "core":
        res = {"Bx": -core[1], "By": core[2], "Bz": -core[0]}
    elif field == "crust":
        res = {"Bx": -crust[1], "By": crust[2], "Bz": -crust[0]}
    elif field == "magneto":
        res = {"Bx": -magnetosphere[1], "By": magnetosphere[2], "Bz": -magnetosphere[0]}
    elif field == "iono":
        res = {"Bx": -ionosphere[1], "By": ionosphere[2], "Bz": -ionosphere[0]}
    else:
        raise ValueError("Invalid field specified. Choose from 'core', 'crust', 'magnetosphere', or 'ionosphere'.")

    outputs['Bx']=res['Bx']
    outputs['By']=res['By']
    outputs['Bz']=res['Bz']
    return outputs

def compare_results(fortran_outputs:dict, python_outputs:dict, stat_results_file: str): 
    measure_diff(fortran_outputs, python_outputs, stat_results_file)

def main():

    curr_dir = os.path.dirname(os.path.abspath(__file__))
    if not os.path.exists(os.path.join(curr_dir, "results")):
        os.mkdir(os.path.join(curr_dir, "results"))

    testval_dict = {"core": "cm4arr_core_TestValues.csv", 
                    "crust": "cm4arr_crust_TestValues.csv", 
                    "magneto": "cm4arr_magneto_TestValues.csv", 
                    "iono": "cm4arr_iono_TestValues.csv"}

    for key, filename in testval_dict.items():
        testval_filename = os.path.join(curr_dir, "test_values", filename)
        results_filename = os.path.join(curr_dir, "results", f"{key}_results.csv")
        pyresults_filename = os.path.join(curr_dir, "results", f"cm4py_{key}_TestValues.csv")
        inputs, fortran_outputs = read_inputs(testval_filename)
        python_outputs = generate_python_output(inputs,field=key)
        pd.DataFrame(python_outputs).to_csv(pyresults_filename)
        compare_results(fortran_outputs, python_outputs, key, results_filename)


if __name__ == "__main__":
    main()
