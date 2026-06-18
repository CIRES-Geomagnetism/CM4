# Copyright (c) Regents of the University of Colorado, June 1, 2025
import datetime as dt
import random

import pandas as pd



def add_nan_data(inputs: list) -> list:

    N = len(inputs)
    num_empty = int (N // random.randint(40, 50))
    num_space = int(N // random.randint(20, 30))
    num_string = int (N // random.uniform(10, 25))

    # add empty data
    empty_idx = [random.randint(0, N-1) for _ in range(num_empty)]

    for i in empty_idx:
        inputs[i] = ""


    return inputs


def generate_random_time_str(start_time: str, end_time: str, num_data: int) -> list[str]:
    time_fmt = "%Y-%m-%d %H:%M:%S"
    time_s = dt.datetime.strptime(start_time, time_fmt)
    time_r = dt.datetime.strptime(end_time, time_fmt)

    total_seconds = int((time_r - time_s) / dt.timedelta(seconds=1))

    dt_arr = ["2025-05-05 00:00:00"] * num_data

    for i in range(num_data):
        dt_obj = time_s + dt.timedelta(seconds=random.uniform(0, total_seconds))
        dt_arr[i] = dt_obj.strftime(time_fmt)

    return dt_arr

def generate_random_inputs(start_time, end_time, input_size: int, out_file:str, add_nan: bool=True, add_invalid: bool=False):

    inputs = {"date": [],
              "latitude": [],
              "longitude": [],
              "altitude": []}

    if add_invalid == False:
        lats = [random.uniform(-90, 90) for _ in range(input_size)]
        lons = [random.uniform(-180, 360) for _ in range(input_size)]
        alts = [random.uniform(-1, 1000) for _ in range(input_size)]
    else:
        lats = [random.uniform(-121, 150) for _ in range(input_size)]
        lons = [random.uniform(-165, 385) for _ in range(input_size)]
        alts = [random.uniform(-11, 1033) for _ in range(input_size)]


    dates = generate_random_time_str(start_time, end_time, input_size)

    if add_nan:
        inputs["latitude"] = add_nan_data(lats)
        inputs["longitude"] = add_nan_data(lons)
        inputs["altitude"] = add_nan_data(alts)
    else:
        inputs["latitude"] = lats
        inputs["longitude"] = lons
        inputs["altitude"] = alts
    inputs["date"] = dates

    df = pd.DataFrame.from_dict(inputs)

    df.to_csv(out_file, index=False)

def main():

    input_size = 5000
    file_name = "small_inputs.csv"
    start_time = "1999-11-01 00:00:00"
    end_time = "2025-06-01 23:00:00"
    generate_random_inputs(start_time, end_time, input_size, file_name)

    input_size = 100000
    file_name = "large_inputs.csv"
    generate_random_inputs(start_time, end_time, input_size, file_name)


if __name__=="__main__":
    main()




