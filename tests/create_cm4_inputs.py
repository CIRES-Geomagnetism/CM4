# Copyright (c) Regents of the University of Colorado, June 1, 2025
import math

import pandas as pd
import datetime as dt
from geomaglib.util import calc_dec_year

from create_dummy_inputs import generate_random_inputs
from HDGM_XT.compute import _utils

def create_cm4_fortran_inputs(dates, lats, lons, alts, dsts, f107s, out_file):

    N = len(dates)

    header = {"date": [],
              "lat":[],
              "lon":[],
              "alt":[],
              "dst": [],
              "f107":[]}

    df = pd.DataFrame(header)

    df["date"] = dates
    df["lat"] = lats
    df["lon"] = lons
    df["alt"] = alts

    df["f107"] = f107s
    df["dst"] = dsts

    df = df.ffill()

    df.to_csv(out_file, sep="\t", index=False)



def create_cm4_inputs():

    test_input_file = "model_test_io/cm4_inputs.csv"
    fortran_input_file = "model_test_io/cm4_fortran_inputs.csv"

    out_size = 3000

    start_time = "2000-01-01 00:00:00"
    end_time = "2002-12-31 23:59:00"

    generate_random_inputs(start_time, end_time, out_size, test_input_file, add_nan=False)

    inp_df = pd.read_csv(test_input_file)

    dec_years = inp_df["date"].tolist()
    dyears = []

    for dyear in dec_years:
        date_object = dt.datetime.strptime(dyear, "%Y-%m-%d %H:%M:%S")

        dec_year = calc_dec_year(date_object.year, date_object.month, date_object.day, date_object.hour,
                                 date_object.minute, date_object.second)

        dyears.append(dec_year)

    swx = _utils.get_space_weather_data(dyears)


    f107 = swx['f107_daily']
    dst = swx['dst']


    lat = inp_df["latitude"]
    lon = inp_df["longitude"]
    alt= inp_df["altitude"]

    create_cm4_fortran_inputs(dyears,lat, lon, alt, dst, f107, fortran_input_file)

def main():
    create_cm4_inputs()

if __name__ == "__main__":
    main()


