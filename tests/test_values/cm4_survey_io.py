from geomaglib.util import calc_dec_year
def read_inputs(input_file):


    lat, lon, date = [], [], []

    with open(input_file, "r") as f:
        for line in f:
            vals = line.strip().split(",")

            try:
                survey_id = int(vals[0])
            except ValueError:
                continue

            lat.append(float(vals[1]))
            lon.append(float(vals[2]))
            date_str = str(float(vals[3]))
            year, month, day, hour, minute = parse_time(str(float(vals[3])))

            if day == 0:
                day = 30
                month -= 1
            print(f"{year}-{month:02d}-{day:02d} {hour:02d}:{minute:02d}")
            decYear = calc_dec_year(year, month, day, hour, minute)
            date.append(decYear)


    return lat, lon, date

def create_inputs_file(lat, lon, date, output_file):

    alt = 0
    dst = [-4, -4, -4, -4, -4, -4, -84, -84, -84, -84, -84, -84, -84]
    f107 = [63.2, 63.2, 63.2, 63.2, 63.2, 63.2, 171.3, 171.3, 171.3, 171.3, 171.3, 171.3, 171.3]

    with open(output_file, "w") as f:
        f.write("date,lat,lon,alt,dst,f107\n")
        for i in range(len(lat)):
            f.write(f"{date[i]},{lat[i]},{lon[i]},{alt},{dst[i]},{f107[i]}\n")

def parse_time(time_str):
    year = int(time_str[0:4])
    month = int(time_str[4:6])
    day = int(time_str[6:8])
    # if(day == 0):
    # day =1
    hour = int(time_str[8:10])
    # if(hour ==0):
    # hour = 1
    minute = int(time_str[10:12])
    # second = int(time_str[12:14])

    return year, month, day, hour, minute

def main():
    input_file = "Core_unittest_inputs.csv"
    output_file = "cm4_survey_inputs.csv"

    lat, lon, date = read_inputs(input_file)
    create_inputs_file(lat, lon, date, output_file)

if __name__ == "__main__":
     main()




