#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cm4_c_library.h"

void print_results(double B[3][7][3000]) {

    for (int i = 0; i< 10; i++){
        for (int j = 0; j < 3; j++){
            for (int k = 0; k < 7; k++){
            printf("results[%d][%d][%d] = %lf\n", j, k, i, B[j][k][i]);
        }
    }
   }
}

void spherical_vector_to_geodetic(double bx, double by, double bz, double geod_lat, double geoc_lat, Results* results){
/*
Inputs:
    Bt: magnetic elements theta (vector component in spherical polar direction)
            Bp: magnetic elements phi (vector component in spherical azimuthal direction)
            Br: magnetic elements radius (vector component in spherical radial direction)
            geoc_lat: geocentric latitude
            geod_lat: geeodetic latitude
    results: struct containing Bx, By, Bz in geodetic coordinates
*/


    double psi;


    psi = (M_PI / 180.0) * (geoc_lat - geod_lat);

    results->Bz = bx * sin(psi) + bz * cos(psi);
    results->Bx = bx * cos(psi) - bz * sin(psi);
    results->By = by;

    //results->Bx = -results->Bx; /* convert to geodetic coordinates */
    //results->Bz = -results->Bz; /* convert to geodetic coordinates */

}

void geod_to_geocentric(double geod_lat, double ellip_alt, CoordSpherical *coord){

    double CosLat, SinLat, rc, xp, zp; /*all local variables */
    double ellip_a, ellip_b, ellip_f, eps, epssq, earth_radius_km; /* WGS-84 ellipsoid parameters */


    /*
     ** Convert geodetic coordinates, (defined by the WGS-84
     ** reference ellipsoid), to Earth Centered Earth Fixed Cartesian
     ** coordinates, and then to spherical coordinates.
     */

    CosLat = cos(DEG2RAD(geod_lat));
    SinLat = sin(DEG2RAD(geod_lat));
    ellip_a = 6378.137;
    ellip_f = 1 / 298.257223563;
    earth_radius_km = 6371.2;

    ellip_b = ellip_a * (1 - ellip_f);
    eps = sqrt(1 - (ellip_b * ellip_b) / (ellip_a * ellip_a)); /*first eccentricity */
    epssq = (eps * eps);

    /* compute the local radius of curvature on the WGS-84 reference ellipsoid */

    rc = ellip_a / sqrt(1.0 - epssq * SinLat * SinLat);

    /* compute ECEF Cartesian coordinates of specified point (for longitude=0) */

    xp = (rc + ellip_alt) * CosLat;
    zp = (rc * (1.0 - epssq) + ellip_alt) * SinLat;

    /* compute spherical radius and angle lambda and phi of specified point */

    coord->r = sqrt(xp * xp + zp * zp);
    coord->r_alt = coord->r - earth_radius_km;
    coord->phig = RAD2DEG(asin(zp / coord->r)); /* geocentric latitude */


}

void fortran_to_c_order(double* f_array, double* c_array, int row, int col, int depth) {
    // Convert a 3D Fortran array to a C-style 3D array
    for (int i = 0; i < row; i++) {
        for (int j = 0; j < col; j++) {
            for (int k = 0; k < depth; k++) {
                int f_index = i + j * row + k * row * col; // Fortran order: (i, j, k)
                int c_index = k + j * depth + i * depth * col; // C order: (k, j, i)

                c_array[c_index] = f_array[f_index];
            }
        }
    }
}

void write_header(FILE* fpw){
    fprintf(fpw, "date,latitude,longitude,altitude,dst,f107,Bx,By,Bz\n");
}

void write_outputs(double* date, double* lat, double* lon, double* alt, double* dst, double* f107, int N, double bmdl[3][7][N], FILE* fptw, char* key, double* geoc_lat, double* rad_alt){

    // Write the results to the output files
    Results results;
    for (int i = 0; i < N ;i++){

        double bx, by, bz;
        if (strcmp(key, "crust") == 0){
            bz = bmdl[2][1][i];
            bx = bmdl[0][1][i];
            by = bmdl[1][1][i];
        }else if (strcmp(key, "core") == 0){
            bz = bmdl[2][0][i];
            bx = bmdl[0][0][i];
            by = bmdl[1][0][i];
        }else if (strcmp(key, "iono") == 0){
            bz = bmdl[2][4][i] + bmdl[2][5][i];
            bx = bmdl[0][4][i] + bmdl[0][5][i];
            by = bmdl[1][4][i] + bmdl[1][5][i];
        }else if (strcmp(key, "magneto") == 0){
            bz = bmdl[2][2][i] + bmdl[2][3][i];
            bx = bmdl[0][2][i] + bmdl[0][3][i];
            by = bmdl[1][2][i] + bmdl[1][3][i];
        }else{
            fprintf(stderr, "Error: unknown field was assigned to -k '%s'. Use 'crust', 'core', 'iono', or 'magneto'.\n", key);
            return;
        }

        spherical_vector_to_geodetic(bx, by, bz, lat[i], 90 - geoc_lat[i], &results);

        fprintf(fptw, "%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf,%lf\n",
                date[i], lat[i], lon[i], alt[i], dst[i], f107[i], results.Bx, results.By, results.Bz);
    }

}

int load_inputs(double* lats, double* lons, double* alts, double* uts, double* dsts, double* f107s, double* geocLat, double* radAlt, int N, const char* inputs_file){

    printf("Get inputs file %s\n", inputs_file);
    FILE* fp = fopen(inputs_file, "r");
    CoordSpherical sph_coord;
    char line[200];

     // Read the first line (header)
   fgets(line,sizeof(line), fp);
   printf("Start reading inputs from %s\n", inputs_file);

   int idx = 0;
   while(fgets(line, sizeof(line), fp) != NULL){
        double lat, lon, colat;
        double ut, thet, alt, dst, f107;

        if (idx >= N) {
            fprintf(stderr, "Warning: input file has more than %d rows; extra rows ignored.\n", N);
            break;
        }

        int parsed = sscanf(line,"%lf %lf %lf %lf %lf %lf",
                 &ut,&lat,&lon,&alt,&dst,&f107);
        if (parsed != 6) {
            fprintf(stderr, "Error: expected 6 values on line %d, got %d. Line: %s\n",
                    idx + 2, parsed, line);
            fclose(fp);
            free(lats); free(lons); free(uts); free(alts); free(dsts); free(f107s);
            exit(EXIT_FAILURE);
        }

        lats[idx] = lat;
        lons[idx] = lon;
        alts[idx] = alt;
        uts[idx] = ut;
        dsts[idx] = dst;
        f107s[idx] = f107;

        geod_to_geocentric(lat, alt, &sph_coord);
        geocLat[idx] = 90 - sph_coord.phig;
        radAlt[idx] = sph_coord.r_alt;

        sph_coord.phig = 0;
        sph_coord.r_alt = 0;

        idx += 1;

   }

   return idx;

}