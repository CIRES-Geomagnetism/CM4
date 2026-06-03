#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "cm4_c_library.h"
extern void call_cm4_arr(double* UT,double* thet, double* phi, double* alt,
     double* dst, double* f107, bool* pred1, bool* pred2, bool* pred3, bool* pred4, bool* pred5, bool* pred6,
     bool* CORD, int* NHMF1, int* NHMF2, int* NLMF1, int* NLMF2, int* N, char* cof_path, double* bmdl, double* jmdl);


int main(){

    char inputs_file[50] = "test_values/cm4_fortran_inputs.csv";
    char line[200];
    int N = 3000;


    char crust_out_file[50] = "test_values/geoc_cm4arr_crust_TestValues.csv";
    char core_out_file[50] = "test_values/geoc_cm4arr_core_TestValues.csv";
    char iono_out_file[50] = "test_values/geoc_cm4arr_iono_TestValues.csv";
    char magneto_out_file[50] = "test_values/geoc_cm4arr_magneto_TestValues.csv";

    bool pred1 = true, pred2 = true, pred3  = true, pred4  = true, pred5  = true, pred6 = true;
    bool CORD = false; // True for geodetic; False for geocentric
    int NHMF1 = 13, NHMF2 = 45, NLMF1 = 1, NLMF2 = 14;
    char cof_path[512] = "umdl.CM4";
    double jmdl[3][4];

    CoordSpherical sph_coord;
    Results results;

    const char* cof_env = getenv("CM4_COEFF_PATH");
    if (cof_env != NULL && cof_env[0] != '\0') {
        strncpy(cof_path, cof_env, sizeof(cof_path) - 1);
        cof_path[sizeof(cof_path) - 1] = '\0';
    }
    
    // Open the file for writing
    FILE* fpw_s = fopen(crust_out_file, "w");
    FILE* fpw_r = fopen(core_out_file, "w");
    FILE* fpw_i = fopen(iono_out_file, "w");
    FILE* fpw_m = fopen(magneto_out_file, "w");
    // Open the file for reading

    double* lats  = malloc(N*sizeof(double));
    double* lons  = malloc(N*sizeof(double));
    double* uts   = malloc(N*sizeof(double));
    double* alts  = malloc(N*sizeof(double));
    double* dsts  = malloc(N*sizeof(double));
    double* f107s = malloc(N*sizeof(double));
    double* geocLat = malloc(N*sizeof(double));
    double* radAlt = malloc(N*sizeof(double));


    double bmdl[3][7][N];
    double B[3][7][N];

    FILE* fp = fopen(inputs_file, "r");

     // Read the first line (header)
   fgets(line,sizeof(line), fp);

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
            fclose(fpw_s); fclose(fpw_r); fclose(fpw_i); fclose(fpw_m);
            free(lats); free(lons); free(uts); free(alts); free(dsts); free(f107s);
            return 1;
        }

        colat = 90 - lat;
        lats[idx] = colat;
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
   N = idx; /* use actual row count, not the declared max */

    fclose(fp);



   call_cm4_arr(uts,geocLat,lons,radAlt,dsts,f107s,
                    &pred1,&pred2,&pred3,&pred4,&pred5,&pred6,
                    &CORD,
                    &NHMF1,&NHMF2,
                    &NLMF1,&NLMF2,
                    &N,
                    cof_path,
                    (double*) bmdl,
                    (double*) jmdl);

   fortran_to_c_order((double*)bmdl, (double*)B, 3, 7, N);


   write_header(fpw_s);
   write_outputs(uts, lats, lons, alts, dsts, f107s, B, fpw_s, 's', N, geocLat, radAlt);
   fclose(fpw_s);

   write_header(fpw_r);
   write_outputs(uts, lats, lons, alts, dsts, f107s, B, fpw_r, 'r', N, geocLat, radAlt);
   fclose(fpw_r);

   write_header(fpw_i);
   write_outputs(uts, lats, lons, alts, dsts, f107s, B, fpw_i, 'i', N, geocLat, radAlt);
   fclose(fpw_i);

   write_header(fpw_m);
   write_outputs(uts, lats, lons, alts, dsts, f107s, B, fpw_m, 'm', N, geocLat, radAlt);
   fclose(fpw_m);


    // Free allocated memory
    free(lats);
    free(lons);
    free(uts);
    free(alts);
    free(dsts);
    free(f107s);
    free(geocLat);
    free(radAlt);

    return 0;
}
