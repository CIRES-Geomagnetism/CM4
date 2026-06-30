#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>

#include "cm4_c_library.h"
extern void call_cm4_arr(double* UT,double* thet, double* phi, double* alt,
     double* dst, double* f107, bool* pred1, bool* pred2, bool* pred3, bool* pred4, bool* pred5, bool* pred6,
     bool* CORD, int* NHMF1, int* NHMF2, int* NLMF1, int* NLMF2, int* N, char* cof_path, double* bmdl, double* jmdl);



int main(int argc, char* argv[]) {

    char inputs_file[80];

    int N = 3000;
    int opt;
    char out_file[80];
    char key[10];

    bool pred1 = true, pred2 = true, pred3  = true, pred4  = true, pred5  = true, pred6 = true;
    bool CORD = false; // True for geodetic; False for geocentric
    int NHMF1 = 13, NHMF2 = 45, NLMF1 = 1, NLMF2 = 14;
    char cof_path[512] = "umdl.CM4";
    double jmdl[3][4];


    Results results;

    const char* cof_env = getenv("CM4_COEFF_PATH");
    if (cof_env != NULL && cof_env[0] != '\0') {
        strncpy(cof_path, cof_env, sizeof(cof_path) - 1);
        cof_path[sizeof(cof_path) - 1] = '\0';
    }



    while ((opt = getopt(argc, argv, "i:f:k:")) != -1) {
        switch (opt) {
            case 'i':
                strncpy(inputs_file, optarg, sizeof(inputs_file) - 1);
                inputs_file[sizeof(inputs_file) - 1] = '\0';
                break;
            case 'f':
                strncpy(out_file, optarg, sizeof(inputs_file) - 1);
                out_file[sizeof(out_file) - 1] = '\0';
                break;
            case 'k':
                strncpy(key, optarg, sizeof(key)-1);
                key[sizeof(key) - 1] = '\0';
                break;

            default:
                fprintf(stderr, "Usage: %s [-i the path to the inputs file] [-f the path of output files] [-k the field whcih testvalue file based on]\n", argv[0]);
                return 1;
        }
    }
    

    // Open the file for reading

    double* lats  = malloc(N*sizeof(double));
    double* lons  = malloc(N*sizeof(double));
    double* uts   = malloc(N*sizeof(double));
    double* alts  = malloc(N*sizeof(double));
    double* dsts  = malloc(N*sizeof(double));
    double* f107s = malloc(N*sizeof(double));
    double* geocLat = malloc(N*sizeof(double));
    double* radAlt = malloc(N*sizeof(double));

    load_inputs(lats, lons, alts, uts, dsts, f107s, geocLat, radAlt, N, inputs_file);


    double bmdl[3][7][N];
    double B[3][7][N];

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


   FILE* fpw = fopen(out_file, "w");
   write_header(fpw);
   write_outputs(uts, lats, lons, alts, dsts, f107s, B, fpw, key, N, geocLat, radAlt);
   fclose(fpw);

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
