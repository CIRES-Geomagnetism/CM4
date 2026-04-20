#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define RAD2DEG(rad)    ((rad)*(180.0L/M_PI))
#define DEG2RAD(deg)    ((deg)*(M_PI/180.0L))



extern void call_cm4_arr(double* UT,double* thet, double* phi, double* alt,
     double* dst, double* f107, bool* pred1, bool* pred2, bool* pred3, bool* pred4, bool* pred5, bool* pred6,
     bool* CORD, int* NHMF1, int* NHMF2, int* NLMF1, int* NLMF2, int* N, char* cof_path, double* bmdl, double* jmdl);

void print_results(double B[3][7][3000]) {

    for (int i = 0; i< 10; i++){
        for (int j = 0; j < 3; j++){
            for (int k = 0; k < 7; k++){
            printf("results[%d][%d][%d] = %lf\n", j, k, i, B[j][k][i]);
        }
    }
   }
}

void rotate_magvec(double* Bx, double* By, double* Bz, double geoc_lat, double geod_lat){

    double psi;

    double orig_Bx, orig_Bz;
    psi = (M_PI / 180.0) * (geoc_lat - geod_lat);
    orig_Bx = *Bx;
    orig_Bz = *Bz;
    *Bz = orig_Bx * sin(psi) + orig_Bz * cos(psi);
    *Bx = orig_Bx * cos(psi) - orig_Bz * sin(psi);

}

double geod_to_geocentric(double geod_lat, double ellip_alt){

    double CosLat, SinLat, rc, xp, zp; /*all local variables */
    double ellip_a, ellip_b, ellip_f, eps, epssq; /* WGS-84 ellipsoid parameters */
    double r, phig;

    /*
     ** Convert geodetic coordinates, (defined by the WGS-84
     ** reference ellipsoid), to Earth Centered Earth Fixed Cartesian
     ** coordinates, and then to spherical coordinates.
     */

    CosLat = cos(DEG2RAD(geod_lat));
    SinLat = sin(DEG2RAD(geod_lat));
    ellip_a = 6378.137;
    ellip_f = 1 / 298.257223563;
    ellip_b = ellip_a * (1 - ellip_f);
    eps = sqrt(1 - (ellip_b * ellip_b) / (ellip_a * ellip_a)); /*first eccentricity */
    epssq = (eps * eps);

    /* compute the local radius of curvature on the WGS-84 reference ellipsoid */

    rc = ellip_a / sqrt(1.0 - epssq * SinLat * SinLat);

    /* compute ECEF Cartesian coordinates of specified point (for longitude=0) */

    xp = (rc + ellip_alt) * CosLat;
    zp = (rc * (1.0 - epssq) + ellip_alt) * SinLat;

    /* compute spherical radius and angle lambda and phi of specified point */

    r = sqrt(xp * xp + zp * zp);
    phig = RAD2DEG(asin(zp / r)); /* geocentric latitude */

    return phig;
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

void write_outputs(double* date, double* lat, double* lon, double* alt, double* dst, double* f107, double bmdl[3][7][3000], FILE* fptw, char key, int N){

    // Write the results to the output files

    
    // if field == "core":
    //     core = np.array([-out_b[2,0], -out_b[0,0],out_b[1,0]])
    //     res = {"Bx": -core[1], "By": core[2], "Bz": -core[0]}
    // elif field == "crust":
    //      crust = np.array([-out_b[2,1], -out_b[0,1],out_b[1,1]])
    //     res = {"Bx": -crust[1], "By": crust[2], "Bz": -crust[0]}
    // elif field == "magneto":
    //      magnetosphere = np.array([-out_b[2,2]-out_b[2,3], -out_b[0,2]-out_b[0,3],out_b[1,2]+out_b[1,3]])
    //     res = {"Bx": -magnetosphere[1], "By": magnetosphere[2], "Bz": -magnetosphere[0]}
    // elif field == "iono":
    //      ionoshere = np.array([-out_b[2,4]-out_b[2,5], -out_b[0,4]-out_b[0,5],out_b[1,4]+out_b[1,5]])
    //     res = {"Bx": -ionosphere[1], "By": ionosphere[2], "Bz": -ionosphere[0]}
    // else:
    //     raise ValueError("Invalid field specified. Choose from 'core', 'crust', 'magnetosphere', or 'ionosphere'.")


    for (int i = 0; i < N ;i++){

        double bx, by, bz;
        if (key == 's'){
            bz = bmdl[2][1][i];
            bx = bmdl[0][1][i];
            by = bmdl[1][1][i];
        }else if (key == 'r'){
            bz = bmdl[2][0][i];
            bx = bmdl[0][0][i];
            by = bmdl[1][0][i];
        }else if (key == 'i'){
            bz = bmdl[2][4][i] + bmdl[2][5][i];
            bx = bmdl[0][4][i] + bmdl[0][5][i];
            by = bmdl[1][4][i] + bmdl[1][5][i];
        }else if (key == 'm'){
            bz = bmdl[2][2][i] + bmdl[2][3][i];
            bx = bmdl[0][2][i] + bmdl[0][3][i];
            by = bmdl[1][2][i] + bmdl[1][3][i];
        }

        fprintf(fptw, "%lf %lf %lf %lf %lf %lf %lf %lf %lf\n",
                date[i], lat[i], lon[i], alt[i], dst[i], f107[i], bx, by, bz);
    }

}

int main(){

    char inputs_file[50] = "test_values/cm4_fortran_inputs.csv";
    char line[200];
    int N = 3000;


    char crust_out_file[50] = "test_values/cm4arr_crust_TestValues.csv";
    char core_out_file[50] = "test_values/cm4arr_core_TestValues.csv";
    char iono_out_file[50] = "test_values/cm4arr_iono_TestValues.csv";
    char magneto_out_file[50] = "test_values/cm4arr_magneto_TestValues.csv";

    bool pred1 = true, pred2 = true, pred3  = true, pred4  = true, pred5  = true, pred6 = true;
    bool CORD = true; // True for geodetic; False for geocentric
    int NHMF1 = 13, NHMF2 = 45, NLMF1 = 1, NLMF2 = 14;
    char cof_path[512] = "umdl.CM4";
    double jmdl[3][4];

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

    double* lats = malloc(N*sizeof(double));
    double* lons = malloc(N*sizeof(double));
    double* uts = malloc(N*sizeof(double));
    double* alts = malloc(N*sizeof(double));
    double* dsts = malloc(N*sizeof(double));
    double* f107s = malloc(N*sizeof(double));
    double bmdl[3][7][N];
    double B[3][7][N];


    FILE* fp =  fopen(inputs_file, "r");
    if (!fp || !fpw_s || !fpw_r || !fpw_i || !fpw_m) {
        fprintf(stderr, "Failed to open input/output files. inputs='%s' coeffs='%s'\n", inputs_file, cof_path);
        if (fp) fclose(fp);
        if (fpw_s) fclose(fpw_s);
        if (fpw_r) fclose(fpw_r);
        if (fpw_i) fclose(fpw_i);
        if (fpw_m) fclose(fpw_m);
        return 1;
    }
    

        
     // Read the first line (header)
   fgets(line,sizeof(line), fp);

   int idx = 0;
   while(fgets(line, sizeof(line), fp) != NULL){
        double lat, lon;
        double ut, thet, alt, dst, f107;

        sscanf(line,"%lf %lf %lf %lf %lf %lf",
                 &ut,&lat,&lon,&alt,&dst,&f107);


        lats[idx] = 90. - lat;
        lons[idx] = lon;
        alts[idx] = alt;
        uts[idx] = ut;
        dsts[idx] = dst;
        f107s[idx] = f107;

        idx += 1;

   }

    fclose(fp);

   call_cm4_arr(uts,lats,lons,alts,dsts,f107s,
                    &pred1,&pred2,&pred3,&pred4,&pred5,&pred6,
                    &CORD,
                    &NHMF1,&NHMF2,
                    &NLMF1,&NLMF2,
                    &N,
                    cof_path,
                    (double*) bmdl,
                    (double*) jmdl);

   fortran_to_c_order((double*)bmdl, (double*)B, 3, 7, N);

   //print_results();



   write_header(fpw_s);
   write_outputs(uts, lats, lons, alts, dsts, f107s, B, fpw_s, 's', N);
   fclose(fpw_s);

   write_header(fpw_r);
   write_outputs(uts, lats, lons, alts, dsts, f107s, B, fpw_r, 'r', N);
   fclose(fpw_r);

   write_header(fpw_i);
   write_outputs(uts, lats, lons, alts, dsts, f107s, B, fpw_i, 'i', N);
   fclose(fpw_i);

   write_header(fpw_m);
   write_outputs(uts, lats, lons, alts, dsts, f107s, B, fpw_m, 'm', N);
   fclose(fpw_m);


    // Free allocated memory
    free(lats);
    free(lons);
    free(uts);
    free(alts);
    free(dsts);
    free(f107s);

    return 0;
}
