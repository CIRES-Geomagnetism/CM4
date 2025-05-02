// wrapper.c
#include <stdio.h>
#include <stdbool.h>

extern void call_cm4(double* UT,double* thet, double* phi, double* alt,
     double* dst, double* f107, bool* pred1, bool* pred2, bool* pred3, bool* pred4, bool* pred5, bool* pred6,
     bool* CORD, int* NHMF1, int* NHMF2, int* NLMF1, int* NLMF2, char* cof_path, double* bmdl, double* jmdl);

extern void call_cm4_arr(double* UT,double* thet, double* phi, double* alt,
     double* dst, double* f107, bool* pred1, bool* pred2, bool* pred3, bool* pred4, bool* pred5, bool* pred6,
     bool* CORD, int* NHMF1, int* NHMF2, int* NLMF1, int* NLMF2, int* N, char* cof_path, double* bmdl, double* jmdl);

extern void call_cm4_org(double* UT,double* thet, double* phi, double* alt,
     double* dst, double* f107, bool* pred1, bool* pred2, bool* pred3, bool* pred4, bool* pred5, bool* pred6,
     bool* CORD, int* NHMF1, int* NHMF2, int* NLMF1, int* NLMF2, double* bmdl, double* jmdl);

extern void fortran_add(double* x, double* y, double* result);

int main(){

    double UT, thet, phi, alt, dst, f107;

    bool cord;
    bool pred1 = true;
    bool pred2 = true;
    bool pred3 = true;
    bool pred4 = true;
    bool pred5 = true;
    bool pred6 = true;
    char cof_path[256] = "umdl.CM4";

    UT = 1990;
    thet = 25.5;
    phi = 30;
    alt = 0;
    dst = 5.5;
    f107 = 125;
    cord = false;

    int N = 10;
    double UT_arr[N];
    double thet_arr[N];
    double phi_arr[N];
    double alt_arr[N];
    double dst_arr[N];
    double f107_arr[N];

    for (int i = 0; i < N; i++){
        UT_arr[i] = UT + 0.1*i;
        thet_arr[i] = thet + 0.1*i;
        phi_arr[i] = phi + 0.1*i;
        alt_arr[i] = alt + 0.1*i;
        dst_arr[i] = dst + 0.1*i;
        f107_arr[i] = f107 + 0.1*i;
    }



    int NHMF1 = 13, NHMF2 = 45, NLMF1 = 1, NLMF2 = 14;

    double bmdl[3][7][N], jmdl[3][4];



    double a = 5.0, b = 7.0, res;


    //printf("pred[0]: %d", pred[0]);

    /*call_cm4_org(&UT, &thet , &phi, &alt, &dst, &f107,
                                      &pred1, &pred2, &pred3,&pred4, &pred5, &pred6,
                                      &cord,
                                      &NHMF1,&NHMF2, &NLMF1,&NLMF2, (double*)bmdl, (double*)jmdl);

    call_cm4(&UT, &thet , &phi, &alt, &dst, &f107,
                                      &pred1, &pred2, &pred3,&pred4, &pred5, &pred6
                                      ,&cord,
                                      &NHMF1,&NHMF2, &NLMF1,&NLMF2, cof_path, (double*)bmdl, (double*)jmdl);*/

    call_cm4_arr(UT_arr, thet_arr , phi_arr, alt_arr, dst_arr, f107_arr,
                                      &pred1, &pred2, &pred3,&pred4, &pred5, &pred6
                                      ,&cord,
                                      &NHMF1,&NHMF2, &NLMF1,&NLMF2, &N, cof_path, (double*)bmdl, (double*)jmdl);


    // Print results
    printf("bmdl[0][1][0] = %f\n", bmdl[0][1][0]);
    printf("jmdl[0][1] = %f\n", jmdl[0][0]);



    return 0;

}