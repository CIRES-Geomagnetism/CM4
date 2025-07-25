#include <stdio.h>
#include <stdbool.h>
#include "ccm4.h"




/*void call_cm4_point(double UT, double thet, double phi, double alt,
     double dst, double f107, bool pred1, bool pred2, bool pred3, bool pred4, bool pred5, bool pred6,
     bool cord, int NHMF1, int NHMF2, int NLMF1, int NLMF2, char* cof_path, double bmdl[3][7]) {


     double jmdl[3][4];



    call_cm4(&UT, &thet , &phi, &alt, &dst, &f107,
                                      &pred1, &pred2, &pred3,&pred4, &pred5, &pred6
                                      ,&cord,
                                      &NHMF1,&NHMF2, &NLMF1,&NLMF2, cof_path, (double*)bmdl, (double*)jmdl);


}*/

void call_cm4_arrays(double* UT, double* thet, double* phi, double* alt,
     double* dst, double* f107, bool pred1, bool pred2, bool pred3, bool pred4, bool pred5, bool pred6,
     bool cord, int NHMF1, int NHMF2, int NLMF1, int NLMF2, int N, char* cof_path, double bmdl[3][7]) {


     double jmdl[3][4];



    call_cm4_arr(UT, thet , phi, alt, dst, f107,
                                      &pred1, &pred2, &pred3,&pred4, &pred5, &pred6
                                      ,&cord,
                                      &NHMF1,&NHMF2, &NLMF1,&NLMF2, &N, cof_path, (double*)bmdl, (double*)jmdl);


}
