#include <stdio.h>
#include <stdbool.h>

extern void call_cm4(double* UT,double* thet, double* phi, double* alt,
     double* dst, double* f107, bool* pred1, bool* pred2, bool* pred3, bool* pred4, bool* pred5, bool* pred6,
     bool* CORD, int* NHMF1, int* NHMF2, int* NLMF1, int* NLMF2, char* cof_path, double* bmdl, double* jmdl);

void write_header(FILE* fpw){
    fprintf(fpw, "date latitude longitude altitude dst f107 Bx By Bz\n");
}

void write_outputs(double date, double lat, double lon, double alt, double dt, double f107, double bmdl[3][7], FILE* fptw, char key){
    double bx, by, bz;
    // Write the results to the output files
    if (key == 's'){
        bz = -bmdl[2][1];
        bx = -bmdl[0][1];
        by = bmdl[1][1];
    }else if (key == 'r'){
        bz = -bmdl[2][0];
        bx = -bmdl[0][0];
        by = bmdl[1][0];
    }else if (key == 'i'){
        bz = -bmdl[2][4] - bmdl[2][5];
        bx = -bmdl[0][4] - bmdl[0][5];
        by = bmdl[1][4] + bmdl[1][5];
    }else if (key == 'm'){
        bz = -bmdl[2][2] - bmdl[2][3];
        bx = -bmdl[0][2] - bmdl[0][3];
        by = bmdl[1][2] + bmdl[1][3];
    }

    fprintf(fptw, "%lf  %lf  %lf    %lf %lf %lf %lf %lf %lf\n",
            date, lat, lon, alt, dt, f107, bx, by, bz); 
}

int main(){

    char inputs_file[50] = "cm4_fortran_inputs.csv";
    char line[200];

    char crust_out_file[50] = "cm4_crust_TestValues.csv";
    char core_out_file[50] = "cm4_core_TestsValues.csv";
    char iono_out_file[50] = "cm4_iono_TEstValues.csv";
    char magneto_out_file[50] = "cm4_magneto_TestValues.csv";

    bool pred1 = true, pred2 = true, pred3  = true, pred4  = true, pred5  = true, pred6 = true;
    bool CORD = false;
    int NHMF1 = 13, NHMF2 = 45, NLMF1 = 1, NLMF2 = 14;
    char cof_path[50] = "/Users/lily/Projects/cm4/CM4/fortran/umdl.CM4";
    double bmdl[3][7], jmdl[3][4];
    
    // Open the file for writing
    FILE* fpw_s = fopen(crust_out_file, "w");
    FILE* fpw_r = fopen(core_out_file, "w");
    FILE* fpw_i = fopen(iono_out_file, "w");
    FILE* fpw_m = fopen(magneto_out_file, "w");
    // Open the file for reading


    FILE* fp =  fopen(inputs_file, "r");
    
    write_header(fpw_s);
    write_header(fpw_r);
    write_header(fpw_i);
    write_header(fpw_m);
        
     // Read the first line (header)
   fgets(line,sizeof(line), fp);


   while(fgets(line,sizeof(line), fp) != NULL){
        double lat, lon;
        double UT, thet, alt, dst, f107;

        
        sscanf(line,"%lf,%lf,%lf,%lf,%lf,%lf",
                 &UT,&lat,&lon,&alt,&dst,&f107);
    
        thet = lat;

        call_cm4(&UT,&thet,&lon,&alt,&dst,&f107,
                    &pred1,&pred2,&pred3,&pred4,&pred5,&pred6,
                    &CORD,
                    &NHMF1,&NHMF2,
                    &NLMF1,&NLMF2,
                    cof_path,
                    (double*) bmdl,
                    (double*) jmdl);
    
        // Write the results to the output files
        // 's': crust field; 'r': core field; 'i': ionosphere field; 'm': magnetosphere field

        printf("bmld[0][0]: %lf\n", bmdl[0][0] );
        printf("bmld[1][0]: %lf\n", bmdl[1][0] );
        printf("bmld[2][0]: %lf\n", bmdl[2][0] );

        write_outputs(UT, lat, lon, alt, dst, f107, bmdl, fpw_s, 's');  
        write_outputs(UT, lat, lon, alt, dst, f107, bmdl, fpw_r, 'r');  
        write_outputs(UT, lat, lon, alt, dst, f107, bmdl, fpw_i, 'i');
        write_outputs(UT, lat, lon, alt, dst, f107, bmdl, fpw_m, 'm');

        break;
    }

    // Close the files
    fclose(fp);
    fclose(fpw_s);
    fclose(fpw_r);
    fclose(fpw_i);
    fclose(fpw_m);
    return 0;
}
