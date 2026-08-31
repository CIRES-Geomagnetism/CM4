#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define RAD2DEG(rad)    ((rad)*(180.0L/M_PI))
#define DEG2RAD(deg)    ((deg)*(M_PI/180.0L))




typedef struct {
    double r; /* spherical radius */
    double r_alt; /*distance of location from the center of the earth - earth radius in km */
    double lambda; /* longitude */
    double phig; /* geocentric latitude */
} CoordSpherical;

typedef struct {
    double Bx;
    double By;
    double Bz;
} Results;

void print_results(double B[3][7][3000]);
int load_inputs(double* lats, double* lons, double* alts, double* uts, double* dsts, double* f107s, double* geocLat, double* radAlt, int N, const char* inputs_file);

void spherical_vector_to_geodetic(double Bt, double Bp, double Br, double geod_lat, double geoc_lat, Results* results);
void geod_to_geocentric(double geod_lat, double ellip_alt, CoordSpherical *coord);
void fortran_to_c_order(double* f_array, double* c_array, int row, int col, int depth);
void write_header(FILE* fpw);
void write_outputs(double* date, double* lat, double* lon, double* alt, double* dst, double* f107, double bmdl[3][7][3000], FILE* fptw, char* key, int N, double* geoc_lat, double* rad_alt);