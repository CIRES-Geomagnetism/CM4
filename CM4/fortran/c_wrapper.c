#include <Python.h>
#include <stdbool.h>

extern void call_cm4(double UT,double thet, double phi, double alt,
     double dst, double f107, int pred1, int pred2, int pred3, int pred4, int pred5, int pred6,
     int CORD, int NHMF1, int NHMF2, int NLMF1, int NLMF2, char* cof_path);

extern double add_numbers(double a, double b);

// C wrapper for the Fortran function
// This function is called when the Python function is invoked.
// It extracts arguments from Python, calls the Fortran function, and returns the result back to Python.
static PyObject* py_call_cm4(PyObject* self, PyObject* args) {

    double ut, thet, phi, alt, dst, f107;
    int pred1, pred2, pred3, pred4, pred5, pred6;
    int cord, nhmf1, nhmf2, nlmf1, nlmf2;
    char* cof_path;


    // Parse the arguments from Python
    if (!PyArg_ParseTuple(args, "ddddddiiiiiiiiiiis", &ut, &thet, &phi, &alt, &dst, &f107,
          &pred1, &pred2, &pred3, &pred4, &pred5, &pred6,
          &cord, &nhmf1, &nhmf2, &nlmf1, &nlmf2, &cof_path)) {
        return NULL;
    }




    char cof_buffer[256];

    int copy_size = sizeof(cof_buffer);

    if (copy_size > strlen(cof_path)) {
        copy_size = strlen(cof_path);
    }

    strncpy(cof_buffer, cof_path, copy_size - 1);
    cof_buffer[sizeof(cof_buffer) - 1] = '\0';

    printf("cof path: %s", cof_buffer);

    // Call the Fortran function
    call_cm4(ut, thet, phi, alt, dst, f107,
          pred1, pred2, pred3, pred4, pred5, pred6,
          cord, nhmf1, nhmf2, nlmf1, nlmf2, cof_buffer);

    // Return the result to Python
    //return PyLong_FromLong(result);
    Py_RETURN_NONE;
}

// Method table that maps Python methods to C functions
// This array defines all the functions that will be exposed from C to Python.
// Each entry in this array contains the name of the Python function (compute"), a pointer to the corresponding C function (py_call_cm4)
static PyMethodDef methods[] = {
    {"call_cm4", py_call_cm4, METH_VARARGS, "Get the magnetic elements from CM4"},
    {NULL, NULL, 0, NULL}
};

// Module definition
static struct PyModuleDef c_cm4_module = {
    PyModuleDef_HEAD_INIT,
    "cm4field",   // Module name
    "Get the magnetic elements from CM4 in core, crustal, ionosphere or magnetoshpere field.",  // Docstring
    -1,  // Size of the module state (-1 means module is global)
    methods  // Method table
};

// Initialization function
//This is the initialization function that Python will call when loading the extension module.

// PyMODINIT_FUNC PyInit_c_cm4(void): This is the function signature for the initialization function.
// The function uses PyModule_Create() to create and initialize the module,
// linking the methods[] array to define the methods available in the module (like compute).
PyMODINIT_FUNC PyInit_cm4field(void) {
    return PyModule_Create(&c_cm4_module);
}