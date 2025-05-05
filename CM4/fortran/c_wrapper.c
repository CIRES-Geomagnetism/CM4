#include <Python.h>

// C wrapper for the Fortran function
// This function is called when the Python function is invoked.
// It extracts arguments from Python, calls the Fortran function, and returns the result back to Python.

static PyObject* py_call_cm4(PyObject* self, PyObject* args) {

    double ut, thet, phi, alt, dst, f107;
    int pred1, pred2, pred3, pred4, pred5, pred6;
    int cord, nhmf1, nhmf2, nlmf1, nlmf2;
    char* cof_path;
    double bmdl[3][7]; // Assuming bmdl is a 3x7 array


    // Parse the arguments from Python
    if (!PyArg_ParseTuple(args, "ddddddiiiiiiiiiiic", &ut, &thet, &phi, &alt, &dst, &f107,
          &pred1, &pred2, &pred3, &pred4, &pred5, &pred6,
          &cord, &nhmf1, &nhmf2, &nlmf1, &nlmf2, &cof_path)) {
        return NULL;
    }

    // Call the Fortran function
    call_cm4_point(&ut, &thet, &phi, &alt, &dst, &f107,
          &pred1, &pred2, &pred3, &pred4, &pred5, &pred6,
          &cord, &nhmf1, &nhmf2, &nlmf1, &nlmf2, &cof_path, (double*)bmdl);

    // Return the result to Python
    //return PyLong_FromLong(result);
}

// Method table that maps Python methods to C functions
// This array defines all the functions that will be exposed from C to Python.
// Each entry in this array contains the name of the Python function (compute"), a pointer to the corresponding C function (py_call_cm4)
static PyMethodDef methods[] = {
    {"compute", py_call_cm4, METH_VARARGS, "Get the magnetic elements from CM4"},
    {NULL, NULL, 0, NULL}
};

// Module definition
static struct PyModuleDef c_cm4 = {
    PyModuleDef_HEAD_INIT,
    "python_cm4",   // Module name
    "Get the magnetic elements from CM4 in core, crustal, ionosphere or magnetoshpere field.",  // Docstring
    -1,  // Size of the module state (-1 means module is global)
    methods  // Method table
};

// Initialization function
//This is the initialization function that Python will call when loading the extension module.

// PyMODINIT_FUNC PyInit_c_cm4(void): This is the function signature for the initialization function.
// The function uses PyModule_Create() to create and initialize the module,
// linking the methods[] array to define the methods available in the module (like compute).
PyMODINIT_FUNC PyInit_c_cm4(void) {
    return PyModule_Create(&c_cm4);
}
