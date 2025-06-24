#include <Python.h>
#include "ccm4.h"

// C wrapper for the Fortran function
// This function is called when the Python function is invoked.
// It extracts arguments from Python, calls the Fortran function, and returns the result back to Python.

PyObject* save_3d_array(int rows, int cols, int arr_len, double*** matrix){
    // Create a new Python list
    PyObject* py_list = PyList_New(rows);
    if (!py_list) {
        return NULL; // Memory allocation failed
    }

    // Fill the list with numpy arrays
    for (int i = 0; i < rows; i++) {
        // Create a new numpy array for each row
        PyObject* py_row = PyList_New(cols);
        if (!py_row) {
            Py_DECREF(py_list); // Clean up previously allocated memory
            return NULL; // Memory allocation failed
        }

        for (int j = 0; j < cols; j++) {
            PyObject* py_col = PyList_New(arr_len);
            if (!py_col) {
                Py_DECREF(py_row);
                Py_DECREF(py_list);
                return NULL; // Memory allocation failed
            }

            for (int k = 0; k < arr_len; k++) {
                PyObject* py_value = PyFloat_FromDouble(matrix[i][j][k]);
                if (!py_value) {
                    Py_DECREF(py_col);
                    Py_DECREF(py_row);
                    Py_DECREF(py_list);
                    return NULL; // Memory allocation failed
                }

                PyList_SetItem(py_col, k, py_value); // Steal reference
            }

            PyList_SetItem(py_row, j, py_col); // Steal reference
        }
        PyList_SetItem(py_list, i, py_row); // Steal reference
    }

    return py_list;
}

double* pyooject_to_darray(PyObject* obj){

    if (!PyList_Check(obj)) {
        PyErr_SetString(PyExc_TypeError, "Expected a list");
        return NULL;
    }

    Py_ssize_t len = PySequence_Length(obj);

    double* array = (double*)malloc(len * sizeof(double));

    for(Py_ssize_t i = 0; i < len; i++) {
        PyObject* item = PySequence_GetItem(obj, i);
        if (!PyFloat_Check(item)){
            Py_DECREF(item);
            free(array);
            PyErr_SetString(PyExc_TypeError, "Expected a float in the list");
            return NULL;
        }
        array[i] = PyFloat_AsDouble(item);
        Py_DECREF(item);
    }

    return array;

}
static PyObject* py_call_cm4_arr(PyObject* self, PyObject* args) {

    PyObject *ut_obj, *thet_obj, *phi_obj, *alt_obj, *dst_obj, *f107_obj;
    int pred1, pred2, pred3, pred4, pred5, pred6;
    int cord;
    char* cof_path;

    int nhmf1 = 13, nhmf2 = 45, nlmf1 = 1, nlmf2 = 14;



    // Parse the arguments from Python
    if (!PyArg_ParseTuple(args, "OOOOOOiiiiiiiiic",
          &ut_obj, &thet_obj, &phi_obj, &alt_obj, &dst_obj, &f107_obj,
          &pred1, &pred2, &pred3, &pred4, &pred5, &pred6,
          &cord, &nhmf1, &nhmf2, &nlmf1, &nlmf2, &cof_path)) {
        return NULL;
    }

    // Convert Python lists to C arrays
    double *ut = pyooject_to_darray(ut_obj);
    double *thet = pyooject_to_darray(thet_obj);
    double *phi = pyooject_to_darray(phi_obj);
    double *alt = pyooject_to_darray(alt_obj);
    double *dst = pyooject_to_darray(dst_obj);
    double *f107 = pyooject_to_darray(f107_obj);

    Py_ssize_t len = PySequence_Length(ut_obj);

    if (len != PySequence_Length(thet_obj) || len != PySequence_Length(phi_obj) ||
        len != PySequence_Length(alt_obj) || len != PySequence_Length(dst_obj) ||
        len != PySequence_Length(f107_obj)) {
        free(ut);
        free(thet);
        free(phi);
        free(alt);
        free(dst);
        free(f107);
        PyErr_SetString(PyExc_ValueError, "All input lists must have the same length");
        return NULL;
    }
    double bmdl[3][7][len]; // Assuming bmdl is a 3x7 array
    double jmdl[3][4]; // Assuming jmdl is a 3x4 array


    call_cm4_arr(ut, thet , phi, alt, dst, f107,
                                      &pred1, &pred2, &pred3,&pred4, &pred5, &pred6
                                      ,&cord,
                                      &nhmf1, &nhmf2, &nlmf1, &nlmf2, &len, cof_path, (double*)bmdl, (double*)jmdl);

    PyObject* results = save_3d_array(3, 7, len, bmdl);

    return results;
}
/*static PyObject* py_call_cm4(PyObject* self, PyObject* args) {

    double ut, thet, phi, alt, dst, f107;
    int pred1, pred2, pred3, pred4, pred5, pred6;
    int cord, nhmf1, nhmf2, nlmf1, nlmf2;
    char* cof_path;
    double bmdl[3][7]; // Assuming bmdl is a 3x7 array
    double jmdl[3][7];


    // Parse the arguments from Python
    if (!PyArg_ParseTuple(args, "ddddddiiiiiiiiiiic", &ut, &thet, &phi, &alt, &dst, &f107,
          &pred1, &pred2, &pred3, &pred4, &pred5, &pred6,
          &cord, &nhmf1, &nhmf2, &nlmf1, &nlmf2, &cof_path)) {
        return NULL;
    }

    // Call the Fortran function
    call_cm4(&ut, &thet, &phi, &alt, &dst, &f107,
          &pred1, &pred2, &pred3, &pred4, &pred5, &pred6,
          &cord, &nhmf1, &nhmf2, &nlmf1, &nlmf2, &cof_path, (double*)bmdl, (double*)jmdl);

    // Return the result to Python
    //return PyLong_FromLong(result);
}*/

// Method table that maps Python methods to C functions
// This array defines all the functions that will be exposed from C to Python.
// Each entry in this array contains the name of the Python function ("py_mat_cm4_arr"), a pointer to the corresponding C function (py_call_cm4)
static PyMethodDef methods[] = {
    {"py_mat_cm4_arr", py_call_cm4_arr, METH_VARARGS, "Get the magnetic elements from CM4"},
    {NULL, NULL, 0, NULL}
};

// Module definition
static struct PyModuleDef c_cm4 = {
    PyModuleDef_HEAD_INIT,
    "python_CM4",   // Module name
    "Get the magnetic elements from CM4 in core, crustal, ionosphere or magnetoshpere field.",  // Docstring
    -1,  // Size of the module state (-1 means module is global)
    methods  // Method table
};

// Initialization function
//This is the initialization function that Python will call when loading the extension module.

// PyMODINIT_FUNC PyInit_c_cm4(void): This is the function signature for the initialization function.
// The function uses PyModule_Create() to create and initialize the module,
// linking the methods[] array to define the methods available in the module (like py_mat_cm4_arr).
PyMODINIT_FUNC PyInit_c_cm4(void) {
    return PyModule_Create(&c_cm4);
}
