
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .C calls */
extern void FreePELT(void *);
extern void PELT(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"FreePELT", (DL_FUNC) &FreePELT,  1},
    {"PELT",     (DL_FUNC) &PELT,     11},
    {NULL, NULL, 0}
};

void R_init_changepoint_np(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}








