#include "GlobalConst.h"
#ifndef _expl_dll_h
#define _expl_dll_h

#include <stdio.h>
#ifndef WIN32
#include <dlfcn.h>
#else
#include <windows.h>
#endif
class loadlib_failed {};
class gpa_failed {};

class expl_dll {

    typedef void ( * lpFunc)(REAL* p, REAL* t, REAL* y, int* ickwrk, 
                             REAL* rckwrk, REAL* wdot);

    typedef void ( * lpFunc2)(int*, char*, REAL* p, int len);


public:
    expl_dll() throw();
    expl_dll(char* dll_fqpath) throw();
    ~expl_dll() throw();
    void init() throw(loadlib_failed,gpa_failed);
    lpFunc get_fptr() throw() {return func_ptr;};
    lpFunc2 get_fptr2() throw() {return func_ptr2;};

private:
    char* dll_fqpath;
#ifndef WIN32
    void*  hLibrary;
#else
	HINSTANCE hLibrary;
#endif
    lpFunc func_ptr;
    lpFunc2 func_ptr2;
};

#endif
