
#include "expl_dll.h"

#include <iostream>
using std::endl;
using std::cout;

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
expl_dll::expl_dll() throw()
{}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
expl_dll::expl_dll(char* dll_fqpath) throw()
:dll_fqpath(dll_fqpath),hLibrary(0),func_ptr(0)
{}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
expl_dll::~expl_dll() throw()
{
#ifndef WIN32
    if(hLibrary) dlclose(hLibrary);     // Unload DLL from memory
#else
	FreeLibrary(hLibrary);
#endif
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
void expl_dll::init() throw(loadlib_failed, gpa_failed)
{    

#ifndef WIN32	
    hLibrary = dlopen(dll_fqpath, RTLD_NOW|RTLD_GLOBAL); // Load the DLL now
    char* error;
    error=dlerror();
    if (hLibrary != '\0'){
        func_ptr = (lpFunc) dlsym(hLibrary, "ckwyp_");
        func_ptr2 = (lpFunc2) dlsym(hLibrary, "update_ss_data__");
        if (func_ptr == '\0' || func_ptr2 =='\0') throw gpa_failed();
    }
    else throw loadlib_failed();
#else
	hLibrary = LoadLibrary(dll_fqpath);
	if (hLibrary != '\0'){
		func_ptr = (lpFunc) GetProcAddress(hLibrary, "CKWYP");
		func_ptr2 = (lpFunc2) GetProcAddress(hLibrary, "UPDATE_SS_DATA");
	if (func_ptr == '\0' || func_ptr2 =='\0') throw gpa_failed();
	}
	else throw loadlib_failed();
#endif

}

