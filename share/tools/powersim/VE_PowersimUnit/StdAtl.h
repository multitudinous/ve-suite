
#ifndef STD_ATL_H
#define STD_ATL_H

//Exclude rarely-used stuff from Windows headers
#define VC_EXTRALEAN

#include <atlbase.h>

extern CComModule _Module;

#include <atlcom.h>
#include <atlwin.h>

#include <atlstr.h>

#endif //STD_ATL_H
