#ifndef OSG_DLL_H
#define OSG_DLL_H

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

extern "C" \
{ \
    void* CreateOSGGeode(); \

    typedef void* ( *CreateFn )();  \

}

#endif //OSG_DLL_H
