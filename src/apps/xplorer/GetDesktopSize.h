#include <vpr/System.h>

#if defined( VPR_OS_Darwin )
#include "CocoaScreenResolution.h"
#endif

#include <utility>

#ifdef VPR_OS_Linux
#include <X11/Xlib.h>

int getRootWindowSize(int *w, int *h)
{
    Display* pdsp = NULL;
    Window wid = 0;
    XWindowAttributes xwAttr;
    
    pdsp = XOpenDisplay( NULL );
    if ( !pdsp ) {
        std::cout << "Failed to open default display." << std::endl;
        return -1;
    }
    
    wid = DefaultRootWindow( pdsp );
    //Window is an unsigned int evidently...so this is always false.
    /*if ( 0 > wid ) {
        std::cout << "Failed to obtain the root windows Id " <<
            "of the default screen of given display." << std::endl;
        return -2;
    }*/
    
    Status ret = XGetWindowAttributes( pdsp, wid, &xwAttr );
    *w = xwAttr.width;
    *h = xwAttr.height;
    
    XCloseDisplay( pdsp );
    return 0;
}

// This code is redundant. Only getRootWindowSize() is actually being used. 
/*
int getScreenSize(int *w, int*h)
{
    
    Display* pdsp = NULL;
    Screen* pscr = NULL;
    
    pdsp = XOpenDisplay( NULL );
    if ( !pdsp ) {
        std::cout << "Failed to open default display." << std::endl;
        return -1;
    }
    
    pscr = DefaultScreenOfDisplay( pdsp );
    if ( !pscr ) {
        std::cout << "Failed to obtain the default screen of given display." << std::endl;
        return -2;
    }
    
    *w = pscr->width;
    *h = pscr->height;
    
    XCloseDisplay( pdsp );
    return 0;
}
*/
#endif

std::pair< int, int > GetDesktopSize()
{
    std::pair< int, int > desktopSize;
    std::cout << "|\tEnabling Desktop Mode" << std::endl;
#ifdef VPR_OS_Win32
    //int dwWidth = GetSystemMetrics(SM_CXBORDER);
    int desktopWidth = GetSystemMetrics( SM_CXSCREEN );
    //int dwHeight = GetSystemMetrics(SM_CYBORDER);
    int desktopHeight = GetSystemMetrics( SM_CYSCREEN );
    desktopSize = std::pair< int, int >( desktopWidth, desktopHeight );
#elif defined( VPR_OS_Linux )
    int w = 0, h = 0;
    //getScreenSize(&w, &h);
    getRootWindowSize(&w, &h);
    desktopSize = std::make_pair( w, h );
#elif defined( VPR_OS_Darwin )
    desktopSize = getScreenResolution();
#endif
    return desktopSize;
}
