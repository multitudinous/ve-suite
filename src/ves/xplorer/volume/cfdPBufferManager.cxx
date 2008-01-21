/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef _OSG
#ifdef _PBUFFER

#include <cmath>
#include <ves/xplorer/volume/cfdPBufferManager.h>
#ifdef WIN32
// WGL_ARB_pbuffer
static PFNWGLCREATEPBUFFERARBPROC  wglCreatePbufferARB;
static PFNWGLGETPBUFFERDCARBPROC  wglGetPbufferDCARB;
static PFNWGLRELEASEPBUFFERDCARBPROC wglReleasePbufferDCARB;
static PFNWGLDESTROYPBUFFERARBPROC wglDestroyPbufferARB;
static PFNWGLQUERYPBUFFERARBPROC wglQueryPbufferARB;

// WGL_ARB_pixel_format
static PFNWGLGETPIXELFORMATATTRIBIVARBPROC   wglGetPixelFormatAttribivARB;
static PFNWGLGETPIXELFORMATATTRIBFVARBPROC   wglGetPixelFormatAttribfvARB;
static PFNWGLCHOOSEPIXELFORMATARBPROC        wglChoosePixelFormatARB;
#else
#endif
using namespace ves::xplorer::volume;
////////////////////////////////
//Constructors                //
////////////////////////////////
cfdPBufferManager::cfdPBufferManager()
{
#ifdef WIN32
    _frameBufferDeviceContext = 0;
    _frameBDGLContext = 0;
    _pBufferDeviceContext = 0;
#else
    _oldDisplay = 0;
    _oldDrawable = 0;
    _oldContext = 0;
    _pfAttribList.push_back( GLX_USE_GL );
    _pfAttribList.push_back( GLX_DRAWABLE_TYPE );
    _pfAttribList.push_back( GLX_PBUFFER_BIT );
    _pfAttribList.push_back( GLX_RENDER_TYPE );
    _pfAttribList.push_back( GLX_RGBA_BIT );
    _pfAttribList.push_back( GLX_DEPTH_SIZE );
    _pfAttribList.push_back( 16 );
    _pfAttribList.push_back( GLX_ALPHA_SIZE );
    _pfAttribList.push_back( 8 );
    _pfAttribList.push_back( GLX_RED_SIZE );
    _pfAttribList.push_back( 8 );
    _pfAttribList.push_back( GLX_GREEN_SIZE );
    _pfAttribList.push_back( 8 );
    _pfAttribList.push_back( GLX_BLUE_SIZE );
    _pfAttribList.push_back( 8 );
    _pfAttribList.push_back( None );
#endif
    _pBufferGLContext = 0;
    _hBuffer = 0;
    _isSupported = false;
    _isCreated = false;
    _h = 0;
    _w = 0;

}
///////////////////////////////////////
cfdPBufferManager::~cfdPBufferManager()
{
    cleanUpPBuffer();
}
//////////////////////////////////////////////////////
//initialize the entry functions                    //
//////////////////////////////////////////////////////
void cfdPBufferManager::initializePBufferEntryFunctions()
{
#ifdef WIN32
    //our pbuffer initialization check
    if( !wglCreatePbufferARB )
    {
        wglCreatePbufferARB = ( PFNWGLCREATEPBUFFERARBPROC )wglGetProcAddress( "wglCreatePbufferARB" );
    }
    if( !wglCreatePbufferARB )
    {
        std::cout << "PBuffers not supported!!" << std::endl;
        _isSupported = false;
        return ;

    }
    if( !wglGetPbufferDCARB )
    {
        wglGetPbufferDCARB = ( PFNWGLGETPBUFFERDCARBPROC )wglGetProcAddress( "wglGetPbufferDCARB" );
    }
    if( !wglGetPbufferDCARB )
    {
        std::cout << "PBuffers not supported!!" << std::endl;
        _isSupported = false;
        return ;
    }

    if( !wglReleasePbufferDCARB )
    {
        wglReleasePbufferDCARB = ( PFNWGLRELEASEPBUFFERDCARBPROC )wglGetProcAddress( "wglReleasePbufferDCARB" );
    }
    if( !wglReleasePbufferDCARB )
    {
        std::cout << "PBuffers not supported!!" << std::endl;
        _isSupported = false;
        return ;
    }
    if( !wglDestroyPbufferARB )
    {
        wglDestroyPbufferARB = ( PFNWGLDESTROYPBUFFERARBPROC )wglGetProcAddress( "wglDestroyPbufferARB" );
    }
    if( !wglDestroyPbufferARB )
    {
        std::cout << "PBuffers not supported!!" << std::endl;
        _isSupported = false;
        return ;
    }

    if( !wglQueryPbufferARB )
    {
        wglQueryPbufferARB = ( PFNWGLQUERYPBUFFERARBPROC )wglGetProcAddress( "wglQueryPbufferARB" );
    }
    if( !wglQueryPbufferARB )
    {
        std::cout << "PBuffers not supported!!" << std::endl;
        _isSupported = false;
        return ;
    }
    if( !wglGetPixelFormatAttribivARB )
    {
        wglGetPixelFormatAttribivARB = ( PFNWGLGETPIXELFORMATATTRIBIVARBPROC )wglGetProcAddress( "wglGetPixelFormatAttribivARB" );
    }
    if( !wglGetPixelFormatAttribivARB )
    {
        std::cout << "PBuffers not supported!!" << std::endl;
        _isSupported = false;
        return ;
    }
    if( !wglGetPixelFormatAttribfvARB )
    {
        wglGetPixelFormatAttribfvARB = ( PFNWGLGETPIXELFORMATATTRIBFVARBPROC )wglGetProcAddress( "wglGetPixelFormatAttribfvARB" );
    }
    if( !wglGetPixelFormatAttribfvARB )
    {
        std::cout << "PBuffers not supported!!" << std::endl;
        _isSupported = false;
        return ;
    }
    if( !wglChoosePixelFormatARB )
    {
        wglChoosePixelFormatARB = ( PFNWGLCHOOSEPIXELFORMATARBPROC )wglGetProcAddress( "wglChoosePixelFormatARB" );
    }
    if( !wglChoosePixelFormatARB )
    {
        std::cout << "PBuffers not supported!!" << std::endl;
        _isSupported = false;
        return ;
    }
#endif
    _isSupported = true;
}
/////////////////////////////////////
//clean up the pbuffer             //
/////////////////////////////////////
void cfdPBufferManager::cleanUpPBuffer()
{
#ifdef WIN32
    if( _pBufferGLContext )
        wglDeleteContext( _pBufferGLContext );
    if( _pBufferDeviceContext )
        wglReleasePbufferDCARB( _hBuffer, _pBufferDeviceContext );
    if( _hBuffer )
        wglDestroyPbufferARB( _hBuffer );
#else
    if( _pBufferGLContext )
        glXDestroyContext( _oldDisplay, _pBufferGLContext );

    if( _hBuffer )
        glXDestroyPbuffer( _oldDisplay, _hBuffer );
#endif
}
////////////////////////////////////////////////////////////
//initialize a pixel buffer                               //
//this functionality will be                              //
//expanded so that you can specialize                     //
//the buffer to you needs but for now                     //
//it is set up w/ default params                          //
////////////////////////////////////////////////////////////
bool cfdPBufferManager::initializePBuffer( int width, int height )
{
    _w = width;
    _h = height;
    // Get ready to query for a suitable pixel format that meets our
    // minimum requirements.
    //get the current frame buffer context
#ifdef WIN32
    _frameBufferDeviceContext = wglGetCurrentDC();
    _frameBDGLContext = wglGetCurrentContext();

    int attribList[] =
        {
            WGL_RED_BITS_ARB,              8,
            WGL_GREEN_BITS_ARB,             8,
            WGL_BLUE_BITS_ARB,              8,
            WGL_ALPHA_BITS_ARB,             8,
            WGL_DOUBLE_BUFFER_ARB,         false,
            WGL_TRANSPARENT_ARB,           true,
            WGL_DEPTH_BITS_ARB,             16,
            WGL_SUPPORT_OPENGL_ARB,        true,
            WGL_DRAW_TO_PBUFFER_ARB,        true,
            WGL_PIXEL_TYPE_ARB,
            WGL_TYPE_RGBA_ARB,
            0
        };
    // Now obtain a list of pixel formats that meet these minimum
    // requirements.
#define MAX_PFORMATS 10
    int format = 0;
    int pformat[MAX_PFORMATS];
    unsigned int nformats = 0;
    if( !wglChoosePixelFormatARB( _frameBufferDeviceContext,
                                   attribList,
                                   NULL,
                                   MAX_PFORMATS,
                                   pformat,
                                   &nformats ) )
    {
        fprintf( stderr, "pbuffer creation error:  Couldn't find a \
                 suitable pixel format.\n" );
        return( 0 );
    }
    format = pformat[0];

    // clear attribute list
    attribList[0] = 0;

    int pb_attr[] = {0};
    //create the buffer
    _hBuffer = wglCreatePbufferARB( _frameBufferDeviceContext,
                                    format, _w,
                                    _h, pb_attr );

    if( !_hBuffer )
    {
        printf( "Unable to create floating point pbuffer (wglCreatePbufferARB failed)\n" );
        return 0;
    }

    //pbuffer device contex
    _pBufferDeviceContext = wglGetPbufferDCARB( _hBuffer );
    if( _pBufferDeviceContext == NULL )
    {
        printf( "Unable to retrieve handle to pbuffer device context\n" );
        return 0;
    }
    //pbuffer gl context
    _pBufferGLContext = wglCreateContext( _pBufferDeviceContext );
    if( _pBufferGLContext == NULL )
    {
        printf( "Unable to create a rendering context for the pbuffer\n" );
        return 0;
    }
    // With sharing turned on, we can simple create our dynamic texture by
    // binding it to the p-buffer and calling glCopyTexSubImage2D, but this
    // will only work if the window's rendering context is sharing its display
    // lists and texture space with the p-buffer's context.
    //
    if( !wglShareLists( _frameBDGLContext, _pBufferGLContext ) )
    {
        MessageBox( NULL, "Call to wglShareLists() failed for p-buffer!",
                    "ERROR", MB_OK | MB_ICONEXCLAMATION );
        exit( -1 );
    }
#else
    _oldDisplay = glXGetCurrentDisplay();
    if( !_oldDisplay )
    {
        _isCreated = false;
        return false;
    }
    _oldContext = glXGetCurrentContext();
    int screen = DefaultScreen( _oldDisplay );

    GLXFBConfig* glxConfig = 0;
    int configCount;

    glxConfig = glXGetFBConfigs( _oldDisplay, screen, &configCount );
    int attrib[] = {
                       GLX_RENDER_TYPE, GLX_RGBA_BIT,
                       GLX_DRAWABLE_TYPE, GLX_PBUFFER_BIT,
                       GLX_DOUBLEBUFFER, 0,
                       GLX_RED_SIZE, 8,
                       GLX_GREEN_SIZE, 8,
                       GLX_BLUE_SIZE, 8,
                       GLX_ALPHA_SIZE, 8,
                       GLX_STENCIL_SIZE, 0,
                       GLX_DEPTH_SIZE, 0,
                       0 };
    if( !glxConfig )
    {
        std::cout << "pbuffer creation error:  glXGetFBConfigs() failed" << std::endl;
        _isCreated = false;
        return false;
    }
    else
    {
        std::cout << "configCount: " << configCount << std::endl;
        glxConfig = glXChooseFBConfig( _oldDisplay, screen, attrib, &configCount );
        if( !glxConfig )
        {
            std::cout << "pbuffer creation error:  glXChooseFBConfig() failed" << std::endl;
            _isCreated = false;
            return false;
        }
    }
    XVisualInfo* visual = glXGetVisualFromFBConfig( _oldDisplay, glxConfig[0] );
    if( !visual )
    {
        std::cout << "pbuffer creation error:  glXGetVisualFromFBConfig() failed" << std::endl;
        _isCreated = false;
        return false;
    }

    int attr[] = {
                     GLX_PBUFFER_WIDTH, _w,
                     GLX_PBUFFER_HEIGHT, _h,
                     GLX_LARGEST_PBUFFER, True,
                     GLX_PRESERVED_CONTENTS, True,
                     None
                 };

    _hBuffer = glXCreatePbuffer( _oldDisplay,
                                 glxConfig[0],
                                 attr );

    if( _hBuffer == None )
    {
        std::cout << "pbuffer creation error:  glXCreatePbuffer() failed" << std::endl;
        _isCreated = false;
        return false;
    }

    _pBufferGLContext = glXCreateContext( _oldDisplay,
                                          visual,
                                          _oldContext, true );
    if( !_pBufferGLContext )
    {
        std::cout << "pbuffer creation error:  glXCreateNewContext() failed" << std::endl;
        _isCreated = false;
        return false;
    }
#endif
    _isCreated = true;
    return true;
}
/////////////////////////////////////
void cfdPBufferManager::setViewport( double w, double h )
{
    glViewport( 0.0, 0.0, w, h );
    _w = w;
    _h = h;
}
/////////////////////////////////
//check if pbuffer extensions  //
//are supported                //
/////////////////////////////////
bool cfdPBufferManager::isSupported()
{
    initializePBufferEntryFunctions();
    return _isSupported;
}
#ifndef WIN32
//////////////////////////////////
void cfdPBufferManager::activate()
{
    if( _isCreated )
    {
        _oldContext = glXGetCurrentContext();
        _oldDrawable = glXGetCurrentDrawable();

        if( False == glXMakeCurrent( _oldDisplay, _hBuffer, _pBufferGLContext ) )
        {
            return;
        }
    }
}
////////////////////////////////////
void cfdPBufferManager::deactivate()
{
    if( _isCreated )
    {
        if( False == glXMakeCurrent( _oldDisplay, _oldDrawable, _oldContext ) )
        {
            return ;
        }
    }
}
#endif
#endif
#endif

