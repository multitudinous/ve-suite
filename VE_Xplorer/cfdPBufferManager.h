#ifndef _BIV_PBUFFER_MANAGER_H_
#define _BIV_PBUFFER_MANAGER_H_
#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <osg/GL>

#if defined(WIN32)
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
#endif
///////////////////////

#include "gl/wglext.h"
#include "gl/glext.h"

#include <iostream>



class cfdPBufferManager
{
   public:
      cfdPBufferManager();
      ~cfdPBufferManager();
   
      //initialize the entry functions
      void initializePBufferEntryFunctions();
   
      //clean up the pbuffer
      void cleanUpPBuffer();
   
      void setViewport(double w, double h);

      void activate(){
         wglMakeCurrent(_pBufferDeviceContext, _pBufferGLContext);
      }
    
      void deactivate(){
         wglMakeCurrent(_frameBufferDeviceContext, _frameBDGLContext);
      }

      //get the device context
      HDC pBufferDeviceContex(){return _pBufferDeviceContext;}

      //get the gl rendering pbuffer contex
      HGLRC pBufferGLDC(){return _pBufferGLContext;}

   //initialize a pixel buffer
   //this functionality will be
   //expanded so that you can specialize
   //the buffer to you needs but for now
   //it is set up w/ default params
   int initializePBuffer(int width, int height);


   int height(){return _h;}
   int width(){return _w;}

   //check if pbuffer extensions
   //are supported
   int isSupported();

   int isCreated(){return _isCreated;}

   
protected:

   HDC _frameBufferDeviceContext;
   HGLRC _frameBDGLContext;

   HDC _pBufferDeviceContext;
   HGLRC _pBufferGLContext;

   HPBUFFERARB _hBuffer;

   int _isSupported;
   int _isCreated;

   int _h;
   int _w;

};
#endif //CFD_USE_SHADER
#endif
#endif //_BIV_PBUFFER_MANAGER_H_
