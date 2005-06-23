#ifndef _BIV_PBUFFER_MANAGER_H_
#define _BIV_PBUFFER_MANAGER_H_
#ifdef VE_PATENTED
#ifdef _OSG

#include <osg/GL>

#if defined(WIN32)
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
#endif
///////////////////////
#ifdef WIN32
#include "gl/wglext.h"
#include "gl/glext.h"
#else
#include <GL/glx.h>
#include <vector>
#endif
#include <iostream>
#include "VE_Xplorer/cfdConfig.h"

class WXPLUGIN_DECLSPEC cfdPBufferManager
{
   public:
      cfdPBufferManager();
      ~cfdPBufferManager();
   
      //initialize the entry functions
      void initializePBufferEntryFunctions();
   
      //clean up the pbuffer
      void cleanUpPBuffer();
   
      void setViewport(double w, double h);

#ifdef WIN32
      BOOL activate(){
         return wglMakeCurrent(_pBufferDeviceContext, _pBufferGLContext);
      }
    
      BOOL deactivate(){
         return wglMakeCurrent(_frameBufferDeviceContext, _frameBDGLContext);
      }

      //get the device context
      HDC pBufferDeviceContex(){return _pBufferDeviceContext;}

      //get the gl rendering pbuffer contex
      HGLRC pBufferGLDC(){return _pBufferGLContext;}
#else
    void activate();
    void deactivate();
#endif
   //initialize a pixel buffer
   //this functionality will be
   //expanded so that you can specialize
   //the buffer to you needs but for now
   //it is set up w/ default params
   bool initializePBuffer(int width, int height);

   int height(){return _h;}
   int width(){return _w;}

   //check if pbuffer extensions
   //are supported
   bool isSupported();

   bool isCreated(){return _isCreated;}

   
protected:
#ifdef WIN32
   HDC _frameBufferDeviceContext;
   HGLRC _frameBDGLContext;
   HPBUFFERARB _hBuffer;

   HDC _pBufferDeviceContext;
   HGLRC _pBufferGLContext;
#else
   GLXContext  _pBufferGLContext;
   GLXPbuffer  _hBuffer;

   Display*    _oldDisplay;
   GLXPbuffer  _oldDrawable;
   GLXContext  _oldContext;

   std::vector<int> _pfAttribList;
#endif
   bool _isSupported;
   bool _isCreated;

   int _h;
   int _w;
};

#endif
#endif
#endif //_BIV_PBUFFER_MANAGER_H_
