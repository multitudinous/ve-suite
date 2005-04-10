#ifndef _BIV_PBUFFER_MANAGER_H_
#define _BIV_PBUFFER_MANAGER_H_
#ifdef VE_PATENTED
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

   //code from Steve Marshall of SPI for setting up
   //proper view matricies for rendering textured pbuffer
   //slices
   enum ProjectionType { Orthographic, Perspective };
   void setGeometricParams(float* center,float radius);
   void initSlice(unsigned int slice);
   void beginSlicing(unsigned int nSlices);
   void endSlicing();
   void applyViewMatrix(bool force = false);
   void applyProjectionMatrix(bool force = false);

   void setProjectionParams(float l, float r, float b,
                                 float t, float n, float f);

   void setProjectionType(ProjectionType type)
   { 
      if(_type != type){
         _projectionMatrixDirty = true;
      }
       _type = type;
   }

   void setLookAt(const float* lookAt);
   void setLookFrom(const float* lookFrom);
   void setLookUp(const float* up);
   void setCenterOfInterest(float coi);
   ////////////////////////////////////
   
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
   
   //Marshall utilities
   void _cross(const float* v0, const float* v1, float* out);
   void _ensureViewMatrix(bool force);

   //Marshall variables
   float _viewMatrix[16];
   float _projectionMatrix[16];
   float _worldToOpacityMapsMatrix[16];

   float _lookFrom[3];
   float _lookAt[3];
   float _up[3];
   float _lastRight[3];

   float _centerOfInterest;

   float _left;
   float _right;
   float _bottom;
   float _top;
   float _near;
   float _far;
   bool _viewMatrixDirty;
   bool _projectionMatrixDirty;
   float* _slices;
   int _numSlices;
   ProjectionType _type;
   float _geomCenter[3];
   float _geomRadius;
   float _offsetMin;
   float _offsetMax;

   float _paramCache[6];

};
#endif //CFD_USE_SHADER
#endif
#endif
#endif //_BIV_PBUFFER_MANAGER_H_
