#ifdef VE_PATENTED
#ifdef _OSG 
#include <cmath>
#ifdef CFD_USE_SHADERS
#include "cfdPBufferManager.h"

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

////////////////////////////////
//Constructors                //
////////////////////////////////
cfdPBufferManager::cfdPBufferManager()
:_left(-.10f),
 _right(.10f),
 _bottom(-.10f),
 _top(.10f),
 _near(.10f),
 _far(100.0f),
 _viewMatrixDirty(true),
 _projectionMatrixDirty(true),
 _slices(0),
 _numSlices(0),
 _geomRadius(0.0f),
 _offsetMax(0.0f),
 _offsetMin(0.f),
 _type(cfdPBufferManager::Perspective)
{
   _frameBufferDeviceContext = 0;
   _hBuffer = 0;
   _pBufferDeviceContext = 0;
   _pBufferGLContext = 0;
   _isSupported = 0;
   _frameBDGLContext = 0;
   _isCreated = 0;
   _h = 0;
   _w = 0;
   _up[0] = 0.0f; 
   _up[1] = 1.0f; 
   _up[2] = 0.0f;

   _centerOfInterest = 20.0f;

   _lookFrom[0] = 0.0f;
   _lookFrom[1] = 0.0f;
   _lookFrom[2] = _centerOfInterest;

   _lookAt[0] = 0.0f;
   _lookAt[1] = 0.0f;
   _lookAt[2] = 0.0f;
   _geomCenter[0] = 0;
   _geomCenter[1] = 0;
   _geomCenter[2] = 0;

}
///////////////////////////////////////
cfdPBufferManager::~cfdPBufferManager()
{
   cleanUpPBuffer();
   if(_slices){
      delete [] _slices;
   }
}
///////////////////////////////////////////////////////////////////////
void cfdPBufferManager::setProjectionParams(float l, float r, float b,
                                       float t, float n, float f)
{
   _left   = l;    
   _right  = r;    
   _bottom = b;    
   _top    = t;    
   _near   = n;    
   _far    = f;    
   _projectionMatrixDirty = true;
}
//////////////////////////////////////////////////////////
void cfdPBufferManager::setLookFrom(const float* lookFrom)
{
   _lookFrom[0] = lookFrom[0];
   _lookFrom[1] = lookFrom[1];
   _lookFrom[2] = lookFrom[2];
   _viewMatrixDirty = true;
}
//////////////////////////////////////////////////
void cfdPBufferManager::setLookUp(const float* up)
{
   _up[0] = up[0];
   _up[1] = up[1];
   _up[2] = up[2];
   _viewMatrixDirty = true;
}
//////////////////////////////////////////////////////
void cfdPBufferManager::setCenterOfInterest(float coi)
{
   _centerOfInterest = coi;
   _viewMatrixDirty = true;
}
//////////////////////////////////////////////////////
void cfdPBufferManager::setLookAt(const float* lookAt)
{
   _lookAt[0] = lookAt[0];
   _lookAt[1] = lookAt[1];
   _lookAt[2] = lookAt[2];
   _viewMatrixDirty = true;
}
///////////////////////////////////////////////////
void cfdPBufferManager::applyViewMatrix(bool force)
{
   _ensureViewMatrix(force);
   glMatrixMode(GL_MODELVIEW);
   glLoadMatrixf(_viewMatrix);
}
/////////////////////////////////////////////////////
void cfdPBufferManager::_ensureViewMatrix(bool force)
{
  

   if(_viewMatrixDirty || force) { 
      //glGetFloatv(GL_MODELVIEW_MATRIX, (GLfloat *) _viewMatrix);
      //return;

      float n[3];
      float u[3];
      float v[3];
      float imag;

      n[0] = _lookFrom[0] - _lookAt[0];
      n[1] = _lookFrom[1] - _lookAt[1];
      n[2] = _lookFrom[2] - _lookAt[2];

      imag = 1.0f / sqrt( n[0]*n[0] + n[1]*n[1] + n[2]*n[2] );
      n[0] *= imag; n[1] *= imag; n[2] *= imag;

      // calculate the view up vector, "v"

      // Need to check that "n" != "_approxViewUp"
      if( _up[0] == n[0] && _up[1] == n[1] && _up[2] == n[2] ) {
         // Alter the vector slightly...
         _up[0] += 0.1f;
         imag = 1.0f / sqrt(_up[0]*_up[0] + 
                          _up[1]*_up[1] + 
                          _up[2]*_up[2] );

         _up[0] *= imag; _up[1] *= imag; _up[2] *= imag;

      }

      _cross(_up, n, u);

      imag = 1.0f / sqrt( u[0]*u[0] + u[1]*u[1] + u[2]*u[2] );
      u[0] *= imag; u[1] *= imag; u[2] *= imag;

      _cross(n, u, v);

      _viewMatrix[0]  = u[0];
      _viewMatrix[4]  = u[1];
      _viewMatrix[8]  = u[2];
      _viewMatrix[12] = u[0] * (-_lookFrom[0]) +
                      u[1] * (-_lookFrom[1]) +
                      u[2] * (-_lookFrom[2]);


      _viewMatrix[1]  = v[0];
      _viewMatrix[5]  = v[1];
      _viewMatrix[9]  = v[2];
      _viewMatrix[13] = v[0] * (-_lookFrom[0]) +
                      v[1] * (-_lookFrom[1]) +
                      v[2] * (-_lookFrom[2]);


      _viewMatrix[2]  = n[0];
      _viewMatrix[6]  = n[1];
      _viewMatrix[10] = n[2];
      _viewMatrix[14] = n[0] * (-_lookFrom[0]) +
                      n[1] * (-_lookFrom[1]) +
                      n[2] * (-_lookFrom[2]);

      _viewMatrix[3]  = 0.0f;
      _viewMatrix[7]  = 0.0f;
      _viewMatrix[11] = 0.0f;
      _viewMatrix[15] = 1.0f;

      _viewMatrixDirty = false;

   }
}
/////////////////////////////////////////////////////////
void cfdPBufferManager::applyProjectionMatrix(bool force)
{
   if(_projectionMatrixDirty || force) {

     float a = 1.0f / (_right - _left);
     float b = 1.0f / (_top   - _bottom);
     float c = 1.0f / (_far   - _near);

     if(_type == cfdPBufferManager::Perspective) {
         _projectionMatrix[0]  = (2.0f * _near) * a;
         _projectionMatrix[5]  = (2.0f * _near) * b;
         _projectionMatrix[10] = -(_far + _near) * c;
         _projectionMatrix[15] = 0.0f;

         _projectionMatrix[8] = (_right + _left  )      * a;
         _projectionMatrix[9] = (_top   + _bottom)      * b;
         _projectionMatrix[14] = (-2.0f * _far * _near) * c;
         _projectionMatrix[11] = -1.0f;

         // Zero out everything else
         _projectionMatrix[1] = 0.0f;
         _projectionMatrix[2] = 0.0f;
         _projectionMatrix[3] = 0.0f;

         _projectionMatrix[4] = 0.0f;
         _projectionMatrix[6] = 0.0f;
         _projectionMatrix[7] = 0.0f;

         _projectionMatrix[12] = 0.0f;
         _projectionMatrix[13] = 0.0f;

     }else{
         _projectionMatrix[0]  =  2.0f * a;
         _projectionMatrix[5]  =  2.0f * b;
         _projectionMatrix[10] = -2.0f * c;
         _projectionMatrix[15] = 1.0f;

         _projectionMatrix[12] = -(_right + _left  ) * a;
         _projectionMatrix[13] = -(_top   + _bottom) * b;
         _projectionMatrix[14] = -(_far   + _near  ) * c;


         // Zero out everything else
         _projectionMatrix[1] = 0.0f;
         _projectionMatrix[2] = 0.0f;
         _projectionMatrix[3] = 0.0f;
         _projectionMatrix[4] = 0.0f;
         _projectionMatrix[6] = 0.0f;
         _projectionMatrix[7] = 0.0f;
         _projectionMatrix[8] = 0.0f;
         _projectionMatrix[9] = 0.0f;
         _projectionMatrix[11] = 0.0f;

     }
        _projectionMatrixDirty = false;
    }

   glMatrixMode(GL_PROJECTION);
   glLoadMatrixf(_projectionMatrix);
   glMatrixMode(GL_MODELVIEW);
}
//////////////////////////////////////////////////////////
void cfdPBufferManager::beginSlicing(unsigned int nSlices)
{
   _numSlices = nSlices;
   if(!_slices){
      _slices = new float[_numSlices];
   }
   // Bounds are in world space.  Transform into
   // view space.  We are only interested in the
   // z position.

   int totalSlices = _numSlices+1;

   _ensureViewMatrix(false);

   float viewZ = _geomCenter[0]*_viewMatrix[ 2] +
                  _geomCenter[1]*_viewMatrix[ 6] +
                  _geomCenter[2]*_viewMatrix[10] + _viewMatrix[14];

   float zInc = (2*_geomRadius+_offsetMax) / (float)(totalSlices-1);

   float currentZ = fabs(viewZ)-_geomRadius+_offsetMin;
   currentZ = (fabs(currentZ) < 1e-6)?0:currentZ;

   int z;
   for(z = 0; z < totalSlices; ++z) {
      _slices[z] = currentZ;
      currentZ += zInc;
   }

   _paramCache[0] = _left;
   _paramCache[1] = _right;
   _paramCache[2] = _top;
   _paramCache[3] = _bottom;
   _paramCache[4] = _near;
   _paramCache[5] = _far;
}
///////////////////////////////////////////////////////////////////////
void cfdPBufferManager::setGeometricParams(float* center,float radius)
{
   _geomCenter[0] = center[0];
   _geomCenter[1] = center[1];
   _geomCenter[2] = center[2];
   _geomRadius = radius;
}
////////////////////////////////////////////
void cfdPBufferManager::initSlice(unsigned int slice)
{

    if(slice < 0 || slice > _numSlices) return;

    // Set the near and far clipping planes based
    // on the current slice

    // Compute fov...

    float fov = atan(_right/_near);

    float aspect = _right/_top;

    _near = _slices[slice];
    _far  = _slices[slice+1];

    // maintain fov
    _right  = _near*tan(fov);    
    _left   = -_right;

    // maintain aspect
    _top    = _right/aspect;    
    _bottom = -_top;    

    _projectionMatrixDirty = true;

}
////////////////////////////////////
void cfdPBufferManager::endSlicing()
{

    _left   = _paramCache[0];
    _right  = _paramCache[1];
    _top    = _paramCache[2];
    _bottom = _paramCache[3];
    _near   = _paramCache[4];
    _far    = _paramCache[5];

    _projectionMatrixDirty = true;

}
/////////////////////////////////////////////////////////////////////////////
void cfdPBufferManager::_cross(const float* v0, const float* v1, float* out)
{
    out[0] = v0[1]*v1[2] - v0[2]*v1[1];
    out[1] = v0[2]*v1[0] - v0[0]*v1[2];
    out[2] = v0[0]*v1[1] - v0[1]*v1[0];
}
//////////////////////////////////////////////////////
//initialize the entry functions                    //
//////////////////////////////////////////////////////
void cfdPBufferManager::initializePBufferEntryFunctions()
{
   //our pbuffer initialization check
   if(!wglCreatePbufferARB){
      wglCreatePbufferARB = (PFNWGLCREATEPBUFFERARBPROC)wglGetProcAddress( "wglCreatePbufferARB" );
   }
   if(!wglCreatePbufferARB){
      std::cout<<"PBuffers not supported!!"<<std::endl;
      _isSupported = 0;     
      return ;

   }
   
   if(!wglGetPbufferDCARB){
      wglGetPbufferDCARB = (PFNWGLGETPBUFFERDCARBPROC)wglGetProcAddress( "wglGetPbufferDCARB" );
   }
   if(!wglGetPbufferDCARB){
      std::cout<<"PBuffers not supported!!"<<std::endl;
      _isSupported = 0;
      return ;
   }

   if(!wglReleasePbufferDCARB){
      wglReleasePbufferDCARB = (PFNWGLRELEASEPBUFFERDCARBPROC)wglGetProcAddress( "wglReleasePbufferDCARB" );
   }
   if(!wglReleasePbufferDCARB){
      std::cout<<"PBuffers not supported!!"<<std::endl;
      _isSupported = 0;
      return ;
   }

  
   if(!wglDestroyPbufferARB){
      wglDestroyPbufferARB = (PFNWGLDESTROYPBUFFERARBPROC)wglGetProcAddress( "wglDestroyPbufferARB" );
   }
   if(!wglDestroyPbufferARB){
      std::cout<<"PBuffers not supported!!"<<std::endl;
      _isSupported = 0;
      return ;
   }

   if(!wglQueryPbufferARB){
      wglQueryPbufferARB = (PFNWGLQUERYPBUFFERARBPROC)wglGetProcAddress( "wglQueryPbufferARB" );
   }
   if(!wglQueryPbufferARB){
      std::cout<<"PBuffers not supported!!"<<std::endl;
      _isSupported = 0;
      return ;
   }


   if(!wglGetPixelFormatAttribivARB){
      wglGetPixelFormatAttribivARB = (PFNWGLGETPIXELFORMATATTRIBIVARBPROC)wglGetProcAddress( "wglGetPixelFormatAttribivARB" );
   }
   if(!wglGetPixelFormatAttribivARB){
      std::cout<<"PBuffers not supported!!"<<std::endl;
      _isSupported = 0;
      return ;
   }

   if(!wglGetPixelFormatAttribfvARB){
      wglGetPixelFormatAttribfvARB = (PFNWGLGETPIXELFORMATATTRIBFVARBPROC)wglGetProcAddress( "wglGetPixelFormatAttribfvARB" );
   }
   if(!wglGetPixelFormatAttribfvARB){
      std::cout<<"PBuffers not supported!!"<<std::endl;
      _isSupported = 0;
      return ;
   }

   if(!wglChoosePixelFormatARB){
      wglChoosePixelFormatARB = (PFNWGLCHOOSEPIXELFORMATARBPROC)wglGetProcAddress( "wglChoosePixelFormatARB" );
   }
   if(!wglChoosePixelFormatARB){
      std::cout<<"PBuffers not supported!!"<<std::endl;
      _isSupported = 0;
      return ;
   }

   /*if(!wglShareLists){
      wglShareLists = (PFNWGLSHARELISTSPROC)wglGetProcAddress( "wglShareLists" );
   }*/
   _isSupported = 1;
}
/////////////////////////////////////
//clean up the pbuffer             //
/////////////////////////////////////
void cfdPBufferManager::cleanUpPBuffer()
{
   if(_pBufferGLContext)
      wglDeleteContext( _pBufferGLContext);
	if(_pBufferDeviceContext)
      wglReleasePbufferDCARB( _hBuffer, _pBufferDeviceContext );
	if(_hBuffer)
      wglDestroyPbufferARB( _hBuffer );
}
////////////////////////////////////////////////////////////
//initialize a pixel buffer                               //
//this functionality will be                              //
//expanded so that you can specialize                     //
//the buffer to you needs but for now                     //
//it is set up w/ default params                          //
////////////////////////////////////////////////////////////
int cfdPBufferManager::initializePBuffer(int width, int height)
{
   _w = width;
   _h = height;
   // Get ready to query for a suitable pixel format that meets our
   // minimum requirements.
   //get the current frame buffer context
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
                //WGL_BIND_TO_TEXTURE_RGBA_ARB, true,
                0
            };
   // Now obtain a list of pixel formats that meet these minimum
   // requirements.
   #define MAX_PFORMATS 10
   int format = 0;
   int pformat[MAX_PFORMATS];
   unsigned int nformats=0;
   
   if ( !wglChoosePixelFormatARB( _frameBufferDeviceContext, 
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

    int pb_attr[] =
	{
		//WGL_TEXTURE_FORMAT_ARB, WGL_TEXTURE_RGBA_ARB, // Our p-buffer will have a texture format of RGBA
		//WGL_TEXTURE_TARGET_ARB, WGL_TEXTURE_2D_ARB,   // Of texture target will be GL_TEXTURE_2D
		0                                             // Zero terminates the list
	};
    
    

    //create the buffer
    _hBuffer = wglCreatePbufferARB( _frameBufferDeviceContext, 
                                format,_w,
                                _h,pb_attr );

    if(!_hBuffer){
       printf("Unable to create floating point pbuffer (wglCreatePbufferARB failed)\n");
       return 0;
    }

    //pbuffer device contex
   _pBufferDeviceContext = wglGetPbufferDCARB( _hBuffer );
   if(_pBufferDeviceContext == NULL){
      printf("Unable to retrieve handle to pbuffer device context\n");
      return 0;
   }
   //pbuffer gl context
   _pBufferGLContext = wglCreateContext( _pBufferDeviceContext );
   if(_pBufferGLContext ==NULL){
      printf("Unable to create a rendering context for the pbuffer\n");
      return 0;
   }
   // With sharing turned on, we can simple create our dynamic texture by 
	// binding it to the p-buffer and calling glCopyTexSubImage2D, but this
	// will only work if the window's rendering context is sharing its display 
	// lists and texture space with the p-buffer's context.
	//

	if( !wglShareLists( _frameBDGLContext, _pBufferGLContext ) )
	{
		MessageBox(NULL,"Call to wglShareLists() failed for p-buffer!",
			"ERROR",MB_OK|MB_ICONEXCLAMATION);
		exit(-1);
	}

   //setViewport(width,height);
   _isCreated = 1;
   return 1;
}
/////////////////////////////////////
void cfdPBufferManager::setViewport(double w, double h)
{
   glViewport(0.0, 0.0,w,h);
   _w = w;
   _h = h;
}
/////////////////////////////////
//check if pbuffer extensions  //
//are supported                //
/////////////////////////////////
int cfdPBufferManager::isSupported()
{
   initializePBufferEntryFunctions();
   return _isSupported;
}
#endif
#endif
#endif

   
