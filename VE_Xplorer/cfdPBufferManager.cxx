#ifdef _OSG
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

}
/////////////////////////////////
cfdPBufferManager::~cfdPBufferManager()
{
   cleanUpPBuffer();
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
                /*WGL_DEPTH_BITS_ARB,             16,*/
                WGL_SUPPORT_OPENGL_ARB,        true,
                WGL_DRAW_TO_PBUFFER_ARB,        true,
                WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB,
                //WGL_BIND_TO_TEXTURE_RGBA_ARB, true,
                0
            };
   // Now obtain a list of pixel formats that meet these minimum
   // requirements.
   //#define MAX_PFORMATS 10
   int format = 0;
   unsigned int nformats;
   
   if ( !wglChoosePixelFormatARB( _frameBufferDeviceContext, attribList, NULL,
                               1, &format, &nformats ) )
    {
       fprintf( stderr, "pbuffer creation error:  Couldn't find a \
                      suitable pixel format.\n" );
       return( 0 );
    }

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

   
