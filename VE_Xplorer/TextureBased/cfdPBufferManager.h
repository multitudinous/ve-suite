/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#ifndef _BIV_PBUFFER_MANAGER_H_
#define _BIV_PBUFFER_MANAGER_H_
/*!\file cfdPBufferManager.h
* cfdPBufferManager API
*/

/*!\class VE_TextureBased::cfdPBufferManager
*
*/
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

#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdPBufferManager
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
}
#endif
#endif //_BIV_PBUFFER_MANAGER_H_
