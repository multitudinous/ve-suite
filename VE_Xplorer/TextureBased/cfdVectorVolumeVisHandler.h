/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VECTOR_VOLUME_VIS_HANDLER_H
#define CFD_VECTOR_VOLUME_VIS_HANDLER_H
#ifdef VE_PATENTED
#ifdef _OSG
namespace osg
{
   class Group;
   class Texture3D;
}
#include "VE_Xplorer/TextureBased/cfdVolumeVisNodeHandler.h"
namespace VE_TextureBased
{
   class cfdOSGAdvectionShaderManager;
   class cfdOSGTransferShaderManager;
   class cfdTextureManager;
   class cfdPBufferManager;
   class cfdUpdateTextureCallback;
   class cfd3DTextureCullCallback;
   class cfdOSGPingPongTexture3D;
}

namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdVectorVolumeVisHandler : public cfdVolumeVisNodeHandler{
      public:
         cfdVectorVolumeVisHandler();
         cfdVectorVolumeVisHandler(const cfdVectorVolumeVisHandler& vvnh);
         virtual ~cfdVectorVolumeVisHandler();
         virtual void Init();
         virtual void SetTextureManager(cfdTextureManager* tm);

         void SetPBufferManager(cfdPBufferManager* pbm);
         void PingPongTextures();
         void SetCurrentTransientTexture(unsigned int whichTimeStep,
                                      bool makeSlave = false);

         cfdOSGAdvectionShaderManager* GetAdvectionShaderManager(){return _aSM;}
         cfdVectorVolumeVisHandler& operator=(const cfdVectorVolumeVisHandler& vvnh);
      protected:
         virtual void _setUpDecorator();
         virtual void _applyTextureMatrix();
         void _createTexturePingPong();
         void _initPropertyTexture();
         void _createVelocityFromTextureManager();
         void _createTransferShader();
         void _setupTransferPropertyStateSet();
         void _setupAdvectionPropertyStateSet();

         bool _ssIsSet;

         cfdOSGAdvectionShaderManager* _aSM;
         cfdOSGTransferShaderManager* _transferSM;
   
         osg::ref_ptr<cfd3DTextureCullCallback> _cullCallback;
         osg::ref_ptr<cfdUpdateTextureCallback> _velocityCbk;
         cfdPBufferManager* _pbuffer;
         cfdOSGPingPongTexture3D* _texturePingPong;

         osg::ref_ptr<osg::Group> _advectionSlice;
         osg::ref_ptr<osg::Group> _propertyTextureGroup;
         osg::ref_ptr<osg::Texture3D> _property;
         osg::ref_ptr<osg::Texture3D> _velocity;

   };
}
#endif //_OSG
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
#endif
