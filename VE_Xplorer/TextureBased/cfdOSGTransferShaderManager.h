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
 * File:          $RCSfile: cfdOSGTransferShaderManager.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_OSG_TRANSFER_SHADER_MANAGER_H
#define CFD_OSG_TRANSFER_SHADER_MANAGER_H
#ifdef VE_PATENTED
#ifdef _OSG
namespace osg{
   class Texture3D;
   class Texture1D;
   class TexMat;
}
#include <vector>
namespace VE_TextureBased
{
   class cfdTextureManager;
   class cfdUpdateTextureCallback;
}

#include "VE_TextureBased/cfdOSGShaderManager.h"
#include "VE_TextureBased/cfdUpdateableOSGTexture1d.h"

#include <string>
namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdOSGTransferShaderManager
      : public cfdOSGShaderManager{
      public:
         cfdOSGTransferShaderManager();
         cfdOSGTransferShaderManager(const cfdOSGTransferShaderManager& sm);
         virtual ~cfdOSGTransferShaderManager();
         void SetUseTextureManagerForProperty(bool trueFalse);
         virtual void Init();
         void SetFieldSize(unsigned int x,unsigned int y,unsigned int z);
         void UpdateTransferFunction(cfdUpdateableOSGTexture1d::TransType type,
                                              float param,int whichFunction);
         void SetPropertyTexture(osg::Texture3D* property);
         void SetTextureMatrix(osg::TexMat* tmat);
         void InitTextureManager(cfdTextureManager* tm);
         void UpdateTextureManager(cfdTextureManager* tm);
   
         osg::Texture3D* GetPropertyTexture();

         virtual cfdOSGTransferShaderManager& operator=(const 
		                               cfdOSGTransferShaderManager& sm);
      protected:
         void _initTransferFunctions();
         void _createTransferFunction(bool gamma = false,
                                   bool clearList = false);
         virtual void _initPropertyTexture();
         void _setupStateSetForGLSL();
         unsigned int _fieldSize[3];
   
         osg::ref_ptr<osg::TexMat> _texMat;
         osg::ref_ptr<osg::Texture3D> _property;
         typedef osg::ref_ptr<osg::Texture1D> TransferFunction ;
         std::vector<TransferFunction> _transferFunctions;
         bool _reinit;
         bool _useTM;
         cfdTextureManager* _tm;
         osg::ref_ptr<cfdUpdateTextureCallback> _utCbk;
   };
}
#endif//_OSG
#endif
#endif// CFD_OSG_SCALAR_SHADER_MANAGER_H
