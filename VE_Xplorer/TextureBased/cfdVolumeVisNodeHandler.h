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
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VOLUME_VIZ_NODE_HANDLER_H
#define CFD_VOLUME_VIZ_NODE_HANDLER_H
/*!\file cfdVolumeVisNodeHandler.h
* cfdVolumeVisNodeHandler API
*/

/*!\class VE_TextureBased::cfdVolumeVisNodeHandler
*
*/
#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/BoundingBox>
#include <osg/ref_ptr>
#include <map>
namespace osg
{
   class Group;
   class Switch;
   class TexGenNode;
}
namespace VE_TextureBased
{
   class cfdTextureManager;
   class cfdOSGShaderManager;
}
#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdVolumeVisNodeHandler
   {
      public:
         cfdVolumeVisNodeHandler();
         cfdVolumeVisNodeHandler(const cfdVolumeVisNodeHandler& vvnh);
         virtual ~cfdVolumeVisNodeHandler();

         void SetSwitchNode(osg::Switch* vvn);
         void SetAttachNode(osg::Group* attachNode);
         void SetCenter(osg::Vec3f center);
         void SetTextureScale(float* scale,bool isInverted = true);
         void SetTextureManager(cfdTextureManager* tm);
         void SetBoundingBox(float* bbox);
         void SetBoundingBoxName(std::string name);
         void SetDecoratorName(std::string name);
         ///Update which scalar shader is active
         ///\param name The shader name
         void SetActiveShader(std::string name);
         bool IsThisActive();
         virtual void Init();
   
         void TurnOnBBox();
         void TurnOffBBox();

         void EnableDecorator();
   
         ///Add new shader manager.
         ///\param name The name of the shader
         ///\param newShader The shader manager
         void AddShaderManager(std::string name,
                               VE_TextureBased::cfdOSGShaderManager* newShader,
                               bool isScalar=true);
         
         ///Get the active shader
         std::string GetActiveShaderName();

		 ///Get a pointer to the active shader
		 VE_TextureBased::cfdOSGShaderManager* GetActiveShader();
         
         ///Get a shader manager
         ///\param name The name of the shader
         VE_TextureBased::cfdOSGShaderManager* GetShaderManager(std::string name);

         cfdVolumeVisNodeHandler& operator=(const cfdVolumeVisNodeHandler& vvnh);
      protected:
         void _createVisualBBox();
         //set up the stateset for the decorator
         virtual void _setUpDecorator()=0;
         virtual void _applyTextureMatrix()=0;
         virtual void _updateTexGenUnit(unsigned int unit=0);
         void _createTexGenNode();
         unsigned int _whichChildIsThis;
         unsigned int _whichTexture;
         bool _autoTexGen;
         cfdTextureManager* _tm;
         osg::ref_ptr<osg::Switch>_bboxSwitch;
         osg::ref_ptr<osg::Group> _visualBoundingBox;
         osg::ref_ptr<osg::Switch> _vvN;
         osg::ref_ptr<osg::Group> _decoratorGroup;
         osg::ref_ptr<osg::Group> _byPassNode;
         osg::ref_ptr<osg::TexGenNode> _texGenParams;
         osg::BoundingBox _bbox;
         osg::Vec3f _center;
         float _scale[3];

         std::string _activeShader;///<The active shader
         std::map<std::string,VE_TextureBased::cfdOSGShaderManager*> _shaderManagers;///<The shaders.
   };
}
#endif //_OSG
#endif// CFD_VOLUME_VIZ_NODE_HANDLER_H
#endif
