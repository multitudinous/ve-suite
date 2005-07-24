/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_OSG_SHADER_MANAGER_H
#define CFD_OSG_SHADER_MANAGER_H
#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/StateSet>
namespace osg
{
   class Shader;
}
#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdOSGShaderManager{
   public:
      cfdOSGShaderManager();
      cfdOSGShaderManager(const cfdOSGShaderManager& sm);
      virtual ~cfdOSGShaderManager();

      virtual void Init() = 0;

      void SetShaderDirectory(char* dir);
      void SetBounds(float* bounds);
      void UseCG(bool useCG = false);

      osg::StateSet* GetShaderStateSet();
      unsigned int GetAutoGenTextureUnit(){return _tUnit;}

   protected:
      virtual cfdOSGShaderManager& operator=(const cfdOSGShaderManager& sm);

      //////////////////
      //GLSL interface//
      //////////////////
      virtual void _setupGLSLShaderProgram(osg::StateSet* ss,
                                        osg::Program* glslProgram,
                                        const std::string pgName,bool override = false);
      virtual osg::Shader* _createGLSLShaderFromFile(const std::string filename,
                                                           bool isFrag);
      virtual void _setupStateSetForGLSL()=0;

      osg::ref_ptr<osg::Shader> _vshader;
      osg::ref_ptr<osg::Shader> _fshader;

      osg::ref_ptr<osg::StateSet> _ss;
      char* _shaderDirectory;
      unsigned int _tUnit;
      float* _bounds;
      bool _useGLSL;
   };
}
#endif //_OSG
#endif
#endif// CFD_OSG_SHADER_MANAGER_H
