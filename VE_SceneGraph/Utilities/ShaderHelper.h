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
 * File:          $RCSfile: ShaderHelper.h,v $
 * Date modified: $Date: 2005-11-03 17:22:13 -0600 (Thu, 03 Nov 2005) $
 * Version:       $Rev: 3259 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef SHADER_HELPER_H
#define SHADER_HELPER_H
/*!\file ShaderHelper.h
  ShaderHelper API
  */
/*!\class VE_SceneGraph::Utilities::ShaderHelper
 * Class that creates an OSG StateSet representing
 * a glsl program.
 */
#include "VE_Installer/include/VEConfig.h"
///\todo This class still needs to be implemented for performer
#ifdef _OSG
#include <osg/StateSet>

namespace osg
{
   class Shader;
   class Program;
}
#elif _PERFORMER
#endif
namespace VE_Shader
{
   class Program;
   class Shader;
}
#include <string>
#include <vector>
///////////////////////////////////////////////////////////////////////
//this class is used to create a stateset representing a glsl program//
///////////////////////////////////////////////////////////////////////
namespace VE_SceneGraph
{
namespace Utilities
{
class VE_SCENEGRAPH_UTILS_EXPORTS ShaderHelper{
public:
   ///Constructor
   ShaderHelper();

   ///Copy Constructor
   ShaderHelper(const ShaderHelper& rhs);
  
   ///Destructor
   virtual ~ShaderHelper();

   ///Load and create the stateset from the input XML data
   void LoadGLSLProgram(VE_Shader::Program* glslProgram);
#ifdef _OSG
   ///Get the created state set representing the shader
   osg::StateSet* GetProgramStateSet();

   ///The state set that we want to load the shader into
   ///\param shader The state set representing the shader.
   void SetStateSet(osg::StateSet* shader);
#elif _PERFORMER
#endif
   ///Equal operator
   ///\param rhs Right hand side.
   ShaderHelper& operator=(const ShaderHelper& rhs);
protected:
   ///helper functions
   ///Utility function to create a shader.
   ///\param shaderData The XML shader data.
   void _createGLSLShader(VE_Shader::Shader* shaderData);
  
   ///Attach the program to the stateset
   ///\param override Flag to override the stateset above
   void _attachGLSLProgramToStateSet(bool override = true);

   ///Extract uniforms from the shader.
   ///\param The shader to extract uniforms from.
   void _extractUniformsFromShader(VE_Shader::Shader* shader);

   std::vector<std::string> _vertexUniformNames;///<Vertex program uniform names.
   std::vector<std::string> _fragmentUniformNames;///<Fragment program uniform names.
#ifdef _OSG
   osg::ref_ptr<osg::Shader> _vshader;///<The vertex shader.
   osg::ref_ptr<osg::Shader> _fshader;///<The fragment shader.
   osg::ref_ptr<osg::Program> _glslProgram;///<The GLSL program.
   osg::ref_ptr<osg::StateSet> _ss;///<The stateset representing the GLSL program.
#elif _PERFORMER
#endif
};
}
}
#endif// SHADER_HELPER_H
