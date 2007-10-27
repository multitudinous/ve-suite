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
 * Date modified: $Date: 2006-10-23 17:02:41 -0500 (Mon, 23 Oct 2006) $
 * Version:       $Rev: 5825 $
 * Author:        $Author: mccdo $
 * Id:            $Id: ShaderHelper.h 5825 2006-10-23 22:02:41Z mccdo $
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PHONG_LOADER_H
#define PHONG_LOADER_H
/*!\file PhongLoader.h
  PhongLoader API
  */
/*!\class ves::xplorer::scenegraph::util::PhongLoader
 * Class that creates an OSG StateSet representing
 * a phong shading glsl program.
 */
#include <ves/VEConfig.h>
#include <ves/xplorer/scenegraph/util/ShaderHelper.h>
///\todo This class still needs to be implemented for performer
#ifdef _OSG

#include <string>
#include <vector>
///////////////////////////////////////////////////////////////////////
//this class is used to create a stateset representing a phong shader// 
///////////////////////////////////////////////////////////////////////
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class VE_SCENEGRAPH_UTILS_EXPORTS PhongLoader : public ves::xplorer::scenegraph::util::ShaderHelper
{
public:
   ///Constructor
   PhongLoader();

   ///Copy Constructor
   PhongLoader(const PhongLoader& rhs);
  
   ///Destructor
   virtual ~PhongLoader();
  
   ///Sync up the active stateset with the current shader
   virtual void SyncShaderAndStateSet(); 

   ///Equal operator
   ///\param rhs Right hand side.
   PhongLoader& operator=(const PhongLoader& rhs);
protected:
   ///Load and create the stateset for phong shader
   ///\param vertexSource The vertex shader source code
   ///\param fragSource The fragment shader source code
   virtual void _loadShader(std::string vertexSource, std::string fragSource);
};
}
}
}
}
#endif
#endif// PHONG_LOADER_H
