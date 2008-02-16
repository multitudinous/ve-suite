/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CONCRETE_LOADER_H
#define CONCRETE_LOADER_H
/*!\file ConcreteLoader.h
  ConcreteLoader API
  */
/*!\class ves::xplorer::scenegraph::util::ConcreteLoader
 * Class that creates an OSG StateSet representing
 * a procedrual concrete shading glsl program.
 */
#include <ves/VEConfig.h>
#include <ves/xplorer/scenegraph/util/ShaderHelper.h>
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
class PerlinNoiseTexture;
class VE_SCENEGRAPH_UTILS_EXPORTS ConcreteLoader :
            public ves::xplorer::scenegraph::util::ShaderHelper
{
public:
    ///Constructor
    ConcreteLoader();

    ///Copy Constructor
    ConcreteLoader( const ConcreteLoader& rhs );

    ///Destructor
    virtual ~ConcreteLoader();

    ///Sync up the active stateset with the current shader
    virtual void SyncShaderAndStateSet();

    ///Equal operator
    ///\param rhs Right hand side.
    ConcreteLoader& operator=( const ConcreteLoader& rhs );
protected:
    ///Load and create the stateset for phong shader
    ///\param vertexSource The vertex shader source code
    ///\param fragSource The fragment shader source code
    virtual void _loadShader( std::string vertexSource, std::string fragSource );

    PerlinNoiseTexture* m_noise;///<The noise texture
};
}
}
}
}

#endif
#endif// PHONG_LOADER_H
