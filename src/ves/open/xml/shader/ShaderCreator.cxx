/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/open/xml/shader/ShaderCreator.h>

#include <ves/open/xml/shader/TextureImage.h>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/shader/Uniform.h>
#include <ves/open/xml/shader/Program.h>

using namespace ves::open::xml::shader;
////////////////////////////////////////////////////////////////////////////
ves::open::xml::XMLObjectPtr ShaderCreator::CreateNewXMLObject( const std::string& objectType )
{
    if( objectType == "Program" )
    {
        return ProgramPtr( new Program() );
    }
    else if( objectType == "Shader" )
    {
        return ShaderPtr( new Shader() );
    }
    else if( objectType == "Uniform" )
    {
        return UniformPtr( new Uniform() );
    }
    else if( objectType == "TextureImage" )
    {
        return TextureImagePtr( new TextureImage() );
    }
    return ves::open::xml::XMLObjectPtr();
}
////////////////////////////////////////////////////////////////////////////////////////
ves::open::xml::XMLObjectPtr ShaderCreator::CreateNewXMLObjectCopy(
                                    const std::string& objectType,
                                    const ves::open::xml::XMLObjectPtr& objectToCopy )
{
    if( objectType == "Program" )
    {
        return ProgramPtr( objectToCopy );
    }
    else if( objectType == "Shader" )
    {
        return ShaderPtr( objectToCopy );
    }
    else if( objectType == "Uniform" )
    {
        return UniformPtr( objectToCopy );
    }
    else if( objectType == "TextureImage" )
    {
        return TextureImagePtr( objectToCopy );
    }
    return ves::open::xml::XMLObjectPtr();
}
