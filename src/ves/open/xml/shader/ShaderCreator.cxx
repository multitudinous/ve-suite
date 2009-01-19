/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
    XMLObjectPtr tmp = XMLObjectPtr();
    if( objectType == "Program" )
    {
        tmp = ProgramPtr( new Program() );
    }
    else if( objectType == "Shader" )
    {
        tmp = ShaderPtr( new Shader() );
    }
    else if( objectType == "Uniform" )
    {
        tmp = UniformPtr( new Uniform() );
    }
    else if( objectType == "TextureImage" )
    {
        tmp = TextureImagePtr( new TextureImage() );
    }
    return tmp;
}
////////////////////////////////////////////////////////////////////////////////////////
ves::open::xml::XMLObjectPtr ShaderCreator::CreateNewXMLObjectCopy(
                                    const std::string& objectType,
                                    const ves::open::xml::XMLObjectPtr& objectToCopy )
{
    ves::open::xml::XMLObjectPtr tmp = ves::open::xml::XMLObjectPtr();
    if( objectType == "Program" )
    {
        tmp = ProgramPtr( new Program(
            *( boost::dynamic_pointer_cast<Program>( objectToCopy ) ) ) );
    }
    else if( objectType == "Shader" )
    {
        tmp = ShaderPtr( new Shader(
            *( boost::dynamic_pointer_cast<Shader>( objectToCopy ) ) ) );
    }
    else if( objectType == "Uniform" )
    {
        tmp = UniformPtr( new Uniform(
            *( boost::dynamic_pointer_cast<Uniform>( objectToCopy ) ) ) );
    }
    else if( objectType == "TextureImage" )
    {
        tmp = TextureImagePtr( new TextureImage(
            *( boost::dynamic_pointer_cast<TextureImage>( objectToCopy ) ) ) );
    }
    return tmp;
}
