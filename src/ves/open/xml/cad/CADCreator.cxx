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
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAttribute.h>
#include <ves/open/xml/cad/CADMaterial.h>
#include <ves/open/xml/cad/CADNodeAnimation.h>

using namespace ves::open::xml::cad;

//////////////////////////////////////////////////////////////////////
ves::open::xml::XMLObjectPtr CADCreator::CreateNewXMLObject( const std::string& objectType )
{
    if( objectType == "CADAssembly" )
    {
        return CADAssemblyPtr( new CADAssembly() );
    }
    else if( objectType == "CADPart" )
    {
        return CADPartPtr( new CADPart() );
    }
    else if( objectType == "CADAttribute" )
    {
        return CADAttributePtr( new CADAttribute() );
    }
    else if( objectType == "CADMaterial" )
    {
        return CADMaterialPtr( new CADMaterial() );
    }
    else if( objectType == "CADNodeAnimation" )
    {
        return CADNodeAnimationPtr( new CADNodeAnimation() );
    }
    return XMLObjectPtr();
}
//////////////////////////////////////////////////////////////////////
ves::open::xml::XMLObjectPtr CADCreator::CreateNewXMLObjectCopy( const std::string& objectType,
        const ves::open::xml::XMLObjectPtr& objectToCopy )
{
    if( objectType == "CADAssembly" )
    {
        return CADAssemblyPtr( objectToCopy );
    }
    else if( objectType == "CADPart" )
    {
        return CADPartPtr( objectToCopy );
    }
    else if( objectType == "CADAttribute" )
    {
        return CADAttributePtr( objectToCopy );
    }
    else if( objectType == "CADMaterial" )
    {
        return CADMaterialPtr( objectToCopy );
    }
    else if( objectType == "CADNodeAnimation" )
    {
        return CADNodeAnimationPtr( objectToCopy );
    }
    return XMLObjectPtr();
}
