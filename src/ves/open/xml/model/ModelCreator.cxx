/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/open/xml/model/ModelCreator.h>

#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/model/TagPtr.h>
#include <ves/open/xml/model/Tag.h>

using namespace VE_XML::VE_Model;
////////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* ModelCreator::CreateNewXMLObject(std::string objectType)
{
    if ( objectType == "Link" )
    {
        return new Link();
    }
    else if ( objectType == "veModel" )
    {
        return new Model();
    } 
    else if ( objectType == "veNetwork" )
    {
        return new Network();
    }
    else if ( objectType == "Model" )
    {
        return new Model();
    } 
    else if ( objectType == "Network" )
    {
        return new Network();
    }
    else if ( objectType == "Point" )
    {
        return new Point();
    }
    else if ( objectType == "Port" )
    {
        return new Port();
    }
    else if ( objectType == "Tag" )
    {
        return new Tag();
    }
    else if ( objectType == "System" )
    {
        return new System();
    }
    else if ( objectType == "veSystem" )
    {
        return new System();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObjectPtr 
ModelCreator::CreateNewXMLObjectSmart( std::string objectType )
{
    if ( objectType == "Tag" )
    {
        return new Tag();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* 
ModelCreator::CreateNewXMLObjectCopy( std::string objectType,
    VE_XML::XMLObject* objectToCopy )
{
   if ( objectType == "Link" )
   {
      return new Link(*dynamic_cast<Link*>(objectToCopy));
   }
   else if ( objectType == "Model" )
   {
      return new Model(*dynamic_cast<Model*>(objectToCopy));
   }
   else if ( objectType == "Network" )
   {
      return new Network(*dynamic_cast<Network*>(objectToCopy));
   }
   else if ( objectType == "veModel" )
   {
      return new Model(*dynamic_cast<Model*>(objectToCopy));
   }
   else if ( objectType == "veNetwork" )
   {
      return new Network(*dynamic_cast<Network*>(objectToCopy));
   }
   else if ( objectType == "Point" )
   {
      return new Point(*dynamic_cast<Point*>(objectToCopy));
   }
   else if ( objectType == "Port" )
   {
      return new Port(*dynamic_cast<Port*>(objectToCopy));
   }
   else if ( objectType == "Tag" )
   {
      return new Tag(*dynamic_cast<Tag*>(objectToCopy));
   }
    else if ( objectType == "System" )
    {
        return new System(*dynamic_cast<System*>(objectToCopy));
    }
    else if ( objectType == "veSystem" )
    {
        return new System(*dynamic_cast<System*>(objectToCopy));
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObjectPtr 
ModelCreator::CreateNewXMLObjectCopySmart( std::string objectType,
    VE_XML::XMLObjectPtr objectToCopy )
{
    if ( objectType == "Tag" )
    {
        ///This is a hack and will be corrected 
        //with the proper use of a factory
        return new Tag( *dynamic_cast< Tag* >( &*objectToCopy ) );
    }
    return 0;
}
