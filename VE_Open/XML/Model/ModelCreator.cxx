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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Open/XML/Model/ModelCreator.h"

#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/Model/Network.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Port.h"
#include "VE_Open/XML/Model/TagPtr.h"
#include "VE_Open/XML/Model/Tag.h"

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
   return 0;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObjectPtr ModelCreator::CreateNewXMLObjectSmart(std::string objectType)
{
    if ( objectType == "Tag" )
    {
        return new Tag();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* ModelCreator::CreateNewXMLObjectCopy(std::string objectType,
                                                     VE_XML::XMLObject* objectToCopy)
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
   return 0;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObjectPtr ModelCreator::CreateNewXMLObjectCopySmart(std::string objectType,
                                                        VE_XML::XMLObjectPtr objectToCopy)
{
    if ( objectType == "Tag" )
    {
        return new Tag( *dynamic_cast< Tag* >( &*objectToCopy ) );
    }
    return 0;
}
