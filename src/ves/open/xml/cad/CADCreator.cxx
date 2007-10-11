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
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAttribute.h>
#include <ves/open/xml/cad/CADMaterial.h>
#include <ves/open/xml/cad/CADNodeAnimation.h>

using namespace VE_XML::VE_CAD;
//////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* CADCreator::CreateNewXMLObject(std::string objectType)
{
   if(objectType == "CADAssembly")
   {
      return new CADAssembly();
   }
   else if(objectType == "CADPart")
   {
      return new CADPart();
   } 
   else if(objectType == "CADAttribute")
   {
      return new CADAttribute();
   }
   else if(objectType == "CADMaterial")
   {
      return new CADMaterial();
   }
   else if(objectType == "CADNodeAnimation")
   {
      return new CADNodeAnimation();
   }
   return 0;
}
//////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* CADCreator::CreateNewXMLObjectCopy(std::string objectType,
                                                     VE_XML::XMLObject* objectToCopy)
{
   if(objectType == "CADAssembly")
   {
      return new CADAssembly(*dynamic_cast<CADAssembly*>(objectToCopy));
   }
   else if(objectType == "CADPart")
   {
      return new CADPart(*dynamic_cast<CADPart*>(objectToCopy));
   }
   else if(objectType == "CADAttribute")
   {
      return new CADAttribute(*dynamic_cast<CADAttribute*>(objectToCopy));
   }
   else if(objectType == "CADMaterial")
   {
      return new CADMaterial(*dynamic_cast<CADMaterial*>(objectToCopy));
   }
   else if(objectType == "CADNodeAnimation")
   {
      return new CADNodeAnimation(*dynamic_cast<CADNodeAnimation*>(objectToCopy));
   }
   return 0;
}
