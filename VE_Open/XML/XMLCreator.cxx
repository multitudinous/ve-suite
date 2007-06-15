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
#include "VE_Open/XML/XMLCreator.h"
#include "VE_Open/XML/XMLObject.h"

#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/OneDDoubleArray.h"
#include "VE_Open/XML/OneDIntArray.h"
#include "VE_Open/XML/OneDStringArray.h"
#include "VE_Open/XML/ParameterBlock.h" 
#include "VE_Open/XML/StateInfo.h"
#include "VE_Open/XML/ThreeDDoubleArray.h"
#include "VE_Open/XML/ThreeDIntArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/TwoDDoubleArray.h"
#include "VE_Open/XML/TwoDIntArray.h" 
#include "VE_Open/XML/User.h" 

using namespace VE_XML;
/////////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* XMLCreator::CreateNewXMLObject(std::string objectType)
{
   if(objectType == "FloatArray")
   {
      return new FloatArray();
   }
   else if(objectType == "Transform")
   {
      return new Transform();
   }
   else if(objectType == "Command")
   {
      return new Command();
   }
   else if ( objectType == "vecommand")
   {
	   return new Command();
   }
   else if(objectType == "DataValuePair")
   {
      return new DataValuePair();
   }
   else if(objectType == "OneDDoubleArray")
   {
      return new OneDDoubleArray();
   }
   else if(objectType == "OneDIntArray")
   {
      return new OneDIntArray();
   }
   else if(objectType == "OneDStringArray")
   {
      return new OneDStringArray();
   }
   else if(objectType == "ParameterBlock")
   {
      return new ParameterBlock();
   }
   else if(objectType == "StateInfo")
   {
      return new StateInfo();
   }
   else if(objectType == "ThreeDDoubleArray")
   {
      return new ThreeDDoubleArray();
   }
   else if(objectType == "Transform")
   {
      return new Transform();
   }
   else if(objectType == "TwoDDoubleArray")
   {
      return new TwoDDoubleArray();
   }
   else if(objectType == "TwoDIntArray")
   {
      return new TwoDIntArray();
   }
   else if(objectType == "ThreeDIntArray")
   {
      return new ThreeDIntArray();
   }
   else if(objectType == "User")
   {
      return new User();
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* XMLCreator::CreateNewXMLObjectCopy(std::string objectType,
                                                      VE_XML::XMLObject* objectToCopy)
{
   if(objectType == "FloatArray")
   {
      return new FloatArray(*dynamic_cast<FloatArray*>(objectToCopy));
   }
   else if(objectType == "Command")
   {
      return new Command(*dynamic_cast<Command*>(objectToCopy));
   }
   else if ( objectType == "vecommand")
   {
	   return new Command(*dynamic_cast<Command*>(objectToCopy));
   }
   else if(objectType == "DataValuePair")
   {
      return new DataValuePair(*dynamic_cast<DataValuePair*>(objectToCopy));
   }
   else if(objectType == "OneDDoubleArray")
   {
      return new OneDDoubleArray(*dynamic_cast<OneDDoubleArray*>(objectToCopy));
   }
   else if(objectType == "OneDIntArray")
   {
      return new OneDIntArray(*dynamic_cast<OneDIntArray*>(objectToCopy));
   }
   else if(objectType == "OneDStringArray")
   {
      return new OneDStringArray(*dynamic_cast<OneDStringArray*>(objectToCopy));
   }
   else if(objectType == "ParameterBlock")
   {
      return new ParameterBlock(*dynamic_cast<ParameterBlock*>(objectToCopy));
   }
   else if(objectType == "StateInfo")
   {
      return new StateInfo(*dynamic_cast<StateInfo*>(objectToCopy));
   }
   else if(objectType == "ThreeDDoubleArray")
   {
      return new ThreeDDoubleArray(*dynamic_cast<ThreeDDoubleArray*>(objectToCopy));
   }
   else if(objectType == "ThreeDIntArray")
   {
      return new ThreeDIntArray(*dynamic_cast<ThreeDIntArray*>(objectToCopy));
   }
   else if(objectType == "Transform")
   {
      return new Transform(*dynamic_cast<Transform*>(objectToCopy));
   }
   else if(objectType == "TwoDDoubleArray")
   {
      return new TwoDDoubleArray(*dynamic_cast<TwoDDoubleArray*>(objectToCopy));
   }
   else if(objectType == "TwoDIntArray")
   {
      return new TwoDIntArray(*dynamic_cast<TwoDIntArray*>(objectToCopy));
   }
   else if(objectType == "User")
   {
      return new User(*dynamic_cast<User*>(objectToCopy));
   }
   return 0;
}


