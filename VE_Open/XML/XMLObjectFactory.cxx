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
 * File:          $RCSfile: XMLObject.h,v $
 * Date modified: $Date: 2006-01-27 08:00:24 -0600 (Fri, 27 Jan 2006) $
 * Version:       $Rev: 3615 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Open/XML/XMLObjectFactory.h"
#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/CreationEventHandler.h"

#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/User.h"
#include "VE_Open/XML/StateInfo.h"
#include "VE_Open/XML/ParameterBlock.h"

#include "VE_Open/XML/OneDDoubleArray.h"
#include "VE_Open/XML/OneDIntArray.h"
#include "VE_Open/XML/OneDStringArray.h"

#include "VE_Open/XML/TwoDDoubleArray.h"
#include "VE_Open/XML/TwoDIntArray.h"

#include "VE_Open/XML/ThreeDDoubleArray.h"
#include "VE_Open/XML/ThreeDIntArray.h"

#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/CAD/CADClone.h"
#include "VE_Open/XML/CAD/CADAttribute.h"
#include "VE_Open/XML/CAD/CADMaterial.h"

#include "VE_Open/XML/Shader/TextureImage.h"
#include "VE_Open/XML/Shader/Uniform.h"
#include "VE_Open/XML/Shader/Program.h"
#include "VE_Open/XML/Shader/Shader.h"
#include <utility>
#include <string>

using namespace VE_XML;
using namespace VE_CAD;
using namespace VE_Shader;
/////////////////////////////////////
XMLObjectFactory::XMLObjectFactory( )
{
   /*std::pair<std::string,CreationEventHandler<VE_XML::XMLObject>* > transform;
   transform.first = std::string("Transform");

   transform.second = new CreationEventHandler<VE_XML::XMLObject>();
   _creationHandlers.insert(transform);

   _creationHandlers["Transform"] = new CreationEventHandler<VE_XML::Transform>;
   _creationHandlers["FloatArray"] = new FloatArrayCreator();

   _creationHandlers["OneDDoubleArray"] = new OneDDoubleArrayCreator();
   _creationHandlers["OneDIntArray"] = new OneDIntArrayCreator();
   _creationHandlers["OneDStringArray"] = new OneDStringArrayCreator();

   _creationHandlers["TwoDDoubleArray"] = new TwoDDoubleArrayreator();
   _creationHandlers["TwoDIntArray"] = new TwoDIntArrayCreator();

   _creationHandlers["ThreeDDoubleArray"] = new ThreeDDoubleArrayCreator();
   _creationHandlers["ThreeDIntArray"] = new ThreeDIntArrayCreator();

   _creationHandlers["CADAsembly"] = new AssemblyCreator();
   _creationHandlers["CADPart"] = new PartCreator();
   _creationHandlers["CADClone"] = new CloneCreator();
   _creationHandlers["CADAttribute"] = new AttributeCreator();*/
}
/////////////////////////////////////
XMLObjectFactory::~XMLObjectFactory()
{
   /*
   for ( std::map<std::string ,CreationEventHandler<class T>* >::iterator itr = _creationHandlers.begin();
                                       itr != _creationHandlers.end(); itr++ )
   {
      delete itr->second;
      itr->second = 0;
   }
   _creationHandlers.clear();
   */
}
////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* XMLObjectFactory::CreateXMLObject(std::string objectType)
{
   /*std::map<std::string,CreationEventHandler<class T>* >::iterator xmlCreator;
   xmlCreator = _creationHandlers.find(objectType);
   if(xmlCreator != _creationHandlers.end())
   {
      return xmlCreator->second->GetNewXMLObject();
   }*/
  if(objectType == "FloatArray"){
      return new FloatArray();
   }else if(objectType == "Transform"){
      return new Transform();
   }else if(objectType == "Command"){
      return new Command();
   }else if(objectType == "DataValuePair"){
      return new DataValuePair();
   }else if(objectType == "OneDDoubleArray"){
      return new OneDDoubleArray();
   }else if(objectType == "OneDIntArray"){
      return new OneDIntArray();
   }else if(objectType == "OneDStringArray"){
      return new OneDStringArray();
   }else if(objectType == "ParameterBlock"){
      return new ParameterBlock();
   }else if(objectType == "StateInfo"){
      return new StateInfo();
   }else if(objectType == "ThreeDDoubleArray"){
      return new ThreeDDoubleArray();
   }else if(objectType == "ThreeDIntArray"){
      return new ThreeDIntArray();
   }else if(objectType == "Transform"){
      return new Transform();
   }else if(objectType == "TwoDDoubleArray"){
      return new TwoDDoubleArray();
   }else if(objectType == "TwoDIntArray"){
      return new TwoDIntArray();
   }else if(objectType == "User"){
      return new User();
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////////
/*template <class T>
bool XMLObjectFactory::RegisterObject(std::string objectType)
{
   std::map<std::string,CreationEventHandler<class T>* >::iterator xmlCreator;
   if(_creationHandlers.find(objectType) != _creationHandlers.end())
   {
      return false
   }
   _creationHandlers[objectType] = new CreationHandler<class T>;
   return true;
}*/
