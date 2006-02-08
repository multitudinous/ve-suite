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

#include <utility>
#include <string>

using namespace VE_XML;
XMLObjectFactory* XMLObjectFactory::_instanceOfFactory = 0;
/////////////////////////////////////
XMLObjectFactory::XMLObjectFactory( )
{
}
/////////////////////////////////////
XMLObjectFactory::~XMLObjectFactory()
{
   for(std::map<std::string, CreationEventHandler* >::iterator itr = _objectCreators.begin();
                                       itr != _objectCreators.end(); itr++ )
   {
      delete itr->second;
      itr->second = 0;
   }
   _objectCreators.clear();
   
}
//////////////////////////////////////////////
XMLObjectFactory* XMLObjectFactory::Instance()
{
   if(!_instanceOfFactory)
   {
      _instanceOfFactory = new XMLObjectFactory();
   }
   return _instanceOfFactory;
}
/////////////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* XMLObjectFactory::CreateXMLObject(std::string objectNameSpace,
                                                     std::string objectType)
{
   std::map<std::string,CreationEventHandler* >::iterator xmlCreator;
   xmlCreator = _objectCreators.find(objectNameSpace);
   if(xmlCreator != _objectCreators.end())
   {
      return xmlCreator->second->CreateNewXMLObject(objectType);
   }
  /*if(objectType == "FloatArray"){
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
   }*/
   return 0;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
VE_XML::XMLObject* XMLObjectFactory::CreateXMLObjectCopy(std::string objectType,
                                                         std::string objectNamespace,
                                                         VE_XML::XMLObject* objectToCopy)
{
   std::map<std::string,CreationEventHandler* >::iterator xmlCreator;
   xmlCreator = _objectCreators.find(objectNamespace);
   if(xmlCreator != _objectCreators.end())
   {
      return xmlCreator->second->CreateNewXMLObjectCopy(objectType,objectToCopy);
   }
   /*if(objectType == "FloatArray"){
      return new FloatArray(*dynamic_cast<FloatArray*>(objectToCopy));
   }else if(objectType == "Command"){
      return new Command(*dynamic_cast<Command*>(objectToCopy));
   }else if(objectType == "DataValuePair"){
      return new DataValuePair(*dynamic_cast<DataValuePair*>(objectToCopy));
   }else if(objectType == "OneDDoubleArray"){
      return new OneDDoubleArray(*dynamic_cast<OneDDoubleArray*>(objectToCopy));
   }else if(objectType == "OneDIntArray"){
      return new OneDIntArray(*dynamic_cast<OneDIntArray*>(objectToCopy));
   }else if(objectType == "OneDStringArray"){
      return new OneDStringArray(*dynamic_cast<OneDStringArray*>(objectToCopy));
   }else if(objectType == "ParameterBlock"){
      return new ParameterBlock(*dynamic_cast<ParameterBlock*>(objectToCopy));
   }else if(objectType == "StateInfo"){
      return new StateInfo(*dynamic_cast<StateInfo*>(objectToCopy));
   }else if(objectType == "ThreeDDoubleArray"){
      return new ThreeDDoubleArray(*dynamic_cast<ThreeDDoubleArray*>(objectToCopy));
   }else if(objectType == "ThreeDIntArray"){
      return new ThreeDIntArray(*dynamic_cast<ThreeDIntArray*>(objectToCopy));
   }else if(objectType == "Transform"){
      return new Transform(*dynamic_cast<Transform*>(objectToCopy));
   }else if(objectType == "TwoDDoubleArray"){
      return new TwoDDoubleArray(*dynamic_cast<TwoDDoubleArray*>(objectToCopy));
   }else if(objectType == "TwoDIntArray"){
      return new TwoDIntArray(*dynamic_cast<TwoDIntArray*>(objectToCopy));
   }else if(objectType == "User"){
      return new User(*dynamic_cast<User*>(objectToCopy));
   }*/
   return 0;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
bool XMLObjectFactory::RegisterObjectCreator(std::string objectNamespace,CreationEventHandler* newCreator)
{
   std::map<std::string,CreationEventHandler* >::iterator xmlCreator;
   if(_objectCreators.find(objectNamespace) != _objectCreators.end())
   {
      return false;
   }
   if(objectNamespace != "XML" || 
      objectNamespace != "CAD" ||
      objectNamespace != "Shader")
   {
      std::cout<<"Invalid namespace specified: "<<objectNamespace<<std::endl;
      std::cout<<"Valid namespaces are: "<<std::endl;
      std::cout<<"XML"<<std::endl;
      std::cout<<"CAD"<<std::endl;
      std::cout<<"Shader"<<std::endl;
      return false;
   }
   _objectCreators[objectNamespace] = newCreator;
   return true;
}
