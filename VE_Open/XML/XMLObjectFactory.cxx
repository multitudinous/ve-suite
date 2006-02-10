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

//utility to properly initialize and delete the Singleton XMLObjectFactory
namespace VE_XML
{

class ObjectFactoryMaker
{
public:
  ObjectFactoryMaker()
  {
     VE_XML::XMLObjectFactory::Instance(); 
  };
  ~ObjectFactoryMaker()
  { 
     XMLObjectFactory::DeleteInstance(); 
  };
};
}


static VE_XML::ObjectFactoryMaker ObjectFactoryManager;

VE_XML::XMLObjectFactory* VE_XML::XMLObjectFactory::_instanceOfFactory = 0;
std::map<std::string,CreationEventHandler*> VE_XML::XMLObjectFactory::_objectCreators;
/////////////////////////////////////
XMLObjectFactory::XMLObjectFactory( )
{
}
/////////////////////////////////////
XMLObjectFactory::~XMLObjectFactory()
{
   if(_objectCreators.size()){
      for(std::map<std::string, CreationEventHandler* >::iterator itr = _objectCreators.begin();
                                       itr != _objectCreators.end(); itr++ )
      {
         delete itr->second;
         itr->second = 0;
      }
      _objectCreators.clear();
   }
}
///////////////////////////////////////
void XMLObjectFactory::DeleteInstance()
{
   if(_instanceOfFactory)
   {
      delete _instanceOfFactory;
      _instanceOfFactory = 0;
   }
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
VE_XML::XMLObject* XMLObjectFactory::CreateXMLObject(std::string objectType,
                                               std::string objectNameSpace)
{
   std::map<std::string,CreationEventHandler* >::iterator xmlCreator;
   xmlCreator = _objectCreators.find(objectNameSpace);
   if(xmlCreator != _objectCreators.end())
   {
      return xmlCreator->second->CreateNewXMLObject(objectType);
   }
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
   return 0;
}
/////////////////////////////////////////////////////////////////////////////
bool XMLObjectFactory::ObjectCreatorIsRegistered(std::string objectNamespace)
{
   std::map<std::string,CreationEventHandler* >::iterator xmlCreator;
   if(_objectCreators.find(objectNamespace) != _objectCreators.end())
   {
      return true;
   }
   return false;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
bool XMLObjectFactory::RegisterObjectCreator(std::string objectNamespace,CreationEventHandler* newCreator)
{
   std::map<std::string,CreationEventHandler* >::iterator xmlCreator;
   if(_objectCreators.find(objectNamespace) != _objectCreators.end())
   {
      return false;
   }
   if(objectNamespace != std::string("XML") && 
      objectNamespace != std::string("CAD") &&
      objectNamespace != std::string("Shader"))
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
