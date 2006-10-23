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
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/Shader/Program.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
#include "VE_Open/XML/CAD/CADMaterial.h"
#include "VE_Open/XML/CAD/CADCreator.h"
#include "VE_Open/XML/CAD/CADNodeAnimation.h"

#include "VE_Open/XML/XMLObjectFactory.h"

#include "VE_Open/XML/Shader/Shader.h"
#include "VE_Open/XML/Shader/Program.h"
#include "VE_Open/XML/Shader/ShaderCreator.h"

#include <ctime>
using namespace VE_CAD;
using namespace VE_XML::VE_Shader;
using namespace VE_XML;

//////////////////////////////////
///Constructor                  //
//////////////////////////////////
CADNode::CADNode(std::string name)
:VE_XML::XMLObject()
{
   _name = name;
   _parent = "";
   _transform = new Transform(); 
   _type = std::string("Node");
   //_uID = std::atoi(uuid.c_str());//static_cast<unsigned int>(time(NULL));
   _activeAttributeName = std::string("");
   SetObjectType("CADNode");
   SetObjectNamespace("CAD");
   //This may need to be somewhere else
   if(!XMLObjectFactory::Instance()->ObjectCreatorIsRegistered("CAD"))
   {
      XMLObjectFactory::Instance()->RegisterObjectCreator("CAD",new CADCreator());
   }

   if(!XMLObjectFactory::Instance()->ObjectCreatorIsRegistered("Shader"))
   {
      XMLObjectFactory::Instance()->RegisterObjectCreator("Shader",new ShaderCreator());
   }

}
///////////////////
///Destructor    //
///////////////////
CADNode::~CADNode()
{
   if(_transform){
      delete _transform;
      _transform = 0;
   }
   
   /*if(_attributeList.size())
   {
     size_t nAttributes =  _attributeList.size();
       for(size_t i = 0; i < nAttributes; i++)
      {
         delete _attributeList.at(i);
      }
     
   }*/
   _attributeList.clear();
   _animations.clear();
}
//////////////////////////////////////////////////////////////////////////
void CADNode::AddAnimation(std::string name,std::string animationFileName)
{
   CADNodeAnimation newAnimation;
   newAnimation.SetAnimationFileName(animationFileName);
   newAnimation.SetAnimationName(name);
   _animations.push_back(newAnimation);
}
///////////////////////////////////////////
void CADNode::SetNodeName(std::string name)
{
   _name = name;
}
////////////////////////////////////////////////////
void CADNode::SetParent(std::string parent)
{
   _parent = parent;
}
///////////////////////////////////////////////////////
void CADNode::SetTransform(VE_XML::Transform* transform)
{
   if(_transform)
   {
      delete _transform;
      _transform = 0;
   }
   _transform = new VE_XML::Transform(*transform);
}
///////////////////////////////////////////////////////////
void CADNode::AddAttribute(VE_CAD::CADAttribute attribute)
{
   _attributeList.push_back(attribute);
}
////////////////////////////////////////////////////////
void CADNode::RemoveAttribute(std::string attributeName)
{
   for ( std::vector<CADAttribute>::iterator itr = _attributeList.begin();
                                    itr != _attributeList.end();
                                    itr++ )
   {
      if((*itr).GetAttributeName() == attributeName)
      {
         _attributeList.erase(itr);
         break;
      }
   }
}
///////////////////////////////////////////////////////////
void CADNode::SetActiveAttribute(std::string attributeName)
{
   _activeAttributeName = attributeName;
}
////////////////////////////
bool CADNode::HasAnimation()
{
   return (!_animations.empty());
}
//////////////////////////////////
std::string CADNode::GetNodeType()
{
   return _type;
}
//////////////////////////////////
std::string CADNode::GetNodeName()
{
   return _name;
}
/////////////////////////////////////////
std::string CADNode::GetParent()
{
   return _parent;
}
//////////////////////////////////////////
VE_XML::Transform* CADNode::GetTransform()
{
   return _transform;
}
///////////////////////////////////////////////////////////////
VE_CAD::CADAttribute& CADNode::GetAttribute(unsigned int index)
{
   try
   {
      return _attributeList.at(index);
   }
   catch(...)
   {
      std::cout<<"ERROR!!!!!"<<std::endl;
      std::cout<<"Invalid index!!!"<<std::endl;
      std::cout<<"CADNode::GetAttribute(): "<<index<<std::endl;
      return _attributeList.at(0);;
   }
   return _attributeList.at(0);;
}
////////////////////////////////////////////////////////////
VE_CAD::CADAttribute& CADNode::GetAttribute(std::string name)
{
   size_t nAttributes = _attributeList.size();
   for(size_t i = 0; i < nAttributes; i++)
   {
      if(_attributeList.at(i).GetAttributeName() == name)
      {
         return _attributeList.at(i);
      }
   }
}
///////////////////////////////////////////////////
VE_CAD::CADAttribute& CADNode::GetActiveAttribute()
{
   return GetAttribute(_activeAttributeName);
}
/////////////////////////////
/*unsigned int CADNode::GetID()
{
   return _uID;
}*/
/////////////////////////////////////////////////
void CADNode::_updateVEElement(std::string input)
{
   _updateNodeType();
   _updateNodeName();

   //SetSubElement(std::string("nodeID"),_uID);
   SetAttribute("id",uuid);
   SetSubElement(std::string("parent"),_parent);

   if(!_transform)
   {
      _transform = new Transform();
   }
   _transform->SetOwnerDocument(_rootDocument);
   _veElement->appendChild( _transform->GetXMLData("transform") );

   if(_attributeList.size())
   {
      size_t nAttributes = _attributeList.size();
      for(size_t i = 0; i < nAttributes; i++)
      {
         _attributeList.at(i).SetOwnerDocument(_rootDocument);
         _veElement->appendChild( _attributeList.at(i).GetXMLData("attribute") );
      }
      SetSubElement(std::string("activeAttributeName"),_activeAttributeName);
   }

   if(_animations.size())
   {
      size_t nAnimations = _animations.size();
      for(size_t i = 0; i < nAnimations; i++)
      {
         _animations.at(i).SetOwnerDocument(_rootDocument);
         _veElement->appendChild( _animations.at(i).GetXMLData("animation") );
      }
   }
}
///////////////////////////////
void CADNode::_updateNodeName()
{
   DOMElement* nodeNameElement = _rootDocument->createElement(xercesString("name"));
   DOMText* nodeName = _rootDocument->createTextNode(xercesString(_name.c_str()));
   nodeNameElement->appendChild(nodeName);
   _veElement->appendChild(nodeNameElement);
}
///////////////////////////////
void CADNode::_updateNodeType()
{
   DOMElement* nodeTypeElement = _rootDocument->createElement(xercesString("type"));
   DOMText* nodeType = _rootDocument->createTextNode(xercesString(_type));
   nodeTypeElement->appendChild(nodeType);
   _veElement->appendChild(nodeTypeElement);
}
/////////////////////////////////////////////////////
void CADNode::SetObjectFromXMLData( DOMNode* xmlNode)
{
   DOMElement* currentElement = 0;
   const XMLCh* name;
   if(xmlNode->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      name = xmlNode->getNodeName();
      currentElement = dynamic_cast<DOMElement*>(xmlNode);
   }
   
   if(currentElement)
   {
      //break down the element
      {
         if(currentElement->hasChildNodes())
         {
            //Is there a better way to do this
            DOMElement* nameNode = GetSubElement(currentElement,std::string("name"),0);
            if(nameNode)
            {
              _name = ExtractDataStringFromSimpleElement( nameNode );
            }
            
            DOMElement* idNode = GetSubElement(currentElement,std::string("nodeID"),0);
            if(idNode)
            {
               VE_XML::XMLObject::SetID(ExtractIntegerDataNumberFromSimpleElement(idNode) );
            }
            else
            {
               VE_XML::XMLObject::GetAttribute(currentElement, "id",uuid);
            }
            DOMElement* typeNode = GetSubElement(currentElement,std::string("type"),0);
            if(typeNode)
            {
              _type = ExtractDataStringFromSimpleElement( typeNode );
            }
            DOMElement* parentNode = GetSubElement(currentElement,std::string("parent"),0);
            if(parentNode)
            {
               _parent = ExtractDataStringFromSimpleElement(parentNode);
            }
            size_t nOldAttributes = _attributeList.size();
            if(nOldAttributes > 0)
            {
               /*for(size_t i = nOldAttributes -1; i >= 0; i--)
               {
                  delete _attributeList.at(i);
               }*/
               _attributeList.clear();
            }
            DOMNodeList* attributeNodes = currentElement->getElementsByTagName(xercesString("attribute"));
            XMLSize_t nNewAttributes = attributeNodes->getLength();
            for(XMLSize_t  i = 0; i < nNewAttributes ; i++)
            {
               DOMElement* attributeNode = dynamic_cast<DOMElement*>(attributeNodes->item(i));
               
               //Need to check if the returned attribute belongs to this node
               if(attributeNode->getParentNode() == currentElement)
               {
                  //CADAttribute* newAttribute = new CADAttribute();
                  CADAttribute newAttribute;
                  newAttribute.SetObjectFromXMLData(attributeNode);
                  _attributeList.push_back(newAttribute);
               }
            }

            _animations.clear();
            DOMNodeList* animationNodes = currentElement->getElementsByTagName(xercesString("animation"));
            XMLSize_t nNewAnimations = animationNodes->getLength();
            for(XMLSize_t  i = 0; i < nNewAnimations ; i++)
            {
               DOMElement* animationNode = dynamic_cast<DOMElement*>(animationNodes->item(i));
               
               if(animationNode->getParentNode() == currentElement)
               {
                  CADNodeAnimation newAnimation;
                  newAnimation.SetObjectFromXMLData(animationNode);
                  _animations.push_back(newAnimation);
               }
            }

            
            DOMElement* activeAttribNode = GetSubElement(currentElement,std::string("activeAttributeName"),0);
            if(activeAttribNode)
            {
               _activeAttributeName = ExtractDataStringFromSimpleElement(activeAttribNode);
               SetActiveAttribute(_activeAttributeName);
            }

            
            DOMElement* transformNode = GetSubElement(currentElement,std::string("transform"),0);
            if(transformNode)
            {
               if(!_transform)
               {
                  _transform = new Transform();
               }
               _transform->SetObjectFromXMLData(transformNode);
            }
            
         }
      }
   }
}
/////////////////////////////////////////////////////
std::vector<CADAttribute> CADNode::GetAttributeList()
{
   return _attributeList;
}
///////////////////////////////////////////////////////////////////
VE_CAD::CADNodeAnimation& CADNode::GetAnimation(unsigned int index)
{
   try
   {
      return _animations.at(index);
   }
   catch(...)
   {
      std::cout<<"Invalid animation index: "<<index<<std::endl;
      std::cout<<"CADNode::GetAnimation()"<<std::endl;
   }
}
/////////////////////////////////////////////////////////
CADNodeAnimation& CADNode::GetAnimation(std::string name)
{
   size_t nAnimations = _animations.size();
   for(size_t i = 0; i < nAnimations; i++)
   {
      if(_animations.at(i).GetAnimationName() == name)
      {
         return _animations.at(i);
      }
   }
}
///////////////////////////////////////
size_t CADNode::GetNumberOfAnimations()
{
   return _animations.size();
}
/////////////////////////////////////
CADNode::CADNode(const CADNode& rhs)
:VE_XML::XMLObject(rhs)
{
   _parent = "";
   _transform = 0;;

   if(rhs._transform)
   {
      _transform = new VE_XML::Transform(*rhs._transform);
   }
   else
   {
      _transform = new Transform();
   }

   if(_attributeList.size())
   {
      _attributeList.clear();
   }
   for(size_t i = 0; i < rhs._attributeList.size(); i++)
   {
      _attributeList.push_back(rhs._attributeList.at(i));
   }
   for(size_t i = 0; i < rhs._animations.size(); i++)
   {
      _animations.push_back(rhs._animations.at(i));
   }
   _activeAttributeName = rhs._activeAttributeName;
   _parent = rhs._parent;
   _name = rhs._name;
   _type = rhs._type;
   //_uID = rhs._uID;
   
}
////////////////////////////////////////////////
CADNode& CADNode::operator=(const CADNode& rhs)
{
   //std::cout<<"CADNode operator= "<<std::endl;
   //std::cout<<"rhs: "<<rhs._uID<<std::endl;
   if ( this != &rhs )
   {
      XMLObject::operator =(rhs);
      if(_attributeList.size())
      {
         _attributeList.clear();
      }

      for(size_t i = 0; i < rhs._attributeList.size(); i++)
      {
         _attributeList.push_back(rhs._attributeList.at(i));
      }

      if(_animations.size())
      {
         _animations.clear();
      }
      for(size_t i = 0; i < rhs._animations.size(); i++)
      {
         _animations.push_back(rhs._animations.at(i));
      }
     
      if(_transform)
      {
         delete _transform;
         _transform = 0;
      }
      _transform = new Transform(*rhs._transform);
      _activeAttributeName = rhs._activeAttributeName;
      
      //_uID = rhs._uID;
      _parent = rhs._parent;
      _name = rhs._name;
   }
   return *this;
}

