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
 * File:          $RCSfile: CADNode.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/Shader/Program.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
#include "VE_Open/XML/CAD/CADMaterial.h"
#include "VE_Open/XML/CAD/CADAttribute.h"


using namespace VE_CAD;
using namespace VE_Shader;
using namespace VE_XML;
//////////////////////////////////
///Constructor                  //
//////////////////////////////////
CADNode::CADNode( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDoc,
               std::string name)
:VE_XML::XMLObject(rootDoc)
{
   _name = name;
   _parent = 0;
   _transform = 0;//new VE_XML::Transform(_rootDocument); 
   _type = std::string("Node");
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
   
   if(_attributeList.size())
   {
      for(size_t i = _attributeList.size() -1; i >= 0; i--)
      {
         delete _attributeList.at(i);
      }
      _attributeList.clear();
   }
}
///////////////////////////////////////////
void CADNode::SetNodeName(std::string name)
{
   _name = name;
}
////////////////////////////////////////////////////
void CADNode::SetParent(VE_CAD::CADNode* parent)
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
////////////////////////////////////////////////////////
void CADNode::AddAttribute(VE_CAD::CADAttribute* attribute)
{
   _attributeList.push_back(attribute);
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
VE_CAD::CADNode* CADNode::GetParent()
{
   return _parent;
}
//////////////////////////////////////////
VE_XML::Transform* CADNode::GetTransform()
{
   return _transform;
}
////////////////////////////////////////////////////////////
VE_CAD::CADAttribute* CADNode::GetAttribute(unsigned int index)
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
      return 0;
   }
   return 0;
}
////////////////////////////////////////////////////////////
VE_CAD::CADAttribute* CADNode::GetAttribute(std::string name)
{
   size_t nAttributes = _attributeList.size();
   for(size_t i = 0; i < nAttributes; i++)
   {
      if(_attributeList.at(i)->GetAttributeName() == name)
      {
         return _attributeList.at(i);
      }
   }
   return 0;
}
/////////////////////////////////////////////////
void CADNode::_updateVEElement(std::string input)
{
   //how is this going to work???
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString(input));
   }

   _updateNodeName();
   if(_parent)
   {
      _veElement->appendChild( _parent->GetXMLData("parent") );
   }
   if(_transform)
   {
      _veElement->appendChild( _transform->GetXMLData("transform") );
   }

   if(_attributeList.size())
   {
      size_t nAttributes = _attributeList.size();
      for(size_t i = 0; i < nAttributes; i++)
      {
         _veElement->appendChild( _attributeList.at(i)->GetXMLData("attribute") );
      }
   }
   _updateNodeType();
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
            DOMElement* typeNode = GetSubElement(currentElement,std::string("type"),0);
            if(typeNode)
            {
              _type = ExtractDataStringFromSimpleElement( typeNode );
            }
            DOMElement* parentNode = GetSubElement(currentElement,std::string("parent"),0);
            if(parentNode)
            {
               _parent->SetObjectFromXMLData(parentNode);
            }
            size_t nOldAttributes = _attributeList.size();
            if(nOldAttributes > 0)
            {
               for(size_t i = nOldAttributes -1; i >= 0; i--)
               {
                  delete _attributeList.at(i);
               }
               _attributeList.clear();
            }
            DOMNodeList* attributeNodes = currentElement->getElementsByTagName(xercesString("attribute"));
            XMLSize_t nNewAttributes = attributeNodes->getLength();
            for(XMLSize_t  i = 0; i < nNewAttributes ; i++)
            {
               DOMElement* attributeNode = dynamic_cast<DOMElement*>(attributeNodes->item(i));
               CADAttribute* newAttribute = new CADAttribute(_rootDocument);
               newAttribute->SetObjectFromXMLData(attributeNode);
               _attributeList.push_back(newAttribute);
            }
            

            DOMElement* transformNode = GetSubElement(currentElement,std::string("transform"),0);
            if(transformNode)
            {
               if(!_transform)
               {
                  _transform = new Transform(_rootDocument);
               }
               _transform->SetObjectFromXMLData(transformNode);
            }
            
         }
      }
   }
}
//////////////////////////////////////////////////////
std::vector<CADAttribute*> CADNode::GetAttributeList()
{
   return _attributeList;
}
/////////////////////////////////////
CADNode::CADNode(const CADNode& rhs)
:VE_XML::XMLObject(rhs)
{

   _parent = 0;
   _transform = 0;

   if(rhs._transform)
      _transform = new VE_XML::Transform(*rhs._transform);
  
   if(_attributeList.size())
   {
      for(size_t i = _attributeList.size() -1; i >= 0; i--)
      {
         delete _attributeList.at(i);
      }
      _attributeList.clear();
   }

   for(size_t i = 0; rhs._attributeList.size(); i++)
   {
      _attributeList.push_back(rhs._attributeList.at(i));
   }
   _parent = rhs._parent;
   _name = rhs._name;
   _type = rhs._type;
   
}
////////////////////////////////////////////////
CADNode& CADNode::operator=(const CADNode& rhs)
{
   if ( this != &rhs )
   {
      XMLObject::operator =(rhs);
      if(_transform)
      {
         delete _transform;
         _transform = 0;
      }
      if(_attributeList.size())
      {
         for(size_t i = _attributeList.size() -1; i >= 0; i--)
         {
            delete _attributeList.at(i);
         }
         _attributeList.clear();
      }

      for(size_t i = 0; rhs._attributeList.size(); i++)
      {
         _attributeList.push_back(rhs._attributeList.at(i));
      }
      _transform = new VE_XML::Transform(*rhs._transform);
      _parent = rhs._parent;
      _name = rhs._name;
   }
   return *this;
}

