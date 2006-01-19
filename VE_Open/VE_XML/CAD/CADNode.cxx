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
#include "VE_Open/VE_XML/VETransform.h"
#include "VE_Open/VE_XML/CAD/CADAssembly.h"
#include "VE_Open/VE_XML/CAD/CADMaterial.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
//////////////////////////////////
///Constructor                  //
//////////////////////////////////
CADNode::CADNode( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDoc,
               std::string name)
:VE_XML::VEXMLObject(rootDoc)
{
   _name = name;
   _parent = 0;
   _transform = new VE_XML::VETransform(_rootDocument); 
   _material = new VE_CAD::CADMaterial(_rootDocument); 
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
   if(_material){
      delete _material;
      _material = 0;
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
void CADNode::SetTransform(VE_XML::VETransform* transform)
{
   if(_transform)
   {
      delete _transform;
      _transform = 0;
   }
   _transform = new VE_XML::VETransform(*transform);
}
////////////////////////////////////////////////////////
void CADNode::SetMaterial(VE_CAD::CADMaterial* material)
{
   if(_material)
   {
      delete _material;
      _material = 0;
   }
   _material = new CADMaterial(*material);
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
VE_XML::VETransform* CADNode::GetTransform()
{
   return _transform;
}
///////////////////////////////////////////
VE_CAD::CADMaterial* CADNode::GetMaterial()
{
   return _material;
}
/////////////////////////////////////////////////
void CADNode::_updateVEElement(std::string input)
{
   //how is this going to work???
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString("CADNode"));
   }

   _updateNodeName();
   
   _veElement->appendChild( _parent->GetXMLData("parent") );
   _veElement->appendChild( _material->GetXMLData("material") );
   _veElement->appendChild( _transform->GetXMLData("transform") );
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
            if(_parent)
               _parent->SetObjectFromXMLData(parentNode);

            DOMElement* materialNode = GetSubElement(currentElement,std::string("material"),0);
            _material->SetObjectFromXMLData(materialNode);

            DOMElement* transformNode = GetSubElement(currentElement,std::string("transform"),0);
            _transform->SetObjectFromXMLData(transformNode);

         }
      }
   }
}
/////////////////////////////////////
CADNode::CADNode(const CADNode& rhs)
:VE_XML::VEXMLObject(rhs)
{
   _transform = new VE_XML::VETransform(*rhs._transform);
   _material = new VE_CAD::CADMaterial(*rhs._material);
   _parent = rhs._parent;
   _name = rhs._name;
}
////////////////////////////////////////////////
CADNode& CADNode::operator=(const CADNode& rhs)
{
   if ( this != &rhs )
   {
      if(_transform)
      {
         delete _transform;
         _transform = 0;
      }
      _transform = new VE_XML::VETransform(*rhs._transform);
      if(_material)
      {
         delete _material;
         _material = 0;
      }
      _material = new VE_CAD::CADMaterial(*rhs._material);
      _parent = rhs._parent;
      _name = rhs._name;
   }
   return *this;
}

