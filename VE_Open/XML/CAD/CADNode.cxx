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

#include <apr_uuid.h>
using namespace VE_XML::VE_CAD;
using namespace VE_XML::VE_Shader;
using namespace VE_XML;

//////////////////////////////////
///Constructor                  //
//////////////////////////////////
CADNode::CADNode(std::string name)
:VE_XML::XMLObject()
{
   m_name = name;
   m_parent = "";
   m_transform = new Transform(); 
   m_type = std::string("Node");
   m_visibility = true;
   //_uID = std::atoi(uuid.c_str());//static_cast<unsigned int>(time(NULL));
   m_activeAttributeName = std::string("");
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
   if(m_transform)
   {
      delete m_transform;
      m_transform = 0;
   }
   m_attributeList.clear();
   m_animations.clear();
}
//////////////////////////////////////////////////////////////////////////
void CADNode::AddAnimation(std::string name,std::string animationFileName)
{
   CADNodeAnimation newAnimation;
   newAnimation.SetAnimationFileName(animationFileName);
   newAnimation.SetAnimationName(name);
   m_animations.push_back(newAnimation);
}
///////////////////////////////////////////
void CADNode::SetNodeName(std::string name)
{
   m_name = name;
}
////////////////////////////////////////////////////
void CADNode::SetParent(std::string parent)
{
   m_parent = parent;
}
///////////////////////////////////////////////////////
void CADNode::SetTransform(VE_XML::Transform* transform)
{
   if(m_transform)
   {
      delete m_transform;
      m_transform = 0;
   }
   m_transform = new VE_XML::Transform(*transform);
}
///////////////////////////////////////////////////////////
void CADNode::AddAttribute(VE_XML::VE_CAD::CADAttribute attribute)
{
   m_attributeList.push_back(attribute);
}
////////////////////////////////////////////////////////
void CADNode::RemoveAttribute(std::string attributeName)
{
   for ( std::vector<CADAttribute>::iterator itr = m_attributeList.begin();
                                    itr != m_attributeList.end();
                                    itr++ )
   {
      if((*itr).GetAttributeName() == attributeName)
      {
         m_attributeList.erase(itr);
         break;
      }
   }
}
///////////////////////////////////////////////////////////
void CADNode::SetActiveAttribute(std::string attributeName)
{
   m_activeAttributeName = attributeName;
}
////////////////////////////
bool CADNode::HasAnimation()
{
   return (!m_animations.empty());
}
//////////////////////////////////
std::string CADNode::GetNodeType()
{
   return m_type;
}
//////////////////////////////////
std::string CADNode::GetNodeName()
{
   return m_name;
}
/////////////////////////////////////////
std::string CADNode::GetParent()
{
   return m_parent;
}
//////////////////////////////////////////
VE_XML::Transform* CADNode::GetTransform()
{
   return m_transform;
}
///////////////////////////////////////////////////////////////////////
VE_XML::VE_CAD::CADAttribute& CADNode::GetAttribute(unsigned int index)
{
   try
   {
      return m_attributeList.at(index);
   }
   catch(...)
   {
      std::cout<<"ERROR!!!!!"<<std::endl;
      std::cout<<"Invalid index!!!"<<std::endl;
      std::cout<<"CADNode::GetAttribute(): "<<index<<std::endl;
      return m_attributeList.at(0);;
   }
   return m_attributeList.at(0);;
}
/////////////////////////////////////////////////////////////////////
VE_XML::VE_CAD::CADAttribute& CADNode::GetAttribute(std::string name)
{
   size_t nAttributes = m_attributeList.size();
   for(size_t i = 0; i < nAttributes; i++)
   {
      if(m_attributeList.at(i).GetAttributeName() == name)
      {
         return m_attributeList.at(i);
      }
   }
}
///////////////////////////////////////////////////////////
VE_XML::VE_CAD::CADAttribute& CADNode::GetActiveAttribute()
{
   return GetAttribute(m_activeAttributeName);
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

   SetAttribute("id",uuid);
   SetAttribute("visibility",m_visibility);
   SetSubElement(std::string("parent"),m_parent);

   if(!m_transform)
   {
      m_transform = new Transform();
   }
   m_transform->SetOwnerDocument(_rootDocument);
   _veElement->appendChild( m_transform->GetXMLData("transform") );

   if(m_attributeList.size())
   {
      size_t nAttributes = m_attributeList.size();
      for(size_t i = 0; i < nAttributes; i++)
      {
         m_attributeList.at(i).SetOwnerDocument(_rootDocument);
         _veElement->appendChild( m_attributeList.at(i).GetXMLData("attribute") );
      }
      SetSubElement(std::string("activeAttributeName"),m_activeAttributeName);
   }

   if(m_animations.size())
   {
      size_t nAnimations = m_animations.size();
      for(size_t i = 0; i < nAnimations; i++)
      {
         m_animations.at(i).SetOwnerDocument(_rootDocument);
         _veElement->appendChild( m_animations.at(i).GetXMLData("animation") );
      }
   }
}
///////////////////////////////
void CADNode::_updateNodeName()
{
   DOMElement* nodeNameElement = _rootDocument->createElement(xercesString("name"));
   DOMText* nodeName = _rootDocument->createTextNode(xercesString(m_name.c_str()));
   nodeNameElement->appendChild(nodeName);
   _veElement->appendChild(nodeNameElement);
}
////////////////////////////////////////////
void CADNode::_updateNodeType()
{
   DOMElement* nodeTypeElement = _rootDocument->createElement(xercesString("type"));
   DOMText* nodeType = _rootDocument->createTextNode(xercesString(m_type));
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
            if(currentElement->getAttributeNode(xercesString("visibility")))
            {
               dynamic_cast<VE_XML::XMLObject*>(this)->GetAttribute(currentElement,
                                                                                      "visibility",
                                                                                      m_visibility);
            }
            else
            {
               m_visibility = true;
            }
            //Is there a better way to do this
            DOMElement* nameNode = GetSubElement(currentElement,std::string("name"),0);
            if(nameNode)
            {
              m_name = ExtractFromSimpleElement< std::string >( nameNode );
            }
            
            DOMElement* idNode = GetSubElement(currentElement,std::string("nodeID"),0);
            if(idNode)
            {
               VE_XML::XMLObject::SetID(ExtractFromSimpleElement< unsigned int >(idNode) );
            }
            else
            {
               VE_XML::XMLObject::GetAttribute(currentElement, "id",uuid);
            }
            DOMElement* typeNode = GetSubElement(currentElement,std::string("type"),0);
            if(typeNode)
            {
              m_type = ExtractFromSimpleElement< std::string >( typeNode );
            }
            DOMElement* parentNode = GetSubElement(currentElement,std::string("parent"),0);
            if(parentNode)
            {
               m_parent = ExtractFromSimpleElement< std::string >(parentNode);
            }
            size_t nOldAttributes = m_attributeList.size();
            if(nOldAttributes > 0)
            {
               m_attributeList.clear();
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
                  m_attributeList.push_back(newAttribute);
               }
            }

            m_animations.clear();
            DOMNodeList* animationNodes = currentElement->getElementsByTagName(xercesString("animation"));
            XMLSize_t nNewAnimations = animationNodes->getLength();
            for(XMLSize_t  i = 0; i < nNewAnimations ; i++)
            {
               DOMElement* animationNode = dynamic_cast<DOMElement*>(animationNodes->item(i));
               
               if(animationNode->getParentNode() == currentElement)
               {
                  CADNodeAnimation newAnimation;
                  newAnimation.SetObjectFromXMLData(animationNode);
                  m_animations.push_back(newAnimation);
               }
            }

            
            DOMElement* activeAttribNode = GetSubElement(currentElement,std::string("activeAttributeName"),0);
            if(activeAttribNode)
            {
               m_activeAttributeName = ExtractFromSimpleElement< std::string >(activeAttribNode);
               SetActiveAttribute(m_activeAttributeName);
            }

            
            DOMElement* transformNode = GetSubElement(currentElement,std::string("transform"),0);
            if(transformNode)
            {
               if(!m_transform)
               {
                  m_transform = new Transform();
               }
               m_transform->SetObjectFromXMLData(transformNode);
            }
            
         }
      }
   }
}
///////////////////////////////////////
void CADNode::SetVisibility(bool onOff)
{
   m_visibility = onOff;
}
/////////////////////////////
bool CADNode::GetVisibility()
{
   return m_visibility;
}
/////////////////////////////////////////////////////
std::vector<CADAttribute> CADNode::GetAttributeList()
{
   return m_attributeList;
}
///////////////////////////////////////////////////////////////////
VE_XML::VE_CAD::CADNodeAnimation& CADNode::GetAnimation(unsigned int index)
{
   try
   {
      return m_animations.at(index);
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
   size_t nAnimations = m_animations.size();
   for(size_t i = 0; i < nAnimations; i++)
   {
      if(m_animations.at(i).GetAnimationName() == name)
      {
         return m_animations.at(i);
      }
   }
}
///////////////////////////////////////
size_t CADNode::GetNumberOfAnimations()
{
   return m_animations.size();
}
///////////////////////////////////////////////
CADNode::CADNode(const CADNode& rhs,bool clone)
:VE_XML::XMLObject(rhs)
{
   m_parent = "";
   m_transform = 0;;

   if(rhs.m_transform)
   {
      m_transform = new VE_XML::Transform(*rhs.m_transform);
   }
   else
   {
      m_transform = new Transform();
   }

   if(m_attributeList.size())
   {
      m_attributeList.clear();
   }
   for(size_t i = 0; i < rhs.m_attributeList.size(); i++)
   {
      m_attributeList.push_back(rhs.m_attributeList.at(i));
   }
   for(size_t i = 0; i < rhs.m_animations.size(); i++)
   {
      m_animations.push_back(rhs.m_animations.at(i));
   }
   m_activeAttributeName = rhs.m_activeAttributeName;
   m_parent = rhs.m_parent;
   m_name = rhs.m_name;
   m_type = rhs.m_type;
   m_visibility = rhs.m_visibility;

   //maintain a unique ID
   if(clone)
   {   
      apr_uuid_t tempUUID;
      apr_uuid_get( &tempUUID );
      char* buffer = new char[ APR_UUID_FORMATTED_LENGTH + 1 ];
      apr_uuid_format( buffer, &tempUUID );
      uuid.assign( buffer );
      delete [] buffer;
   }
}
////////////////////////////////////////////////
CADNode& CADNode::operator=(const CADNode& rhs)
{
   //std::cout<<"CADNode operator= "<<std::endl;
   //std::cout<<"rhs: "<<rhs._uID<<std::endl;
   if ( this != &rhs )
   {
      XMLObject::operator =(rhs);
      if(m_attributeList.size())
      {
         m_attributeList.clear();
      }

      for(size_t i = 0; i < rhs.m_attributeList.size(); i++)
      {
         m_attributeList.push_back(rhs.m_attributeList.at(i));
      }

      if(m_animations.size())
      {
         m_animations.clear();
      }
      for(size_t i = 0; i < rhs.m_animations.size(); i++)
      {
         m_animations.push_back(rhs.m_animations.at(i));
      }
     
      if(m_transform)
      {
         delete m_transform;
         m_transform = 0;
      }
      m_transform = new Transform(*rhs.m_transform);
      m_activeAttributeName = rhs.m_activeAttributeName;
      m_visibility = rhs.m_visibility;
      //_uID = rhs._uID;
      m_parent = rhs.m_parent;
      m_name = rhs.m_name;
   }
   return *this;
}

