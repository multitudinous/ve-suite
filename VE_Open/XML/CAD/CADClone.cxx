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
#include "VE_Open/XML/CAD/CADClone.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_XML::VE_CAD;
/////////////////////////////////////////////////////////////////////////////////////////////
//Constructor                                                                              //
/////////////////////////////////////////////////////////////////////////////////////////////
CADClone::CADClone(std::string name,CADNode* originalNode)
:VE_XML::VE_CAD::CADNode(name)
{
   _originalNode = 0;
   
   if(originalNode)
   {
      SetOriginalNode(originalNode);
   }

   m_type = std::string("Clone");
   SetObjectType("CADClone");
}
/////////////////////
//Destructor       //
/////////////////////
CADClone::~CADClone()
{
  
}
/////////////////////////////////////////////////////
void CADClone::SetOriginalNode(CADNode* originalNode)
{
   if(!_originalNode)
   {
      if(originalNode->GetNodeType() == std::string("Assembly"))
      {
         _originalNode = new CADAssembly(*dynamic_cast<CADAssembly*>(originalNode));
      }
      else
      {
         _originalNode = new CADPart(*dynamic_cast<CADPart*>(originalNode));
      }
   }
   else
   {
      _originalNode = originalNode;
   }
}
//////////////////////////////////////////////////
void CADClone::_updateVEElement(std::string input)
{
   
   //Get the base elements from CADNode
   VE_XML::VE_CAD::CADNode::_updateVEElement(input);
   
   //add the extra stuff
   _originalNode->SetOwnerDocument(_rootDocument);
   _veElement->appendChild(_originalNode->GetXMLData("originalNode"));

}
///////////////////////////////////////////
VE_XML::VE_CAD::CADNode* CADClone::GetOriginalNode()
{
   return _originalNode;
}
/////////////////////////////////////////////////////
void CADClone::SetObjectFromXMLData( DOMNode* xmlNode)
{
   
   DOMElement* currentElement = 0;

   if(xmlNode->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(xmlNode);
   }
   
   if(currentElement)
   {
      //populate the base elements in node
      VE_XML::VE_CAD::CADNode::SetObjectFromXMLData(xmlNode);

      //break down the element
      {
         if(currentElement->hasChildNodes())
         {
            DOMElement* originalNode = GetSubElement(currentElement,std::string("originalNode"),0);
            DOMElement* nodeType = GetSubElement(originalNode,std::string("type"),0);
            if(_originalNode)
            {
               delete _originalNode;
               _originalNode = 0;
            }
            if(ExtractFromSimpleElement< std::string >(nodeType) == std::string("Assembly"))
            {
               //this is an Assembly
               _originalNode = new VE_XML::VE_CAD::CADAssembly();
               
            }else if(ExtractFromSimpleElement< std::string >(nodeType) == std::string("Part")){
               //this is a Part
               _originalNode = new VE_XML::VE_CAD::CADPart();
            }
            _originalNode->SetObjectFromXMLData(originalNode);
         }
      }
   }
}
///////////////////////////////////////
CADClone::CADClone(const CADClone& rhs)
:VE_XML::VE_CAD::CADNode(rhs)
{
   if(rhs._originalNode)
   {
      if(rhs._originalNode->GetNodeType() == std::string("Assembly"))
      {
         _originalNode = new CADAssembly(*dynamic_cast<CADAssembly*>(rhs._originalNode));
      }
      else
      {
         _originalNode = new CADPart(*dynamic_cast<CADPart*>(rhs._originalNode));
      }
   }
   else
   {
      _originalNode = 0;
   }
}
///////////////////////////////////////////////////
CADClone& CADClone::operator=(const CADClone& rhs)
{
   if ( this != &rhs )
   {
      VE_XML::VE_CAD::CADNode::operator =(rhs);
      if(rhs._originalNode)
      {
         if(rhs._originalNode->GetNodeType() == std::string("Assembly"))
         {
            _originalNode = new CADAssembly(*dynamic_cast<CADAssembly*>(rhs._originalNode));
         }
         else
         {
            _originalNode = new CADPart(*dynamic_cast<CADPart*>(rhs._originalNode));
         }
      }
      else
      {
         _originalNode = 0;
      }
   }
   return *this;
}
