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
 * File:          $RCSfile: CADClone.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Open/XML/CAD/CADClone.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
CADClone::CADClone(DOMDocument* rootDocument,std::string name,
                 VE_CAD::CADNode* originalNode)
:VE_CAD::CADNode(rootDocument,name)
{
   _originalNode = originalNode;
   _type = std::string("Clone");
}
/////////////////////
//Destructor       //
/////////////////////
CADClone::~CADClone()
{
   if(_originalNode)
   {
      delete _originalNode;
      _originalNode = 0;
   }
}
//////////////////////////////////////////////////
void CADClone::_updateVEElement(std::string input)
{
   //How is this going to work???
   //Get the base elements from CADNode
   VE_CAD::CADNode::_updateVEElement(input);

   //add the extra stuff
   _veElement->appendChild(_originalNode->GetXMLData("originalNode"));
   
}
///////////////////////////////////////////
VE_CAD::CADNode* CADClone::GetOriginalNode()
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
      VE_CAD::CADNode::SetObjectFromXMLData(xmlNode);

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
            if(ExtractDataStringFromSimpleElement(nodeType) == std::string("Assembly"))
            {
               //this is an Assembly
               _originalNode = new VE_CAD::CADAssembly(_rootDocument);
               
            }else if(ExtractDataStringFromSimpleElement(nodeType) == std::string("Part")){
               //this is a Part
               _originalNode = new VE_CAD::CADPart(_rootDocument);
            }
            _originalNode->SetObjectFromXMLData(originalNode);
         }
      }
   }
}
///////////////////////////////////////
CADClone::CADClone(const CADClone& rhs)
:VE_CAD::CADNode(rhs)
{
   _originalNode = rhs._originalNode;
}
///////////////////////////////////////////////////
CADClone& CADClone::operator=(const CADClone& rhs)
{
   if ( this != &rhs )
   {
      VE_CAD::CADNode::operator =(rhs);
      _originalNode = rhs._originalNode;
   }
   return *this;
}
