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
#include "VE_Open/XML/CAD/CADPart.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
////////////////////////////////////////////////////////////
//Constructor                                             //
////////////////////////////////////////////////////////////
CADPart::CADPart(std::string name)
:VE_CAD::CADNode(name)
{
   _cadFileName = std::string("CADFile");
   _type = std::string("Part");
  SetObjectType("CADPart");
}
///////////////////
//Destructor     //
///////////////////
CADPart::~CADPart()
{
}
/////////////////////////////////////////////////////
void CADPart::SetCADFileName(std::string cadFileName)
{
   _cadFileName = cadFileName;
}
/////////////////////////////////////
std::string CADPart::GetCADFileName()
{
   return _cadFileName;
}
//////////////////////////////////
void CADPart::_updateCADFileName()
{
   DOMElement* nameElement  = _rootDocument->createElement( xercesString("fileName") );
   _veElement->appendChild( nameElement );      
   
   DOMText* fileName = _rootDocument->createTextNode( xercesString( _cadFileName ) );
   nameElement->appendChild( fileName  );
}
/////////////////////////////////////////////////
void CADPart::_updateVEElement(std::string input)
{
   //How is this going to work???
   //Get the base elements from CADNode
   VE_CAD::CADNode::_updateVEElement(input);

   _updateCADFileName();
}
/////////////////////////////////////////////////////
void CADPart::SetObjectFromXMLData( DOMNode* xmlNode)
{
   DOMElement* currentElement = 0;

   if(xmlNode->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(xmlNode);
   }
   
   if(currentElement)
   {
      //populate the base elements in node
      VE_CAD::CADNode::SetObjectFromXMLData(currentElement);

      //break down the element
      {
         if(currentElement->hasChildNodes())
         {
            DOMElement* fileNameElement = GetSubElement(currentElement,std::string("fileName"),0);
            _cadFileName = ExtractDataStringFromSimpleElement(fileNameElement);
         }
      }
   }
}
/////////////////////////////////////
CADPart::CADPart(const CADPart& rhs)
:VE_CAD::CADNode(rhs)
{
   _cadFileName = rhs._cadFileName;
}
////////////////////////////////////////////////
CADPart& CADPart::operator=(const CADPart& rhs)
{
   if ( this != &rhs )
   {
      VE_CAD::CADNode::operator =(rhs);
      _cadFileName = rhs._cadFileName;
   }
   return *this;
}

