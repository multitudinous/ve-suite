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
 * File:          $RCSfile: VEXMLObject.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/VE_XML/VEXMLObject.h"
#include <sstream>
#include <iomanip>
XERCES_CPP_NAMESPACE_USE

using namespace VE_XML;
//////////////////////////
VEXMLObject::VEXMLObject(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDoc)
{
   _veElement = 0;
   _needsUpdate = false;
   _rootDocument = rootDoc; 
   _nChildren = 0;
}
///////////////////////////////////////////
VEXMLObject::VEXMLObject( const VEXMLObject& input )
{
   _veElement = input._veElement;
   _needsUpdate = input._needsUpdate;
   _rootDocument = input._rootDocument; 
   _nChildren = input._nChildren;
}
/////////////////////////////////////////////////////
VEXMLObject& VEXMLObject::operator=( const VEXMLObject& input)
{
   if ( this != &input )
   {
      _veElement = input._veElement;
      _needsUpdate = input._needsUpdate;
      _rootDocument = input._rootDocument; 
      _nChildren = input._nChildren;
   }
   return *this;
}
////////////////////////////
VEXMLObject::~VEXMLObject()
{
   if(_veElement)
   {
      _veElement->release();
   }
}
//////////////////////////////////////////////////////
void VEXMLObject::SetOwnerDocument(DOMDocument* owner)
{
   _rootDocument = owner;
}
/////////////////////////////////////
DOMElement* VEXMLObject::GetXMLData( std::string input )
{
   //Make sure old data is cleared from the xerces side of the element
   _clearAllChildrenFromElement();
  
   //update the xerces element w/ the current data in the object
   //This function should be overridden in ALL derived classes!!!!!
   _updateVEElement( input );

   return _veElement;
}
////////////////////////////////////////////////
void VEXMLObject::_clearAllChildrenFromElement()
{
   if ( _veElement )
   {
      _nChildren = _veElement->getChildNodes()->getLength();
      for(unsigned int i = _nChildren - 1; i > -1; i--)
      {
         _veElement->removeChild(_veElement->getChildNodes()->item(i));
      }
   }
}
///////////////////////////////////////////
VEXMLObject::VEStr::VEStr(const char* const toTranscode)
{
   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode(toTranscode);
}
///////////////////////////////////////////
VEXMLObject::VEStr::VEStr( std::string input )
{
   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode( input.c_str() );
}
///////////////////////////////////////////
VEXMLObject::VEStr::VEStr( int input )
{
   std::ostringstream dirStringStream;
   dirStringStream << std::setprecision(10) << input;

   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode( dirStringStream.str().c_str() );
}
/////////////////////////////////////////
VEXMLObject::VEStr::VEStr( double input )
{
   std::ostringstream dirStringStream;
   dirStringStream << std::setprecision(10) << input;

   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode(dirStringStream.str().c_str());
}
////////////////////////////
VEXMLObject::VEStr::~VEStr()
{
   XMLString::release(&fUnicodeForm);
}
/////////////////////////////////////////////////////////////////////////////////////////////
DOMElement* VEXMLObject::GetSubElement(DOMElement* baseElement,std::string subElementTagName,
                                  unsigned int itemNumber)
{
   return dynamic_cast<DOMElement*>(baseElement->getElementsByTagName(xercesString(subElementTagName))->item(itemNumber));
}
////////////////////////////////////////////////////
const XMLCh* VEXMLObject::VEStr::unicodeForm() const
{
   return fUnicodeForm;
}
///////////////////////////////////////////////////////////////////////////
std::string VEXMLObject::ExtractDataStringFromSimpleElement(DOMElement* element)
{
   DOMText* rawText = dynamic_cast< DOMText* >( element->getFirstChild() );
   std::string tmp = XMLString::transcode( rawText->getData() );
   return tmp;
}
//////////////////////////////////////////////////////////////////////////
double VEXMLObject::ExtractDataNumberFromSimpleElement(DOMElement* element)
{
   DOMText* rawText = dynamic_cast< DOMText* >( element->getFirstChild() );
   std::string tmp = XMLString::transcode( rawText->getData() );
   return atof( tmp.c_str() );
}
