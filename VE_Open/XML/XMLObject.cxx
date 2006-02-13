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
 * File:          $RCSfile: XMLObject.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/XMLCreator.h"
#include "VE_Open/XML/XMLObjectFactory.h"
#include <sstream>
#include <iomanip>
XERCES_CPP_NAMESPACE_USE

using namespace VE_XML;
//////////////////////
XMLObject::XMLObject()
{
   _veElement = 0;
   _needsUpdate = false;
   _rootDocument = 0;
   _nChildren = 0;
   _objectType = std::string("XMLObject");
   _objectNamespace = std::string("XML");

   //This may need to be somewhere else
   /*if(!XMLObjectFactory::Instance()->ObjectCreatorIsRegistered("XML"))
   {
      XMLObjectFactory::Instance()->RegisterObjectCreator("XML",new XMLCreator());
   }*/
}
///////////////////////////////////////////////////
XMLObject::XMLObject( const XMLObject& input )
{
   _veElement = input._veElement;
   _needsUpdate = input._needsUpdate;
   _rootDocument = input._rootDocument; 
   _nChildren = input._nChildren;
   _objectType = input._objectType;
   _objectNamespace = input._objectNamespace;
}
//////////////////////////////////////////////////////////////
XMLObject& XMLObject::operator=( const XMLObject& input)
{
   if ( this != &input )
   {
      _veElement = input._veElement;
      _needsUpdate = input._needsUpdate;
      _rootDocument = input._rootDocument; 
      _nChildren = input._nChildren;
      _objectType = input._objectType;
      _objectNamespace = input._objectNamespace;
   }
   return *this;
}
////////////////////////////
XMLObject::~XMLObject()
{
}
//////////////////////////////////////////////////
void XMLObject::SetObjectNamespace(std::string tagname)
{
   _objectNamespace = tagname;
}
//////////////////////////////////////////////////
void XMLObject::SetObjectType(std::string tagName)
{
   _objectType = tagName;
}
/////////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetOwnerDocument(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* owner)
{
   _rootDocument = owner;
}
///////////////////////////////////////////
std::string XMLObject::GetObjectNamespace()
{
   return _objectNamespace;
}
/////////////////////////////////////////
std::string XMLObject::GetObjectType()
{
   return _objectType;
}
////////////////////////////////////////////////////////
DOMElement* XMLObject::GetXMLData( std::string input )
{
   if(_rootDocument)
   {
      //Make sure old data is cleared from the xerces side of the element
      _clearAllChildrenFromElement();
  
      

      //update the xerces element w/ the current data in the object
      //This function should be overridden in ALL derived classes!!!!!
      _updateVEElement( input );

      //SetSubElement("objectType",_objectType);
      //SetSubElement("objectNamespace",_objectNamespace);
      return _veElement;
   }
   else
   {
      std::cout<<"Root Document not set!!"<<std::endl;
      return 0;
   }
}
////////////////////////////////////////////////
void XMLObject::_clearAllChildrenFromElement()
{
   if ( _veElement )
   {
      _nChildren = _veElement->getChildNodes()->getLength();
      for(int i = _nChildren - 1; i > -1; i--)
      {
         _veElement->removeChild(_veElement->getChildNodes()->item(i));
      }
   }
}
////////////////////////////////////////////////////////////////
XMLObject::VEStr::VEStr(const char* const toTranscode)
{
   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode(toTranscode);
}
//////////////////////////////////////////////
XMLObject::VEStr::VEStr( std::string input )
{
   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode( input.c_str() );
}
//////////////////////////////////////
XMLObject::VEStr::VEStr( int input )
{
   std::ostringstream dirStringStream;
   dirStringStream << std::setprecision(10) << input;

   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode( dirStringStream.str().c_str() );
}
//////////////////////////////////////
XMLObject::VEStr::VEStr( unsigned int input )
{
   std::ostringstream dirStringStream;
   dirStringStream << std::setprecision(10) << input;

   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode( dirStringStream.str().c_str() );
}
//////////////////////////////////////
XMLObject::VEStr::VEStr( long int input )
{
   std::ostringstream dirStringStream;
   dirStringStream << std::setprecision(10) << input;

   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode( dirStringStream.str().c_str() );
}
/////////////////////////////////////////
XMLObject::VEStr::VEStr( double input )
{
   std::ostringstream dirStringStream;
   dirStringStream << std::setprecision(10) << input;

   // Call the private transcoding method
   fUnicodeForm = XMLString::transcode(dirStringStream.str().c_str());
}
////////////////////////////
XMLObject::VEStr::~VEStr()
{
   XMLString::release(&fUnicodeForm);
}
////////////////////////////////////////////////////
const XMLCh* XMLObject::VEStr::unicodeForm() const
{
   return fUnicodeForm;
}
////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetSubElement( std::string subElementTagName, std::string dataValue )
{
   DOMElement* dataValueStringElement = _rootDocument->createElement( xercesString( subElementTagName ) );
   DOMText* dataValueString = _rootDocument->createTextNode( xercesString( dataValue ) );
   dataValueStringElement->appendChild( dataValueString );
   _veElement->appendChild( dataValueStringElement );
}
////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetSubElement( std::string subElementTagName, unsigned int dataValue )
{
   DOMElement* dataValueNumElement = _rootDocument->createElement( xercesString( subElementTagName ) );
   std::stringstream float2string;
   float2string << dataValue;
   DOMText* dataValueText = _rootDocument->createTextNode( xercesString( float2string.str().c_str() ) );

   dataValueNumElement->appendChild( dataValueText );
   _veElement->appendChild( dataValueNumElement );
}
////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetSubElement( std::string subElementTagName, long int dataValue )
{
   DOMElement* dataValueNumElement = _rootDocument->createElement( xercesString( subElementTagName ) );
   std::stringstream float2string;
   float2string << dataValue;
   DOMText* dataValueText = _rootDocument->createTextNode( xercesString( float2string.str().c_str() ) );

   dataValueNumElement->appendChild( dataValueText );
   _veElement->appendChild( dataValueNumElement );
}
////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetSubElement( std::string subElementTagName, double dataValue )
{
   DOMElement* dataValueNumElement = _rootDocument->createElement( xercesString( subElementTagName ) );
   std::stringstream float2string;
   float2string << dataValue;
   DOMText* dataValueText = _rootDocument->createTextNode( xercesString( float2string.str().c_str() ) );

   dataValueNumElement->appendChild( dataValueText );
   _veElement->appendChild( dataValueNumElement );
}
////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetSubElement( std::string subElementTagName, XMLObject* dataValue )
{
   dataValue->SetOwnerDocument( _rootDocument );
   _veElement->appendChild( dataValue->GetXMLData( subElementTagName ) );
}
////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetSubElement( std::string subElementTagName, XMLObject* dataValue,
                              std::string attribName, std::string attrib )
{
   dataValue->SetOwnerDocument( _rootDocument );
   DOMElement* xmlObjectElement = dataValue->GetXMLData( subElementTagName );
   xmlObjectElement->setAttribute( xercesString( attribName ), xercesString( attrib ) );
   _veElement->appendChild( xmlObjectElement );
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
DOMElement* XMLObject::GetSubElement(DOMElement* baseElement,std::string subElementTagName,unsigned int itemIndex)
{
   DOMElement* foundElement = dynamic_cast<DOMElement*>(baseElement->getElementsByTagName(xercesString(subElementTagName))->item(itemIndex));
   if(foundElement){
      if(foundElement->getParentNode() != baseElement)
      {
         XMLSize_t nChildren = baseElement->getElementsByTagName(xercesString(subElementTagName))->getLength();
         for(XMLSize_t i = 0; i < nChildren; i++)
         {
            foundElement = dynamic_cast<DOMElement*>(baseElement->getElementsByTagName(xercesString(subElementTagName))->item(i));
            if(foundElement->getParentNode() == baseElement)
               return foundElement;
         }   
      }
      else
      {
         return foundElement;
      }
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////////////
std::string XMLObject::ExtractDataStringFromSimpleElement(DOMElement* element)
{
   DOMText* rawText = dynamic_cast< DOMText* >( element->getFirstChild() );
   std::string tmp = XMLString::transcode( rawText->getData() );
   return tmp;
}
//////////////////////////////////////////////////////////////////////////
double XMLObject::ExtractDataNumberFromSimpleElement(DOMElement* element)
{
   DOMText* rawText = dynamic_cast< DOMText* >( element->getFirstChild() );
   std::string tmp = XMLString::transcode( rawText->getData() );
   return std::atof( tmp.c_str() );
}
//////////////////////////////////////////////////////////////////////////
unsigned int XMLObject::ExtractIntegerDataNumberFromSimpleElement(DOMElement* element)
{
   DOMText* rawText = dynamic_cast< DOMText* >( element->getFirstChild() );
   std::string tmp = XMLString::transcode( rawText->getData() );
   return std::atoi( tmp.c_str() );
}
//////////////////////////////////////////////////////////////////////////
long int XMLObject::ExtractLongIntegerDataNumberFromSimpleElement(DOMElement* element)
{
   DOMText* rawText = dynamic_cast< DOMText* >( element->getFirstChild() );
   std::string tmp = XMLString::transcode( rawText->getData() );
   return std::atoi( tmp.c_str() );
}
///////////////////////////////////////////
DOMDocument* XMLObject::GetRootDocument()
{
   return _rootDocument;
}
