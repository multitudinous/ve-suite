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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _VE_XML_OBJECT_H_
#define _VE_XML_OBJECT_H_

#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

#include <iostream>
#include <string>

#include "VE_Installer/include/VEConfig.h"
/*!\file XMLObject.h
  Base XML API
  */
/*!\class VE_XML::XMLObject
 * This class is the base class for representing
 * XML objects.
 */
/*!\namespace VE_XML
 * Contains nodes for creating/managing a XML Objects.
 */
XERCES_CPP_NAMESPACE_USE

namespace VE_XML
{
class VE_XML_EXPORTS XMLObject
{
public:
   ///Base constructor
   XMLObject( );
   ///Destructor
   virtual ~XMLObject();
   ///Copy Construstor
   XMLObject( const XMLObject& );
   ///equal operator
   XMLObject& operator= ( const XMLObject& );

protected:
   ///Set the XMLObject type
   ///\param veObjectType
   void SetObjectType(std::string veObjectType);

   ///Set the XMLObject Namespace
   ///\param veObjectNamespace
   void SetObjectNamespace(std::string veObjectNamespace);

public:   
   ///Set the DOMDocument this object belongs to.
   void SetOwnerDocument( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* owner);

   ///Populate the XMLObject data from an XML element.
   virtual void SetObjectFromXMLData( DOMNode* xmlInput )=0;

   ///Return the object type. This should be set in the constructor of all derived classes
   std::string GetObjectType();
   
   ///Return the object namespace. This should be set in the constructor of all derived classes
   std::string GetObjectNamespace();

   ///Get an XML element from the current data in the string.
   DOMElement* GetXMLData( std::string tagName); 

   ///Return the root document of this element.
   XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* GetRootDocument();

   ///utility functions for reading data from an element
   ///\param element Element to extract string from.
   bool ExtractBooleanFromSimpleElement( DOMElement* element );

   ///utility functions for reading data from an element
   ///\param element Element to extract string from.
   std::string ExtractDataStringFromSimpleElement( DOMElement* element );
   
   ///utility functions for reading data from an element
   ///\param element Element to extract double from.
   double ExtractDataNumberFromSimpleElement( DOMElement* element);
   
   ///utility functions for reading data from an element
   ///\param element Element to extract unsigned integer from.
   unsigned int ExtractIntegerDataNumberFromSimpleElement( DOMElement* element);

   ///utility functions for reading data from an element
   ///\param element Element to extract long integer from.
   long int ExtractLongIntegerDataNumberFromSimpleElement( DOMElement* element);

   ///utility functions for extracting subElement itemIndex from a complex element.
   ///\param baseElement The XML complexElement to extract a subelement from of type subElementTagName.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param itemIndex The index of the subElement to extract from the complex element.
   DOMElement* GetSubElement(DOMElement* baseElement,std::string subElementTagName,unsigned int itemIndex);

   ///utility functions for creating subElements for _veElement.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param dataValue The data to be stored.
   void SetSubElement( std::string subElementTagName, bool dataValue );

   ///utility functions for creating subElements for _veElement.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param dataValue The data to be stored.
   void SetSubElement( std::string subElementTagName, std::string dataValue );
   ///utility functions for creating subElements for _veElement.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param dataValue The data to be stored.
   void SetSubElement( std::string subElementTagName, unsigned int dataValue );
   ///utility functions for creating subElements for _veElement.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param dataValue The data to be stored.
   void SetSubElement( std::string subElementTagName, long int dataValue );
   ///utility functions for creating subElements for _veElement.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param dataValue The data to be stored.
   void SetSubElement( std::string subElementTagName, double dataValue );
   ///utility functions for creating subElements for _veElement.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param dataValue The data to be stored.
   void SetSubElement( std::string subElementTagName, XMLObject* dataValue );
   ///utility functions for creating subElements for _veElement.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param dataValue The data to be stored.
   ///\param attribName The name of the atrribute to be set
   ///\param attrib The attribute value
   void SetSubElement( std::string subElementTagName, XMLObject* dataValue, 
                        std::string attribName, std::string attrib );


   class VE_XML_EXPORTS VEStr
   {
   public:
      ///Constructor
      ///\param toTranscode The input to translate.
      VEStr( const char* const toTranscode);
      ///Constructor
      ///\param input The input to translate.
      VEStr( int input);
      ///Constructor
      ///\param input The input to translate.
      VEStr( unsigned int input);
      ///Constructor
      ///\param input The input to translate.
      VEStr( long int input);
      ///Constructor
      ///\param input The input to translate.
      VEStr( double input);
      ///Constructor
      ///\param input The input to translate.
      VEStr( std::string input);

      ///Destructor
      virtual ~VEStr();

      ///Get the char for the string.
      const XMLCh* unicodeForm( void ) const;

   private:
     // -----------------------------------------------------------------------
     //  Private data members
     //
     //  fUnicodeForm
     //      This is the Unicode XMLCh format of the string.
     // -----------------------------------------------------------------------
     XMLCh*   fUnicodeForm;///< The raw unicode string.
   };


protected:
   ///Internally update the XML data.
   virtual void _updateVEElement( std::string )=0;
  
   ///Clear all the children from the element.
   void _clearAllChildrenFromElement();
   bool _needsUpdate;///<Determines whether the internal data has changed.
   DOMElement* _veElement;///<The XML element.
   XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* _rootDocument;///<The owning document for this element.
   unsigned int _nChildren;///<The number of childern for this element.
   std::string _objectType;///<The type of object;
   std::string _objectNamespace;///<The namespace for this object;
};
///Utility function to convert strings to Xerces compatible strings
#define xercesString(str) VE_XML::XMLObject::VEStr(str).unicodeForm()
}
#endif// _VE_XML_OBJECT_H_
