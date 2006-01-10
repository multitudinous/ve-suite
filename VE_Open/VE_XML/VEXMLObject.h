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
 * File:          $RCSfile: VEXMLObject.h,v $
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
/*!\file VEXMLObject.h
  Base XML API
  */
/*!\class VE_XML::VEXMLObject
 * This class is the base class for representing
 * XML objects.
 */
/*!\namespace VE_XML
 * Contains nodes for creating/managing a XML Objects.
 */
XERCES_CPP_NAMESPACE_USE

namespace VE_XML
{
class VE_XML_EXPORTS VEXMLObject
{
public:
   VEXMLObject( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* );
   virtual ~VEXMLObject();
   VEXMLObject( const VEXMLObject& );
   //equal operator
   VEXMLObject& operator= ( const VEXMLObject& );
   
   ///Set the DOMDocument this object belongs to.
   void SetOwnerDocument(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* owner);

   virtual void SetObjectFromXMLData( DOMNode* )=0;
   DOMElement* GetXMLData( std::string ); 

   ///utility functions for reading data from an element
   ///\param element Element to extract string from.
   std::string ExtractDataStringFromSimpleElement( DOMElement* element );
   
   ///utility functions for reading data from an element
   ///\param element Element to extract double from.
   double ExtractDataNumberFromSimpleElement( DOMElement* element);
   
   ///utility functions for extracting subElement itemIndex from a complex element.
   ///\param baseElement The XML complexElement to extract a subelement from of type subElementTagName.
   ///\param subElementTagName The subelement tagname to extract from baseElement.
   ///\param itemIndex The index of the subElement to extract from the complex element.
   DOMElement* GetSubElement(DOMElement* baseElement,std::string subElementTagName,unsigned int itemIndex);
   // ---------------------------------------------------------------------------
   //  This is a simple class that lets us do easy (though not terribly efficient)
   //  trancoding of char* data to XMLCh data. --taken from xerces examples
   // ---------------------------------------------------------------------------
   class VE_XML_EXPORTS VEStr
   {
   public:
      VEStr( const char* const );
      VEStr( int );
      VEStr( double );
      VEStr( std::string );

      virtual ~VEStr();

      const XMLCh* unicodeForm( void ) const;

   private:
     // -----------------------------------------------------------------------
     //  Private data members
     //
     //  fUnicodeForm
     //      This is the Unicode XMLCh format of the string.
     // -----------------------------------------------------------------------
     XMLCh*   fUnicodeForm;
   };


protected:
   virtual void _updateVEElement( std::string )=0;
   void _clearAllChildrenFromElement();
   bool _needsUpdate;
   DOMElement* _veElement;
   XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* _rootDocument;
   unsigned int _nChildren;
};
#define xercesString(str) VE_XML::VEXMLObject::VEStr(str).unicodeForm()
}
#endif// _VE_XML_OBJECT_H_
