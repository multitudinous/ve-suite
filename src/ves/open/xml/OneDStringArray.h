/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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

#ifndef _XML_VE_1DSTRING_ARRAY_H_
#define _XML_VE_1DSTRING_ARRAY_H_
/*!\file OneDStringArray.h
  Double Array API
  */
/*!\class VE_XML::OneDStringArray
 *This class basically manages a vector of doubles. 
 */
#include <vector>

#include "ves/open/xml/XMLObject.h"

#include <xercesc/dom/DOM.hpp>

namespace VE_XML
{
class VE_XML_EXPORTS OneDStringArray : public XMLObject
{
public:
   ///Constructor
   ///\param nElements The length of the float array.
   OneDStringArray(unsigned int nElements=3);
   ///Destructor
   virtual ~OneDStringArray();
   ///Copy Constructor
   OneDStringArray( const OneDStringArray& );
   ///equal operator
   OneDStringArray& operator= ( const OneDStringArray& );

   ///Add a new element to the end of this array
   ///\param newValue The new value to add.
   void AddElementToArray( std::string newValue);
   ///Set this array from an input vector
   ///\param newArrayValues The new values to set to this array.
   void SetArray( std::vector<std::string> newArrayValues);

   ///Get a specific element
   ///\param index The index of the element to return
   std::string GetElement( unsigned int index);

   ///Get the internal array.
   std::vector<std::string> GetArray( void );
   
   ///Populate the XMLObject data from an XML element.
   ///\param inputXML The input data.
   virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* inputXML ); 
   
protected:
   
   ///Internally update the data.
   ///\param tagName The tag name of this element.
   virtual void _updateVEElement( std::string tagName);
   unsigned int _nElements;///<Length of this float array.
   std::vector<std::string> _array;///<Raw data.

private:
   XMLSize_t minIndex;///<Mininum size of the array.
};
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(const std::string subElementTagName, OneDStringArray* val)
{
   val->SetOwnerDocument( _rootDocument );
   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
   _veElement->appendChild( childElement );
   return childElement;
}
}
#endif// _XML_VE_1DSTRING_ARRAY_H_
