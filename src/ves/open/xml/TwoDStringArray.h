/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef _XML_VE_2DSTRING_ARRAY_H_
#define _XML_VE_2DSTRING_ARRAY_H_
/*!\file TwoDStringArray.h
  Double Array API
  */
/*!\class VE_XML::TwoDStringArray
 *This class basically manages a vector of strings.
 */
#include <vector>

#include <ves/open/xml/XMLObject.h>

#include <xercesc/dom/DOM.hpp>
#include <iostream>

#include <ves/open/xml/TwoDStringArrayPtr.h>
#include <ves/open/xml/OneDStringArrayPtr.h>

namespace ves
{
namespace open
{
namespace xml
{
class VE_XML_EXPORTS TwoDStringArray : public XMLObject
{
public:
    ///Constructor
    ///\param nElements The length of the float array.
    TwoDStringArray( unsigned int nElements = 3 );
    ///Destructor
    virtual ~TwoDStringArray();
    ///Copy Constructor
    TwoDStringArray( const TwoDStringArray& );
    ///equal operator
    TwoDStringArray& operator= ( const TwoDStringArray& );

    ///Add a new element to the end of this array
    ///\param newValue The new value to add.
    void AddElementToArray( std::vector< std::string > newValue );

    ///Add a new element to the end of this array
    ///\param newValue The new value to add.
    void AddElementToArray( OneDStringArrayPtr newValue );

    ///Set this array from an input vector
    ///\param newArrayValues The new values to set to this array.
    void SetArray( std::vector< std::vector< std::string > > newArrayValues );

    ///Get a specific element
    ///\param i The i'th index of the element to return
    ///\param j The j'th index of the element to return
    std::string GetElement( unsigned int i, unsigned int j );

    ///Get the internal array.
    std::vector< std::vector< std::string > > GetArray( void );

    ///Populate the XMLObject data from an XML element.
    ///\param inputXML The input data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* inputXML );

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( const std::string& tagName );
    unsigned int mNElements;///<Length of this float array.
    std::vector< OneDStringArrayPtr > mOneDArray;///<Raw data.

private:
    XMLSize_t mMinIndex;///<Mininum size of the array.
};

}
}
}
#endif// _XML_VE_2DSTRING_ARRAY_H_
