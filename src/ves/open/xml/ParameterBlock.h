/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#ifndef _XML_VE_PARAMETER_BLOCK_H_
#define _XML_VE_PARAMETER_BLOCK_H_
/*!\file ParameterBlock.h
  Parameter Block Information API
  */
/*!\class VE_XML::ParameterBlock
 * This class manages a parameter block which is really a
 * list of DataValuePair s and an optional Transform
 */
#include <string>
#include <vector>

#include <ves/open/xml/XMLObject.h>
namespace ves
{
namespace open
{
namespace xml
{
class Transform;
class DataValuePair;
}
}
}

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace ves
{
namespace open
{
namespace xml
{
class VE_XML_EXPORTS ParameterBlock : public XMLObject
{
public:
    ///Constructor
    ///\param id The identification number of this parameter block
    ParameterBlock( unsigned int id = 0 );
    ///Destructor
    virtual ~ParameterBlock();
    ///Copy Constructor
    ParameterBlock( const ParameterBlock& );
    ///equal operator
    ParameterBlock& operator= ( const ParameterBlock& );

    ///Set the identification number.
    ///\param id The number specifiying what type of data is in this parameter block.
    void SetBlockId( unsigned int id );
    ///Set the name of the parameter block.
    ///\param name The name of the parameter block.
    void SetName( std::string name );

    ///Optional. Set the Transform.
    ///\param transform The Transform information. Commonly used with CFD datasets and CAD information.
    void SetTransform( Transform* transform );

    ///Add a property, which is held in a DataValuePair.
    ///\param prop The DataValuePair holding the information such as a CFD filename.
    void AddProperty( DataValuePair* prop );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Return the block ID.
    unsigned int GetBlockId( void );

    ///Return the paramter block.
    std::string GetName( void );
    ///Return the Transform.
    Transform* GetTransform();
    ///Return a DataValuePair based on a name.
    ///\param name The name of the DataValuePair to search for.
    DataValuePair* GetProperty( std::string name );
    ///Return the DataValuePair at the index.
    ///\param index The index of the DataValuePair.
    DataValuePair* GetProperty( int index );
    ///Return the number DataValuePair's.
    size_t GetNumberOfProperties( void );
    ///Remove the DataValuePair at the index.
    ///\param index The index of the DataValuePair.
    void RemoveProperty( unsigned int index );


protected:
    ///Internally update the XML data.
    ///\param tagName The tag name for this element
    virtual void _updateVEElement( std::string tagName );
    unsigned int _id;///<The block ID.
    Transform* _dcs;///<The optional Transform.
    std::vector<DataValuePair*> _properties;///<The DataValuePair list containing the block properties.
    std::string paramName;
};
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, ParameterBlock* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif// _XML_VE_PARAMETER_BLOCK_H_
