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

#ifndef _XML_VE_DATA_VALUE_PAIR_H_
#define _XML_VE_DATA_VALUE_PAIR_H_

#include <ves/open/xml/DataValuePairPtr.h>

/*!\file DataValuePair.h
  *Data value pairs API
  */

/*!\class VE_XML::DataValuePair
 *Class that manages data value pairs.
 *Pairs consist of a name and a value which can be of types:
 *float,float array,string and transform
 */
#include <string>
#include <vector>
#include <ves/open/xml/XMLObject.h>

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace ves
{
namespace open
{
namespace xml
{
class Transform;
class FloatArray;
class OneDDoubleArray;
class TwoDDoubleArray;
class ThreeDDoubleArray;
class OneDIntArray;
class TwoDIntArray;
class ThreeDIntArray;
class OneDStringArray;
//class TwoDStringArray;
//class ThreeDStringArray;
}
}
}

namespace ves
{
namespace open
{
namespace xml
{
class VE_XML_EXPORTS DataValuePair : public XMLObject
{
public:

    //valid values are
    //STRING == a string value
    //FLOAT == a single float value
    //FARRAY == a float array
    //TRANSFORM == a Transform
    //1DSTRING
    //1DDOUBLE
    //2DDOUBLE
    //3DDOUBLE
    //LONG
    //1DLONG
    //2DLONG
    //3DLONG
    //UNSIGNED INT
    ///Constructor
    ///\param type The type of value in this pair.
    DataValuePair( std::string type = std::string( "STRING" ) );
    ///Destructor
    virtual ~DataValuePair();
    ///Copy Constructor
    DataValuePair( const DataValuePair& );
    ///equal operator
    DataValuePair& operator= ( const DataValuePair& );

    /// Set the name
    ///\param name The name of this data value pair
    void SetDataName( std::string name );

    ///Set the data value type
    ///\param type Set the type of the data held.
    void SetDataType( std::string type );

    ///Set the string data
    ///\param data The string data.
    void SetDataString( std::string data );

    ///Set the float data
    ///\param data The float data.
    void SetDataValue( double data );

    ///set the Unsigned int data
    ///\param data The unsigned data.
    void SetDataValue( unsigned int data );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the unsigned data
    unsigned int GetUIntData();

    ///Get the data type.
    std::string GetDataType();
    ///Get the data name.
    std::string GetDataName();

    ///Get the string data.
    std::string GetDataString();

    ///Get the value of the double data.
    double GetDataValue();

    ///Get the xmlObject from the DataValuePair
    XMLObject* GetDataXMLObject();

    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data XMLObject being passed in. Can be a broad range of data types
    void SetData( std::string dataName, XMLObject* data );

    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data unsgined int being passed in.
    void SetData( std::string dataName, unsigned int data );

    ///String data
    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, std::string data );
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, std::vector< std::string > data );

    ///Double arrays
    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, double data );
    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, std::vector< double > data );
    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, std::vector< std::vector< double > > data );
    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, std::vector< std::vector< std::vector< double > > > data );

    ///Int arrays
    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, long data );
    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, std::vector< long > data );
    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, std::vector< std::vector< long > > data );
    ///Helper functions to set data easily
    ///\param dataName Name of the data being passed in
    ///\param data Data value being passed in. Can be a broad range of data types
    void SetData( std::string dataName, std::vector< std::vector< std::vector< long > > > data );

    ///String data
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( std::string& data );
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( std::vector< std::string >& data );

    ///Double arrays
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( double& data );
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( std::vector< double >& data );
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( std::vector< std::vector< double > > & data );
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( std::vector< std::vector< std::vector< double > > > & data );

    ///Int arrays
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( long& data );
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( std::vector< long >& data );
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( std::vector< std::vector< long > > & data );
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( std::vector< std::vector< std::vector< long > > > & data );

    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    void GetData( unsigned int& data );
    ///XMLObject
    ///Helper functions to get data easily
    ///\param data Name of the data being passed in
    //void GetData( VE_XML::XMLObject& data );

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( std::string tagName );

    ///Internally extract data of type "XMLOBJECT" from the DataValuePair
    ///In derived classes, this should be overridden
    ///\param baseElement The element to extract the XMLObject from.
    ///\param objectType The XMLObject::_objectType to extract
    virtual void _extractXMLObject( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement, std::string objectType );

    std::string _dataType;///<The data type.
    std::string _dataName;///<The data name.

    ///raw datatypes of DataValuePair that are specified in the verg.xsd file
    double _dataValue;///<Raw double value.
    long intDataValue;///<Raw long value.
    unsigned int _dataUInt;///<Raw unsigned int value
    std::string _dataString;///<Raw string value.

    XMLObject* _veXMLObject;///<Raw XMLObject.
};
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, DataValuePair* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif// _VE_DATA_VALUE_PAIR_H_
