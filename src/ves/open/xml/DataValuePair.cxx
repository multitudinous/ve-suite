/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/OneDDoubleArray.h>
#include <ves/open/xml/TwoDDoubleArray.h>
#include <ves/open/xml/ThreeDDoubleArray.h>
#include <ves/open/xml/OneDIntArray.h>
#include <ves/open/xml/TwoDIntArray.h>
#include <ves/open/xml/ThreeDIntArray.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/open/xml/XMLObjectFactory.h>

#include <iostream>
#include <sstream>
XERCES_CPP_NAMESPACE_USE

using namespace ves::open::xml;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
DataValuePair::DataValuePair( std::string type )
        : XMLObject()
{
    _dataType = type;
    _dataName = '\0';

    _dataValue = 0;
    _dataString = '\0';
    _dataUInt = 0;
    intDataValue = 0;

    _veXMLObject = 0;
    SetObjectType( "DataValuePair" );

}
///////////////////////////////////
DataValuePair::~DataValuePair()
{
    if( _veXMLObject )
    {
        delete _veXMLObject;
        _veXMLObject = 0;
    }
}
//////////////////////////////////////////////////////////
DataValuePair::DataValuePair( const DataValuePair& input )
        : XMLObject( input )
{
    _dataUInt = input._dataUInt;
    _dataType = input._dataType;
    _dataName = input._dataName;
    intDataValue = input.intDataValue;

    _dataValue = input._dataValue;
    _dataString = input._dataString;

    _veXMLObject = 0;

    if( input._veXMLObject )
    {
        _veXMLObject = XMLObjectFactory::Instance()->CreateXMLObjectCopy( input._veXMLObject );
    }
}
/////////////////////////////////////////////////////
DataValuePair& DataValuePair::operator=( const DataValuePair& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        _dataType = input._dataType;
        _dataName = input._dataName;
        _dataUInt = input._dataUInt;
        intDataValue = input.intDataValue;
        _dataValue = input._dataValue;
        _dataString = input._dataString;

        if( _veXMLObject )
        {
            delete _veXMLObject;
            _veXMLObject = 0;
        }

        if( input._veXMLObject )
        {
            _veXMLObject = XMLObjectFactory::Instance()->CreateXMLObjectCopy( input._veXMLObject );
        }
    }
    return *this;
}
/////////////////////////////////////////
unsigned int DataValuePair::GetUIntData()
{
    return _dataUInt;
}
/////////////////////////////////////////////////////
void DataValuePair::SetDataValue( unsigned int data )
{
    if( _dataType != std::string( "UNSIGNED INT" ) )
    {
        std::cout << "Invalid type passed into DataValuePair::SetDataString" << std::endl;
        return;
    }
    _dataUInt = data;

}
/////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, unsigned int data )
{
    _dataName = dataName;
    SetDataType( std::string( "UNSIGNED INT" ) );
    _dataUInt = data;
}
////////////////////////////////////////////
void DataValuePair::SetDataType( std::string type )
{
    if (( type == std::string( "STRING" ) ) ||
            ( type == std::string( "UNSIGNED INT" ) ) ||
            ( type == std::string( "FLOAT" ) )  ||
            ( type == std::string( "FARRAY" ) ) ||
            ( type == std::string( "LONG" ) ) ||
            ( type == std::string( "XMLOBJECT" ) ) )
    {
        _dataType = type;
    }
    else
    {
        std::cout << "Invalid type specified in DataValuePair::SetDataType: " << std::endl;
        std::cout << type << std::endl;
        std::cout << "Valid types are:" << std::endl
        << "STRING == a string value. " << std::endl
        << "FLOAT == a single float value. " << std::endl
        << "XMLOBJECT == an XMLObject" << std::endl;
    }
}
/////////////////////////////////////////////////////
void DataValuePair::SetDataString( std::string data )
{
    if( _dataType != std::string( "STRING" ) )
    {
        std::cout << "Invalid type passed into DataValuePair::SetDataString" << std::endl;
        return;
    }
    if( !data.empty() )
    {
        _dataString = data;
    }
}
///////////////////////////////////////////////
void DataValuePair::SetDataValue( double data )
{
    if( _dataType != std::string( "FLOAT" ) )
    {
        std::cout << "Invalid type passed into DataValuePair::SetCommandDataTransform" << std::endl;
        return;
    }
    _dataValue = data;
}
////////////////////////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, XMLObject* vexmlObject )
{
    if( vexmlObject )
    {
        if( _veXMLObject )
        {
            delete _veXMLObject;
            _veXMLObject = 0;
        }
        _veXMLObject = XMLObjectFactory::Instance()->CreateXMLObjectCopy( vexmlObject );

        _dataName = dataName;

        SetDataType( std::string( "XMLOBJECT" ) );
    }
    else
    {
        std::cout << "Invalid type passed into DataValuePair::SetData(std::string dataName,XMLObject* vexmlObject)" << std::endl;
    }
}
///////////////////////////////////////
void DataValuePair::_updateVEElement( std::string input )
{
    //fill in datavalue pairs, via xerces, according to schema
    //Add code here to update the specific sub elements
    SetAttribute( "dataName", _dataName );
    SetAttribute( "id", uuid );

    //update the value held in the pair
    if( _dataType == std::string( "FLOAT" ) )
    {
        SetSubElement( "dataValue", _dataValue );
    }
    else if( _dataType == std::string( "UNSIGNED INT" ) )
    {
        SetSubElement( "dataValue", _dataUInt );
    }
    else if( _dataType == std::string( "LONG" ) )
    {
        SetSubElement( "dataValue", intDataValue );
    }
    else if( _dataType == std::string( "STRING" ) )
    {
        SetSubElement( "dataValue", _dataString );
    }
    else if( _dataType == std::string( "XMLOBJECT" ) )
    {
        DOMElement* genericElement = SetSubElement( "genericObject" , _veXMLObject );
        SetAttribute( "objectType", _veXMLObject->GetObjectType(), genericElement );
    }
}
///////////////////////////////////////////////////
void DataValuePair::SetDataName( std::string name )
{
    if( !name.empty() )
    {
        _dataName = name;
    }
}
//////////////////////////////////////////
std::string DataValuePair::GetDataName()
{
    return _dataName;
}
//////////////////////////////////////////
std::string DataValuePair::GetDataType()
{
    return _dataType;
}
////////////////////////////////////////////
std::string DataValuePair::GetDataString()
{
    if( _dataType == std::string( "STRING" ) )
    {
        return _dataString;
    }
    return 0;
}
/////////////////////////////////////
double DataValuePair::GetDataValue()
{
    if( _dataType == std::string( "FLOAT" ) )
    {
        return _dataValue;
    }
    return 0;
}
////////////////////////////////////////////////////
XMLObject* DataValuePair::GetDataXMLObject()
{
    if( _dataType == std::string( "XMLOBJECT" ) )
    {
        return _veXMLObject;
    }
    return 0;
}
//////////////////////////////////////////////////////////////////////////////////////
void DataValuePair::_extractXMLObject( DOMElement* baseElement, std::string objectTypeName )
{
    DOMElement* genericObject = GetSubElement( baseElement, objectTypeName, 0 );
    if( genericObject )
    {
        if( _veXMLObject )
        {
            delete _veXMLObject;
            _veXMLObject = 0;
        }

        char* tempString = XMLString::transcode( genericObject->getAttribute( xercesString( "objectType" ) ) );
        std::string attr( tempString );
        XMLString::release( &tempString );
        if( !attr.empty() )
        {
            _veXMLObject = XMLObjectFactory::Instance()->CreateXMLObject( attr, "new" );
            _veXMLObject->SetObjectFromXMLData( genericObject );
        }
        else
        {
            std::cerr << "DataValuePair::_extractXMLObject : ERROR : Document not formated properly" << std::endl;
        }
    }

}
////////////////////////////////////////////////////////////
void DataValuePair::SetObjectFromXMLData( DOMNode* element )
{
    DOMElement* currentElement = 0;
    if( element->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( element );
        //std::cout << " Node name " << XMLString::transcode(currentElement->getNodeName()) << std::endl;
    }

    if( currentElement )
    {
        GetAttribute( currentElement, "id", uuid );

        {
            DOMNodeList* subElements = currentElement->getElementsByTagName( xercesString( "dataName" ) );
            if( subElements->getLength() > 0 )
            {
                //should only be the name of the command
                DOMElement* dataName = dynamic_cast<DOMElement*>( subElements->item( 0 ) );
                _dataName = ExtractFromSimpleElement< std::string >( dataName );
            }
            else
            {
                GetAttribute( currentElement, "dataName", _dataName );
            }
        }

        //get the choice element
        {
            //get variables by tags
            DOMNodeList* subElements = 0;
            DOMElement* dataElement = 0;

            if( currentElement->getElementsByTagName( xercesString( "genericObject" ) )->getLength() )
            {
                try
                {
                    _extractXMLObject( currentElement, "genericObject" );
                }
                catch ( ... )
                {
                    std::cout << "Couldn't exctract generic XMLObject in DataValuePair!!" << std::endl;
                }
                _dataType = "XMLOBJECT";
            }
            //else if(_dataType == "STRING")
            else if( currentElement->getElementsByTagName( xercesString( "dataValueString" ) )->getLength() )
            {
                subElements = currentElement->getElementsByTagName( xercesString( "dataValueString" ) );

                DOMElement* dataValueStringName = dynamic_cast<DOMElement*>( subElements->item( 0 ) );
                if( dataValueStringName )
                {
                    this->_dataString = ExtractFromSimpleElement< std::string >( dataValueStringName );
                    SetDataType( std::string( "STRING" ) );
                }
            }
            else if( currentElement->getElementsByTagName( xercesString( "dataValueUInt" ) )->getLength() )
                //_dataType == "UNSIGNED INT")
            {
                DOMElement* dataUnsignedValue = GetSubElement( currentElement, "dataValueUInt", 0 );
                _dataUInt = ExtractFromSimpleElement< unsigned int >( dataUnsignedValue );
                _dataType = "UNSIGNED INT";
            }
            //else if(_dataType == "LONG")
            else if( currentElement->getElementsByTagName( xercesString( "dataValueInt" ) )->getLength() )
            {
                DOMElement* dataLongdValue = GetSubElement( currentElement, "dataValueInt", 0 );
                intDataValue = ExtractFromSimpleElement< long int >( dataLongdValue );
                _dataType = "LONG";
            }
            //else if(_dataType == "FLOAT" )
            else if( currentElement->getElementsByTagName( xercesString( "dataValueNum" ) )->getLength() )
            {
                //get variables by tags
                //DOMNodeList* subElements = 0;
                subElements = currentElement->getElementsByTagName( xercesString( "dataValueNum" ) );

                DOMElement* dataValueNum = dynamic_cast<DOMElement*>( subElements->item( 0 ) );
                if( dataValueNum )
                {
                    _dataValue  = ExtractFromSimpleElement< double >( dataValueNum );
                    SetDataType( std::string( "FLOAT" ) );
                }
            }
            else if( currentElement->getElementsByTagName( xercesString( "dataValue" ) )->getLength() )
            {
                DOMElement* dataValueTemp = GetSubElement( currentElement, "dataValue", 0 );
                std::string type;
                GetAttribute( dataValueTemp, "type", type );

                if( type == "xs:string" )
                {
                    this->_dataString = ExtractFromSimpleElement< std::string >( dataValueTemp );
                    SetDataType( std::string( "STRING" ) );
                }
                else if( type == "xs:unsignedInt" )
                {
                    _dataUInt = ExtractFromSimpleElement< unsigned int >( dataValueTemp );
                    _dataType = "UNSIGNED INT";
                }
                else if( type == "xs:integer" )
                {
                    intDataValue = ExtractFromSimpleElement< long int >( dataValueTemp );
                    _dataType = "LONG";
                }
                else if( type == "xs:double" )
                {
                    _dataValue  = ExtractFromSimpleElement< double >( dataValueTemp );
                    SetDataType( std::string( "FLOAT" ) );
                }
                else
                {
                    std::cerr << "ERROR DataValuePair::SetObjectFromXMLData = "
                    << type << " data type not supported." << std::endl;
                }
            }
        }
    }

}
////////////////////////////////////////////////////////////
/*void GetData( XMLObject& data )
{
   data = *(XMLObjectFactory::Instance()->CreateXMLObjectCopy( _veXMLObject) );
}*/
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::string data )
{
    _dataName = dataName;
    SetDataType( std::string( "STRING" ) );
    _dataString = data;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::string > data )
{
    _dataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    OneDStringArray* oneDString = new OneDStringArray();

    oneDString->SetArray( data );
    SetData( dataName, oneDString );

    delete oneDString;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, double data )
{
    _dataName = dataName;
    SetDataType( std::string( "FLOAT" ) );
    _dataValue = data;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< double > data )
{
    _dataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    OneDDoubleArray* oneDDouble = new OneDDoubleArray();

    oneDDouble->SetArray( data );
    SetData( dataName, oneDDouble );

    delete oneDDouble;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< double > > data )
{
    _dataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    TwoDDoubleArray* twoDDouble = new TwoDDoubleArray();

    twoDDouble->SetArray( data );
    SetData( dataName, twoDDouble );

    delete twoDDouble;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< std::vector< double > > > data )
{
    _dataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    ThreeDDoubleArray* threeDDouble = new ThreeDDoubleArray();

    threeDDouble->SetArray( data );
    SetData( dataName, threeDDouble );

    delete threeDDouble;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, long int data )
{
    _dataName = dataName;
    SetDataType( std::string( "LONG" ) );
    intDataValue = data;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< long > data )
{
    _dataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    OneDIntArray* oneDInt = new OneDIntArray();

    oneDInt->SetArray( data );
    SetData( dataName, oneDInt );

    delete oneDInt;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< long > > data )
{
    _dataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    TwoDIntArray* twoDInt = new TwoDIntArray();

    twoDInt->SetArray( data );
    SetData( dataName, twoDInt );

    delete twoDInt;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< std::vector< long > > > data )
{
    _dataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    ThreeDIntArray* threeDInt = new ThreeDIntArray();

    threeDInt->SetArray( data );
    SetData( dataName, threeDInt );

    delete threeDInt;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::string& data )
{
    data = _dataString;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::string >& data )
{
    if( _veXMLObject->GetObjectType() == "OneDStringArray" )
    {
        data = dynamic_cast< OneDStringArray* >( _veXMLObject )->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( double& data )
{
    data = _dataValue;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< double >& data )
{
    if( _veXMLObject->GetObjectType() == "OneDDoubleArray" )
    {
        data = dynamic_cast< OneDDoubleArray* >( _veXMLObject )->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< double > > & data )
{
    if( _veXMLObject->GetObjectType() == "TwoDDoubleArray" )
    {
        data = dynamic_cast< TwoDDoubleArray* >( _veXMLObject )->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< std::vector< double > > > & data )
{
    if( _veXMLObject->GetObjectType() == "ThreeDDoubleArray" )
    {
        data = dynamic_cast< ThreeDDoubleArray* >( _veXMLObject )->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( long int & data )
{
    data = intDataValue;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< long >& data )
{
    if( _veXMLObject->GetObjectType() == "OneDIntArray" )
    {
        data = dynamic_cast< OneDIntArray* >( _veXMLObject )->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< long > > & data )
{
    if( _veXMLObject->GetObjectType() == "TwoDIntArray" )
    {
        data = dynamic_cast< TwoDIntArray* >( _veXMLObject )->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< std::vector< long > > > & data )
{
    if( _veXMLObject->GetObjectType() == "ThreeDIntArray" )
    {
        data = dynamic_cast< ThreeDIntArray* >( _veXMLObject )->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( unsigned int& data )
{
    data = _dataUInt;
}

