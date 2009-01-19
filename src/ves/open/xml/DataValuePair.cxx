/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/open/xml/TwoDStringArray.h>
#include <ves/open/xml/XMLObjectFactory.h>

#include <iostream>
#include <sstream>
XERCES_CPP_NAMESPACE_USE

using namespace ves::open::xml;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
DataValuePair::DataValuePair( const std::string type )
        : XMLObject()
{
    mDataType = type;
    mDataName = '\0';

    mDataValue = 0;
    mDataString = '\0';
    mDataUInt = 0;
    mIntDataValue = 0;

    mVeXMLObject = XMLObjectPtr();
    SetObjectType( "DataValuePair" );

}
///////////////////////////////////
DataValuePair::~DataValuePair()
{
    ;
}
//////////////////////////////////////////////////////////
DataValuePair::DataValuePair( const DataValuePair& input )
        : XMLObject( input )
{
    mDataUInt = input.mDataUInt;
    mDataType = input.mDataType;
    mDataName = input.mDataName;
    mIntDataValue = input.mIntDataValue;

    mDataValue = input.mDataValue;
    mDataString = input.mDataString;

    mVeXMLObject = XMLObjectPtr();

    if( input.mVeXMLObject )
    {
        mVeXMLObject = XMLObjectFactory::Instance()->CreateXMLObjectCopy( input.mVeXMLObject );
    }
}
/////////////////////////////////////////////////////
DataValuePair& DataValuePair::operator=( const DataValuePair& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mDataType = input.mDataType;
        mDataName = input.mDataName;
        mDataUInt = input.mDataUInt;
        mIntDataValue = input.mIntDataValue;
        mDataValue = input.mDataValue;
        mDataString = input.mDataString;

        if( input.mVeXMLObject )
        {
            mVeXMLObject = XMLObjectFactory::Instance()->CreateXMLObjectCopy( input.mVeXMLObject );
        }
    }
    return *this;
}
/////////////////////////////////////////
unsigned int DataValuePair::GetUIntData()
{
    return mDataUInt;
}
/////////////////////////////////////////////////////
void DataValuePair::SetDataValue( unsigned int data )
{
    if( mDataType != std::string( "UNSIGNED INT" ) )
    {
        std::cout << "Invalid type passed into DataValuePair::SetDataString" << std::endl;
        return;
    }
    mDataUInt = data;

}
/////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, unsigned int data )
{
    mDataName = dataName;
    SetDataType( std::string( "UNSIGNED INT" ) );
    mDataUInt = data;
}
////////////////////////////////////////////
void DataValuePair::SetDataType( const std::string& type )
{
    if (( type == std::string( "STRING" ) ) ||
            ( type == std::string( "UNSIGNED INT" ) ) ||
            ( type == std::string( "FLOAT" ) )  ||
            ( type == std::string( "FARRAY" ) ) ||
            ( type == std::string( "LONG" ) ) ||
            ( type == std::string( "XMLOBJECT" ) ) )
    {
        mDataType = type;
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
void DataValuePair::SetDataString( const std::string& data )
{
    if( mDataType != std::string( "STRING" ) )
    {
        std::cout << "Invalid type passed into DataValuePair::SetDataString" << std::endl;
        return;
    }
    if( !data.empty() )
    {
        mDataString = data;
    }
}
///////////////////////////////////////////////
void DataValuePair::SetDataValue( double data )
{
    if( mDataType != std::string( "FLOAT" ) )
    {
        std::cout << "Invalid type passed into DataValuePair::SetDataValue" << std::endl;
        return;
    }
    mDataValue = data;
}
////////////////////////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, XMLObjectPtr vexmlObject )
{
    if( vexmlObject )
    {
        mVeXMLObject = XMLObjectFactory::Instance()->CreateXMLObjectCopy( vexmlObject );

        mDataName = dataName;

        SetDataType( std::string( "XMLOBJECT" ) );
    }
    else
    {
        std::cout << "Invalid type passed into DataValuePair::SetData(std::string dataName,XMLObjectPtr vexmlObject)" << std::endl;
    }
}
///////////////////////////////////////
void DataValuePair::_updateVEElement( const std::string& input )
{
    //fill in datavalue pairs, via xerces, according to schema
    //Add code here to update the specific sub elements
    SetAttribute( "dataName", mDataName );
    SetAttribute( "id", mUuid );

    //update the value held in the pair
    if( mDataType == std::string( "FLOAT" ) )
    {
        SetSubElement( "dataValue", mDataValue );
    }
    else if( mDataType == std::string( "UNSIGNED INT" ) )
    {
        SetSubElement( "dataValue", mDataUInt );
    }
    else if( mDataType == std::string( "LONG" ) )
    {
        SetSubElement( "dataValue", mIntDataValue );
    }
    else if( mDataType == std::string( "STRING" ) )
    {
        SetSubElement( "dataValue", mDataString );
    }
    else if( mDataType == std::string( "XMLOBJECT" ) )
    {
        DOMElement* genericElement = 
            SetSubElement<ves::open::xml::XMLObjectPtr>( "genericObject", mVeXMLObject );
        SetAttribute( "objectType", mVeXMLObject->GetObjectType(), genericElement );
    }
}
///////////////////////////////////////////////////
void DataValuePair::SetDataName( const std::string& name )
{
    if( !name.empty() )
    {
        mDataName = name;
    }
}
//////////////////////////////////////////
const std::string DataValuePair::GetDataName()
{
    return mDataName;
}
//////////////////////////////////////////
const std::string DataValuePair::GetDataType()
{
    return mDataType;
}
////////////////////////////////////////////
const std::string DataValuePair::GetDataString()
{
    if( mDataType == std::string( "STRING" ) )
    {
        return mDataString;
    }
    return std::string();
}
/////////////////////////////////////
double DataValuePair::GetDataValue()
{
    if( mDataType == std::string( "FLOAT" ) )
    {
        return mDataValue;
    }
    return 0;
}
////////////////////////////////////////////////////
XMLObjectPtr DataValuePair::GetDataXMLObject()
{
    if( mDataType == std::string( "XMLOBJECT" ) )
    {
        return mVeXMLObject;
    }
    return XMLObjectPtr();
}
//////////////////////////////////////////////////////////////////////////////////////
void DataValuePair::_extractXMLObject( DOMElement* baseElement, const std::string& objectTypeName )
{
    DOMElement* genericObject = GetSubElement( baseElement, objectTypeName, 0 );
    if( genericObject )
    {
        char* tempString = XMLString::transcode(
                           genericObject->getAttribute(
                           Convert( "objectType" ).toXMLString() ) );
        std::string attr( tempString );
        XMLString::release( &tempString );
        if( !attr.empty() )
        {
            mVeXMLObject = XMLObjectFactory::Instance()->CreateXMLObject( attr, "new" );
            mVeXMLObject->SetObjectFromXMLData( genericObject );
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
        GetAttribute( currentElement, "id", mUuid );

        {
            DOMNodeList* subElements = currentElement->getElementsByTagName(
                                       Convert( "dataName" ).toXMLString() );
            if( subElements->getLength() > 0 )
            {
                //should only be the name of the command
                DOMElement* dataName = static_cast<DOMElement*>( subElements->item( 0 ) );
                GetDataFromElement( dataName, mDataName );
            }
            else
            {
                GetAttribute( currentElement, "dataName", mDataName );
            }
        }

        //get the choice element
        {
            //get variables by tags
            DOMNodeList* subElements = 0;
            DOMElement* dataElement = 0;

            if( currentElement->getElementsByTagName( ves::open::xml::Convert( "genericObject" ).toXMLString() )->getLength() )
            {
                try
                {
                    _extractXMLObject( currentElement, "genericObject" );
                }
                catch ( ... )
                {
                    std::cout << "Couldn't exctract generic XMLObject in DataValuePair!!" << std::endl;
                }
                mDataType = "XMLOBJECT";
            }
            //else if(mDataType == "STRING")
            else if( currentElement->getElementsByTagName( ves::open::xml::Convert( "dataValueString" ).toXMLString() )->getLength() )
            {
                subElements = currentElement->getElementsByTagName( ves::open::xml::Convert( "dataValueString" ).toXMLString() );

                DOMElement* dataValueStringName = static_cast<DOMElement*>( subElements->item( 0 ) );
                if( dataValueStringName )
                {
                    GetDataFromElement( dataValueStringName, mDataString);
                    SetDataType( std::string( "STRING" ) );
                }
            }
            else if( currentElement->getElementsByTagName( ves::open::xml::Convert( "dataValueUInt" ).toXMLString() )->getLength() )
                //mDataType == "UNSIGNED INT")
            {
                DOMElement* dataUnsignedValue = GetSubElement( currentElement, "dataValueUInt", 0 );

                GetDataFromElement( dataUnsignedValue, mDataUInt );
                mDataType = "UNSIGNED INT";
            }
            //else if(mDataType == "LONG")
            else if( currentElement->getElementsByTagName( ves::open::xml::Convert( "dataValueInt" ).toXMLString() )->getLength() )
            {
                DOMElement* dataLongdValue = GetSubElement( currentElement, "dataValueInt", 0 );
                GetDataFromElement( dataLongdValue, mIntDataValue);
                mDataType = "LONG";
            }
            //else if(mDataType == "FLOAT" )
            else if( currentElement->getElementsByTagName( ves::open::xml::Convert( "dataValueNum" ).toXMLString() )->getLength() )
            {
                //get variables by tags
                //DOMNodeList* subElements = 0;
                subElements = currentElement->getElementsByTagName( ves::open::xml::Convert( "dataValueNum" ).toXMLString() );

                DOMElement* dataValueNum = static_cast<DOMElement*>( subElements->item( 0 ) );
                if( dataValueNum )
                {
                    GetDataFromElement( dataValueNum, mDataValue );
                    SetDataType( std::string( "FLOAT" ) );
                }
            }
            else if( currentElement->getElementsByTagName( ves::open::xml::Convert( "dataValue" ).toXMLString() )->getLength() )
            {
                DOMElement* dataValueTemp = GetSubElement( currentElement, "dataValue", 0 );
                std::string type;
                GetAttribute( dataValueTemp, "type", type );

                if( type == "xs:string" )
                {
                    GetDataFromElement( dataValueTemp, mDataString );
                    SetDataType( std::string( "STRING" ) );
                }
                else if( type == "xs:unsignedInt" )
                {
                    GetDataFromElement( dataValueTemp, mDataUInt );
                    mDataType = "UNSIGNED INT";
                }
                else if( type == "xs:integer" )
                {
                    GetDataFromElement( dataValueTemp, mIntDataValue );
                    mDataType = "LONG";
                }
                else if( type == "xs:double" )
                {
                    GetDataFromElement( dataValueTemp, mDataValue );
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
   data = *(XMLObjectFactory::Instance()->CreateXMLObjectCopy( mVeXMLObject) );
}*/
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, const std::string& data )
{
    mDataName = dataName;
    SetDataType( std::string( "STRING" ) );
    mDataString = data;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, const std::vector< std::string >& data )
{
    mDataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    OneDStringArrayPtr oneDString( new OneDStringArray() );

    oneDString->SetArray( data );
    SetData( dataName, oneDString );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, 
							 const std::vector< std::vector< 
							 std::string > >& data )
{
    mDataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    TwoDStringArrayPtr twoDString( new TwoDStringArray() );

    twoDString->SetArray( data );
    SetData( dataName, twoDString );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, double data )
{
    mDataName = dataName;
    SetDataType( std::string( "FLOAT" ) );
    mDataValue = data;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, const std::vector< double >& data )
{
    mDataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    OneDDoubleArrayPtr oneDDouble( new OneDDoubleArray() );

    oneDDouble->SetArray( data );
    SetData( dataName, oneDDouble );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, const std::vector< std::vector< double > >& data )
{
    mDataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    TwoDDoubleArrayPtr twoDDouble( new TwoDDoubleArray() );

    twoDDouble->SetArray( data );
    SetData( dataName, twoDDouble );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, const std::vector< std::vector< std::vector< double > > >& data )
{
    mDataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    ThreeDDoubleArrayPtr threeDDouble( new ThreeDDoubleArray() );

    threeDDouble->SetArray( data );
    SetData( dataName, threeDDouble );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, long int data )
{
    mDataName = dataName;
    SetDataType( std::string( "LONG" ) );
    mIntDataValue = data;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, const std::vector< long >& data )
{
    mDataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    OneDIntArrayPtr oneDInt( new OneDIntArray() );

    oneDInt->SetArray( data );
    SetData( dataName, oneDInt );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, const std::vector< std::vector< long > >& data )
{
    mDataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    TwoDIntArrayPtr twoDInt( new TwoDIntArray() );

    twoDInt->SetArray( data );
    SetData( dataName, twoDInt );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( const std::string& dataName, const std::vector< std::vector< std::vector< long > > >& data )
{
    mDataName = dataName;
    SetDataType( std::string( "XMLOBJECT" ) );

    ThreeDIntArrayPtr threeDInt( new ThreeDIntArray() );

    threeDInt->SetArray( data );
    SetData( dataName, threeDInt );
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::string& data )
{
    data = mDataString;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::string >& data )
{
    if( mVeXMLObject->GetObjectType() == "OneDStringArray" )
    {
        OneDStringArrayPtr tempPtr = boost::static_pointer_cast<OneDStringArray>( mVeXMLObject );
        data = tempPtr->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
///////////////////////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< std::string > >& data )
{
    if( mVeXMLObject->GetObjectType() == "TwoDStringArray" )
    {
        TwoDStringArrayPtr tempPtr = 
			boost::static_pointer_cast<TwoDStringArray>( mVeXMLObject );
        data = tempPtr->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( double& data )
{
    data = mDataValue;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< double >& data )
{
    if( mVeXMLObject->GetObjectType() == "OneDDoubleArray" )
    {
        OneDDoubleArrayPtr tempPtr = boost::static_pointer_cast<OneDDoubleArray>( mVeXMLObject );
        data = tempPtr->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< double > > & data )
{
    if( mVeXMLObject->GetObjectType() == "TwoDDoubleArray" )
    {
        TwoDDoubleArrayPtr tempPtr =  boost::static_pointer_cast<TwoDDoubleArray>( mVeXMLObject );
        data = tempPtr->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< std::vector< double > > > & data )
{
    if( mVeXMLObject->GetObjectType() == "ThreeDDoubleArray" )
    {
        ThreeDDoubleArrayPtr tempPtr = boost::static_pointer_cast<ThreeDDoubleArray>( mVeXMLObject );
        data = tempPtr->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( long int & data )
{
    data = mIntDataValue;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< long >& data )
{
    if( mVeXMLObject->GetObjectType() == "OneDIntArray" )
    {
        OneDIntArrayPtr tempPtr = boost::static_pointer_cast<OneDIntArray>( mVeXMLObject );
        data = tempPtr->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< long > > & data )
{
    if( mVeXMLObject->GetObjectType() == "TwoDIntArray" )
    {
        TwoDIntArrayPtr tempPtr = boost::static_pointer_cast<TwoDIntArray>( mVeXMLObject );
        data = tempPtr->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< std::vector< long > > > & data )
{
    if( mVeXMLObject->GetObjectType() == "ThreeDIntArray" )
    {
        ThreeDIntArrayPtr tempPtr = boost::static_pointer_cast<ThreeDIntArray>( mVeXMLObject );
        data = tempPtr->GetArray();
    }
    else
    {
        std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
    }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( unsigned int& data )
{
    data = mDataUInt;
}

