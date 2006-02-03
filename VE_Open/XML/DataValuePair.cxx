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
 * File:          $RCSfile: DataValuePair.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/OneDDoubleArray.h"
#include "VE_Open/XML/TwoDDoubleArray.h"
#include "VE_Open/XML/ThreeDDoubleArray.h"
#include "VE_Open/XML/OneDIntArray.h"
#include "VE_Open/XML/TwoDIntArray.h"
#include "VE_Open/XML/ThreeDIntArray.h"
#include "VE_Open/XML/OneDStringArray.h"

#include <iostream>
#include <sstream>

using namespace VE_XML;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
DataValuePair::DataValuePair(std::string type )
:XMLObject()
{
   _dataType = type;
   _dataName = '\0';

   _dataValue = 0;
   _dataArray = 0;
   _dataString = '\0';
   _dataTransform = 0;
   
   oneDDouble = 0;
   twoDDouble = 0;
   threeDDouble = 0;
   oneDInt = 0;
   twoDInt = 0;
   threeDInt = 0;
   oneDString = 0;
   _dataUInt = 0;
   
}
///////////////////////////////////
DataValuePair::~DataValuePair()
{
   if(_dataArray)
   {
      delete _dataArray;
      _dataArray = 0;
   }
   if(_dataTransform)
   {
      delete _dataTransform;
      _dataTransform = 0;
   }
   
   if ( oneDDouble )
   {
      delete oneDDouble;
      oneDDouble = 0;
   }

   if ( twoDDouble )
   {
      delete twoDDouble;
      twoDDouble = 0;
   }

   if ( threeDDouble )
   {
      delete threeDDouble;
      threeDDouble = 0;
   }

   if ( oneDInt )
   {
      delete oneDInt;
      oneDInt = 0;
   }

   if ( twoDInt )
   {
      delete twoDInt;
      twoDInt = 0;
   }

   if ( threeDInt )
   {
      delete threeDInt;
      threeDInt = 0;
   }

   if ( oneDString )
   {
      delete oneDString;
      oneDString = 0;
   }
}
///////////////////////////////////////////
DataValuePair::DataValuePair( const DataValuePair& input )
:XMLObject(input)
{
   oneDDouble = 0;
   twoDDouble = 0;
   threeDDouble = 0;
   oneDInt = 0;
   twoDInt = 0;
   threeDInt = 0;
   oneDString = 0;

   _dataUInt = 0;
   _dataTransform = 0;
   _dataUInt = input._dataUInt;
   _dataType = input._dataType;
   _dataName = input._dataName;

   _dataValue = input._dataValue;
   _dataArray = input._dataArray;
   _dataString = input._dataString;

   if(input._dataTransform)
      _dataTransform = new Transform(*input._dataTransform);

   if ( input.oneDDouble )
      oneDDouble = new OneDDoubleArray( *(input.oneDDouble) );

   if ( input.twoDDouble )
      twoDDouble = new TwoDDoubleArray( *(input.twoDDouble) );

   if ( input.threeDDouble )
      threeDDouble = new ThreeDDoubleArray( *(input.threeDDouble) );

   if ( input.oneDInt )
      oneDInt = new OneDIntArray( *(input.oneDInt) );

   if ( input.twoDInt )
      twoDInt = new TwoDIntArray( *(input.twoDInt) );

   if ( input.threeDInt )
      threeDInt = new ThreeDIntArray( *(input.threeDInt) );

   if ( input.oneDString )
      oneDString = new OneDStringArray( *(input.oneDString) );
}
/////////////////////////////////////////////////////
DataValuePair& DataValuePair::operator=( const DataValuePair& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      XMLObject::operator =(input);
      _dataType = input._dataType;
      _dataName = input._dataName;
      _dataUInt = input._dataUInt;
      _dataValue = input._dataValue;
      _dataArray = input._dataArray;
      _dataString = input._dataString;
      _dataTransform = new Transform(*input._dataTransform);

      if ( input.oneDDouble )
         *oneDDouble = *(input.oneDDouble);

      if ( input.twoDDouble )
         *twoDDouble = *(input.twoDDouble);

      if ( input.threeDDouble )
         *threeDDouble = *(input.threeDDouble);

      if ( input.oneDInt )
         *oneDInt = *(input.oneDInt);

      if ( input.twoDInt )
         *twoDInt = *(input.twoDInt);

      if ( input.threeDInt )
         *threeDInt = *(input.threeDInt);

      if ( input.oneDString )
         *oneDString = *(input.oneDString);
   }
   return *this;
}
/////////////////////////////////////////
unsigned int DataValuePair::GetUIntData()
{
   return _dataUInt;
}
/////////////////////////////////////////////////////
void DataValuePair::SetDataValue(unsigned int data)
{
   if(_dataType != std::string("UNSIGNED INT"))
   {
      std::cout<<"Invalid type passed into DataValuePair::SetDataString"<<std::endl;
      return;
   }
    _dataUInt = data;

}
////////////////////////////////////////////
void DataValuePair::SetDataType(std::string type)
{
   if( (type == std::string("STRING")) ||
      (type == std::string("UNSIGNED INT")) ||
       (type == std::string("FLOAT"))  ||
       (type == std::string("FARRAY")) ||
       (type == std::string("TRANSFORM")) ||
       (type == std::string("1DDBOUBLE")) ||
       (type == std::string("2DDBOUBLE")) ||
       (type == std::string("3DDBOUBLE")) ||
       (type == std::string("LONG")) ||
       (type == std::string("1DLONG")) ||
       (type == std::string("2DLONG")) ||
       (type == std::string("3DLONG")) )
   {
      _dataType = type;
   }
   else
   {
      std::cout<<"Invalid type specified in DataValuePair::SetDataType: "<<std::endl;
      std::cout<<type<<std::endl;
      std::cout<<"Valid types are:"<<std::endl
               <<"STRING == a string value. "<<std::endl
               <<"FLOAT == a single float value. "<<std::endl
               <<"FARRAY == a float array. "<<std::endl
               <<"TRANSFORM == a Transform."<<std::endl
               <<"UNSIGNED INT == an Unsigned int."<<std::endl
               <<"1DDBOUBLE == a ."<<std::endl
               <<"2DDBOUBLE == a ."<<std::endl
               <<"3DDBOUBLE == a ."<<std::endl;
   }
}
/////////////////////////////////////////////////////
void DataValuePair::SetDataString(std::string data)
{
   if(_dataType != std::string("STRING"))
   {
      std::cout<<"Invalid type passed into DataValuePair::SetDataString"<<std::endl;
      return;
   }
   if(!data.empty())
   {
      _dataString = data;
   }
}
///////////////////////////////////////////////
void DataValuePair::SetDataValue( double data )
{
   if(_dataType != std::string("FLOAT"))
   {
      std::cout<<"Invalid type passed into DataValuePair::SetCommandDataTransform"<<std::endl;
      return;
   }
   _dataValue = data;
}
////////////////////////////////////////////////////////////////////////
void DataValuePair::SetDataArray( VE_XML::FloatArray* input )
{
   if ( _dataType != std::string("FARRAY") )
   {
      std::cout<<"Invalid type passed into DataValuePair::SetCommandDataTransform"<<std::endl;
      return;
   }

   if( input )
   {
      if ( !_dataArray )
      {
         _dataArray = new FloatArray( *input );
      }
      else
      {
         *_dataArray = *input;
      }
   }
}
///////////////////////////////////////////////////////////////////
void DataValuePair::SetDataTransform(VE_XML::Transform* xForm)
{
   if ( _dataType != std::string("TRANSFORM") )
   {
      std::cout<<"Invalid type passed into DataValuePair::SetDataTransform"<<std::endl;
      return;
   }

   if ( xForm )
   {
      if ( !_dataTransform )
      {
         _dataTransform = new VE_XML::Transform( *xForm );
      }
      else
      {
         *_dataTransform = *xForm;
      }
   }
}
///////////////////////////////////////
void DataValuePair::_updateDataName()
{
   DOMElement* dataNameElement = _rootDocument->createElement(xercesString("dataName"));
   DOMText* dataName = _rootDocument->createTextNode(xercesString(_dataName.c_str()));
   dataNameElement->appendChild(dataName);
   _veElement->appendChild(dataNameElement);
}
//////////////////////////////////////////////
void DataValuePair::_updateDataValueNumber()
{
   DOMElement* dataValueNumElement = _rootDocument->createElement(xercesString("dataValueNum"));
   std::stringstream float2string;
   float2string<<_dataValue;
   DOMText* dataValue = _rootDocument->createTextNode( xercesString( float2string.str().c_str() ) );

   dataValueNumElement->appendChild(dataValue);
   _veElement->appendChild(dataValueNumElement);
}
//////////////////////////////////////////////
void DataValuePair::_updateDataValueString()
{
   DOMElement* dataValueStringElement = _rootDocument->createElement(xercesString("dataValueString"));
   DOMText* dataValueString = _rootDocument->createTextNode(xercesString(_dataString.c_str()));
   dataValueStringElement->appendChild(dataValueString);
   _veElement->appendChild(dataValueStringElement);
}
///////////////////////////////////////
void DataValuePair::_updateVEElement( std::string input )
{
   //fill in datavalue pairs, via xerces, according to schema
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }
   //Be sure to set the number of children (_nChildren) either here or in the updating subElements code

   //we know there are only 2 children so set it now
   _nChildren = 2;

   //Add code here to update the specific sub elements
   _updateDataName();

   //update the value held in the pair
   if ( _dataType == std::string("FLOAT") )
   {
      _updateDataValueNumber();
   }
   else if( _dataType == std::string("UNSIGNED INT") )
   {
       SetSubElement( "dataValueUInt", _dataUInt );
   }
   else if( _dataType == std::string("LONG") )
   {
       SetSubElement( "dataValueInt", intDataValue );
   }
   else if(_dataType == std::string("STRING"))
   {
      _updateDataValueString();
   }
   else if(_dataType == std::string("FARRAY"))
   {
      _dataArray->SetOwnerDocument(_rootDocument);
      SetSubElement( "dataArray", _dataArray );
   }
   else if(_dataType == std::string("TRANSFORM"))
   {
      _dataTransform->SetOwnerDocument(_rootDocument);
      SetSubElement( "dataTransform", _dataTransform );
   }
   else if(_dataType == std::string("1DSTRING"))
   {
      oneDString->SetOwnerDocument(_rootDocument);
      SetSubElement( "data1DStringArray", oneDString );
   }
   else if( _dataType == std::string("1DDOUBLE") )
   {
      oneDDouble->SetOwnerDocument(_rootDocument);
      SetSubElement( "data1DDoubleArray", oneDDouble );
   }
   else if( _dataType == std::string("2DDOUBLE") )
   {
      twoDDouble->SetOwnerDocument(_rootDocument);
      SetSubElement( "data2DDoubleArray", twoDDouble );
   }
   else if( _dataType == std::string("3DDOUBLE") )
   {
      threeDDouble->SetOwnerDocument(_rootDocument);
      SetSubElement( "data3DDoubleArray", threeDDouble );
   }
   else if( _dataType == std::string("1DLONG") )
   {
      oneDDouble->SetOwnerDocument(_rootDocument);
      SetSubElement( "data1DIntArray", oneDDouble );
   }
   else if( _dataType == std::string("2DLONG") )
   {
      twoDDouble->SetOwnerDocument(_rootDocument);
      SetSubElement( "data2DIntArray", twoDDouble );
   }
   else if( _dataType == std::string("3DLONG") )
   {
      threeDDouble->SetOwnerDocument(_rootDocument);
      SetSubElement( "data3DIntArray", threeDDouble );
   }
}
///////////////////////////////////////////////////
void DataValuePair::SetDataName(std::string name)
{
   if(!name.empty())
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
   if(_dataType == std::string("STRING"))
   {
      return _dataString;
   }
   return 0;
}
/////////////////////////////////////
double DataValuePair::GetDataValue()
{
   if(_dataType == std::string("FLOAT"))
   {
      return _dataValue;
   }
   return 0;
}
//////////////////////////////////////
VE_XML::FloatArray* DataValuePair::GetDataArray()
{
   if(_dataType == std::string("FARRAY"))
   {
      return _dataArray;
   }
   return 0;
}
////////////////////////////////////////////////////////
VE_XML::Transform* DataValuePair::GetDataTransform()
{
   if(_dataType == std::string("TRANSFORM"))
   {
      return _dataTransform;
   }
   return 0;
}

////////////////////////////////////////////////////////////
void DataValuePair::SetObjectFromXMLData(DOMNode* element)
{
   DOMElement* currentElement = 0;
   if(element->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(element);
   }

   if(currentElement)
   {
      {
         //get variables by tags
         DOMNodeList* subElements = currentElement->getElementsByTagName(xercesString("dataName"));
         //should only be the name of the command
         DOMElement* dataName = dynamic_cast<DOMElement*>(subElements->item(0));
         if(dataName)
         {
            _dataName = ExtractDataStringFromSimpleElement(dataName);
         }
      }

      //get the choice element
      {
         //get variables by tags
         DOMNodeList* subElements = 0;
         DOMElement* dataElement = 0;
         if(currentElement->getElementsByTagName(xercesString("dataValueString"))->getLength() )
         {
            subElements = currentElement->getElementsByTagName(xercesString("dataValueString"));

            DOMElement* dataValueStringName = dynamic_cast<DOMElement*>(subElements->item(0));
            if(dataValueStringName)
            {
               this->_dataString = ExtractDataStringFromSimpleElement(dataValueStringName);
               SetDataType(std::string("STRING"));
            }
         }
         else if(currentElement->getElementsByTagName(xercesString("dataValueNum"))->getLength() )
         {
            //get variables by tags
            DOMNodeList* subElements = 0;
            subElements = currentElement->getElementsByTagName(xercesString("dataValueNum"));

            DOMElement* dataValueNum = dynamic_cast<DOMElement*>(subElements->item(0));
            if(dataValueNum)
            {
               _dataValue  = ExtractDataNumberFromSimpleElement(dataValueNum);
               SetDataType(std::string("FLOAT"));
            }
         }
         else if(currentElement->getElementsByTagName(xercesString("dataArray"))->getLength() )
         {
            subElements = currentElement->getElementsByTagName(xercesString("dataArray"));
            if(_dataArray)
            {
               delete _dataArray;
               _dataArray = 0;
            }
            _dataArray = new VE_XML::FloatArray();
            _dataArray->SetObjectFromXMLData(subElements->item(0));
            SetDataType(std::string("FARRAY"));
         }
         else if(currentElement->getElementsByTagName(xercesString("dataTransform"))->getLength() )
         {
            subElements = currentElement->getElementsByTagName(xercesString("dataTransform"));
            if(_dataTransform)
            {
               delete _dataTransform;
               _dataTransform = 0;
            }
            _dataTransform = new VE_XML::Transform();
            _dataTransform->SetObjectFromXMLData(subElements->item(0));
            SetDataType( std::string("TRANSFORM") );
         }
         else if( currentElement->getElementsByTagName( xercesString("data1DDoubleArray") )->getLength() )
         {
            dataElement = GetSubElement( currentElement, "data1DDoubleArray", 0 );
            if ( oneDDouble )
            {
               delete oneDDouble;
            }
            oneDDouble = new VE_XML::OneDDoubleArray(  );
            oneDDouble->SetObjectFromXMLData( dataElement );
            SetDataType( std::string("1DDOUBLE") );
         }
         else if( currentElement->getElementsByTagName( xercesString("data2DDoubleArray") )->getLength() )
         {
            dataElement = GetSubElement( currentElement, "data2DDoubleArray", 0 );
            if ( twoDDouble )
            {
               delete twoDDouble;
            }
            twoDDouble = new VE_XML::TwoDDoubleArray(  );
            twoDDouble->SetObjectFromXMLData( dataElement );
            SetDataType( std::string("2DDOUBLE") );
         }
         else if( currentElement->getElementsByTagName( xercesString("data3DDoubleArray") )->getLength() )
         {
            dataElement = GetSubElement( currentElement, "data3DDoubleArray", 0 );
            if ( threeDDouble )
            {
               delete threeDDouble;
            }
            threeDDouble = new VE_XML::ThreeDDoubleArray(  );
            threeDDouble->SetObjectFromXMLData( dataElement );
            SetDataType( std::string("3DDOUBLE") );
         }
         else if( currentElement->getElementsByTagName( xercesString("data1DStringArray") )->getLength() )
         {
            dataElement = GetSubElement( currentElement, "data1DStringArray", 0 );
            if ( oneDString )
            {
               delete oneDString;
            }
            oneDString = new VE_XML::OneDStringArray(  );
            oneDString->SetObjectFromXMLData( dataElement );
            SetDataType( std::string("1DSTRING") );
         }
         else if( currentElement->getElementsByTagName( xercesString("data1DIntArray") )->getLength() )
         {
            dataElement = GetSubElement( currentElement, "data1DIntArray", 0 );
            if ( oneDInt )
            {
               delete oneDInt;
            }
            oneDInt = new VE_XML::OneDIntArray(  );
            oneDInt->SetObjectFromXMLData( dataElement );
            SetDataType( std::string("1DLONG") );
         }
         else if( currentElement->getElementsByTagName( xercesString("data2DIntArray") )->getLength() )
         {
            dataElement = GetSubElement( currentElement, "data2DIntArray", 0 );
            if ( twoDInt )
            {
               delete twoDInt;
            }
            twoDInt = new VE_XML::TwoDIntArray(  );
            twoDInt->SetObjectFromXMLData( dataElement );
            SetDataType( std::string("2DLONG") );
         }
         else if( currentElement->getElementsByTagName( xercesString("data3DIntArray") )->getLength() )
         {
            dataElement = GetSubElement( currentElement, "data3DIntArray", 0 );
            if ( threeDInt )
            {
               delete threeDInt;
            }
            threeDInt = new VE_XML::ThreeDIntArray(  );
            threeDInt->SetObjectFromXMLData( dataElement );
            SetDataType( std::string("3DLONG") );
         }
      }
   }
   
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, VE_XML::FloatArray* data )
{
   _dataName = dataName;
   SetDataType( std::string("FARRAY") );
   if ( !_dataArray )
   {
      _dataArray = new FloatArray( *data );
   }
   else
   {
      *_dataArray = *data;
   }
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, VE_XML::Transform* data )
{
   _dataName = dataName;
   SetDataType( std::string("TRANSFORM") );
   if ( !_dataTransform )
   {
      _dataTransform = new VE_XML::Transform( *data );
   }
   else
   {
      *_dataTransform = *data;
   }
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::string data )
{
   _dataName = dataName;
   SetDataType( std::string("STRING") );
   _dataString = data;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::string > data )
{
   _dataName = dataName;
   SetDataType( std::string("1DSTRING") );

   if ( oneDString == NULL )
      oneDString = new OneDStringArray(  );

   oneDString->SetArray( data );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, double data )
{
   _dataName = dataName;
   SetDataType( std::string("FLOAT") );
   _dataValue = data;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< double > data )
{
   _dataName = dataName;
   SetDataType( std::string("1DDOUBLE") );

   if ( oneDDouble == NULL )
      oneDDouble = new OneDDoubleArray(  );

   oneDDouble->SetArray( data );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< double > > data )
{
   _dataName = dataName;
   SetDataType( std::string("2DDOUBLE") );

   if ( twoDDouble == NULL )
      twoDDouble = new TwoDDoubleArray(  );

   twoDDouble->SetArray( data );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< std::vector< double > > > data )
{
   _dataName = dataName;
   SetDataType( std::string("3DDOUBLE") );

   if ( threeDDouble == NULL )
      threeDDouble = new ThreeDDoubleArray(  );

   threeDDouble->SetArray( data );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, long data )
{
   _dataName = dataName;
   SetDataType( std::string("LONG") );
   intDataValue = data;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< long > data )
{
   _dataName = dataName;
   SetDataType( std::string("1DLONG") );

   if ( oneDInt == NULL )
      oneDInt = new OneDIntArray(  );

   oneDInt->SetArray( data );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< long > > data )
{
   _dataName = dataName;
   SetDataType( std::string("2DLONG") );

   if ( twoDInt == NULL )
      twoDInt = new TwoDIntArray(  );

   twoDInt->SetArray( data );
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< std::vector< long > > > data )
{
   _dataName = dataName;
   SetDataType( std::string("3DLONG") );

   if ( threeDInt == NULL )
      threeDInt = new ThreeDIntArray(  );

   threeDInt->SetArray( data );
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::string& data )
{
   data = _dataString;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::string >& data )
{
   data = oneDString->GetArray();
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( double& data )
{
   data = _dataValue;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< double >& data )
{
   data = oneDDouble->GetArray();
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< double > >& data )
{
   data = twoDDouble->GetArray();
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< std::vector< double > > >& data )
{
   data = threeDDouble->GetArray();
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( long& data )
{
   data = intDataValue;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< long >& data )
{
   data = oneDInt->GetArray();
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< long > >& data )
{
   data = twoDInt->GetArray();
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< std::vector< long > > >& data )
{
   data = threeDInt->GetArray();
}
