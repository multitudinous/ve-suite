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
#include "VE_Open/XML/XMLObjectFactory.h"

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
   _dataString = '\0';
   _dataUInt = 0;
   intDataValue = 0;

   _veXMLObject = 0;
   SetObjectType("DataValuePair");
   
}
///////////////////////////////////
DataValuePair::~DataValuePair()
{
   if(_veXMLObject)
   {
      delete _veXMLObject;
      _veXMLObject = 0;
   }
}
//////////////////////////////////////////////////////////
DataValuePair::DataValuePair( const DataValuePair& input )
:XMLObject(input)
{
   _dataUInt = input._dataUInt;
   _dataType = input._dataType;
   _dataName = input._dataName;
   intDataValue = input.intDataValue;

   _dataValue = input._dataValue;
   _dataString = input._dataString;

   _veXMLObject = 0;

   if(input._veXMLObject)
   {
      _veXMLObject = VE_XML::XMLObjectFactory::Instance()->CreateXMLObjectCopy( input._veXMLObject );
   }
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
      intDataValue = input.intDataValue;
      _dataValue = input._dataValue;
      _dataString = input._dataString;

      if(_veXMLObject)
      {
         delete _veXMLObject;
         _veXMLObject = 0;
      }

      if(input._veXMLObject)
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
       (type == std::string("LONG")) ||
       (type == std::string("XMLOBJECT")) )
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
               <<"XMLOBJECT == an XMLObject"<<std::endl;
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
////////////////////////////////////////////////////////////////////////////////
void DataValuePair::SetData(std::string dataName,VE_XML::XMLObject* vexmlObject)
{
   _dataName = dataName;

   if ( _dataType != std::string("XMLOBJECT") )
   {
      std::cout<<"Invalid type passed into DataValuePair::SetDataTransform"<<std::endl;
      return;
   }
   if(_veXMLObject)
   {
      delete _veXMLObject;
      _veXMLObject = 0;
   }

   if(vexmlObject)
   {
      _veXMLObject = XMLObjectFactory::Instance()->CreateXMLObjectCopy( vexmlObject );
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

   //SetSubElement("dataType",_dataType);

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
   else if(_dataType == std::string("XMLOBJECT"))
   {
      SetSubElement( "genericObject" , _veXMLObject, "objectType", _veXMLObject->GetObjectType() );
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
////////////////////////////////////////////////////
VE_XML::XMLObject* DataValuePair::GetDataXMLObject()
{
   if(_dataType == std::string("XMLOBJECT"))
   {
      return _veXMLObject;
   }
   return 0;
}
//////////////////////////////////////////////////////////////////////////////////////
void DataValuePair::_extractXMLObject(DOMElement* baseElement,std::string objectTypeName)
{
   DOMElement* genericObject = GetSubElement(baseElement,objectTypeName,0);
   if(genericObject)
   {
      if(_veXMLObject)
      {
         delete _veXMLObject;
         _veXMLObject = 0;
      }

      std::string attr = XMLString::transcode( genericObject->getAttribute(xercesString("objectType")));
      if ( !attr.empty() )
      {
         {
            _veXMLObject = XMLObjectFactory::Instance()->CreateXMLObject( attr, "new" );
            _veXMLObject->SetObjectFromXMLData(genericObject);
         } 
      }
      else
      {
         std::cerr << "DataValuePair::_extractXMLObject : ERROR : Document not formated properly" << std::endl;
      }
   }

}
////////////////////////////////////////////////////////////
void DataValuePair::SetObjectFromXMLData(DOMNode* element)
{
   DOMElement* currentElement = 0;
   if(element->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(element);
      //std::cout << " Node name " << XMLString::transcode(currentElement->getNodeName()) << std::endl;
   }

   if(currentElement)
   {
      {
         //get variables by tags
         DOMNodeList* subElements = currentElement->getChildNodes();
         ///Test code below
         /*std::cout<< subElements->getLength() << std::endl;
         for ( XMLSize_t i = 0; i < subElements->getLength(); ++i )
         {
            DOMNode* tempNode = subElements->item( i );
            std::cout << tempNode->getNodeType() << std::endl;
            if (  1 == tempNode->getNodeType() )
            {
               DOMElement* tempElement = dynamic_cast<DOMElement*>(tempNode);
               std::cout << "----------------" << std::endl;
               std::cout << " Node name " << XMLString::transcode(tempElement->getNodeName()) << std::endl;
            }
            else if (  3 == tempNode->getNodeType() )
            {
               DOMText* tempElement = dynamic_cast<DOMText*>(tempNode);
               std::cout << " text value " << XMLString::transcode(tempElement->getData()) << std::endl;
            }
         }*/
         subElements = currentElement->getElementsByTagName(xercesString("dataName"));
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
         //DOMElement* typeNode = GetSubElement(currentElement,std::string("dataType"),0);
         //if(typeNode)
         //{
         //   _dataType = ExtractDataStringFromSimpleElement( typeNode );
         //}

         //if(_dataType == "XMLOBJECT")
         if ( currentElement->getElementsByTagName(xercesString("genericObject"))->getLength() )
         {
            try
            {
               //subElements = currentElement->getElementsByTagName(xercesString("genericObject"));
               _extractXMLObject( currentElement, "genericObject" );
            }
            catch(...)
            {
               std::cout<<"Couldn't exctract generic XMLObject in DataValuePair!!"<<std::endl;
            }
            _dataType = "XMLOBJECT";
         }
         //else if(_dataType == "STRING")
         else if ( currentElement->getElementsByTagName(xercesString("dataValueString"))->getLength() )
         {
            subElements = currentElement->getElementsByTagName(xercesString("dataValueString"));

            DOMElement* dataValueStringName = dynamic_cast<DOMElement*>(subElements->item(0));
            if(dataValueStringName)
            {
               this->_dataString = ExtractDataStringFromSimpleElement(dataValueStringName);
               SetDataType(std::string("STRING"));
            }
         }
         else if ( currentElement->getElementsByTagName(xercesString("dataValueUInt"))->getLength() )
         //_dataType == "UNSIGNED INT")
         {
            DOMElement* dataUnsignedValue = GetSubElement( currentElement, "dataValueUInt", 0 );
            _dataUInt = ExtractIntegerDataNumberFromSimpleElement( dataUnsignedValue  );
            _dataType = "UNSIGNED INT";
         }
         //else if(_dataType == "LONG")
         else if ( currentElement->getElementsByTagName(xercesString("dataValueInt"))->getLength() )
         {
            DOMElement* dataLongdValue = GetSubElement( currentElement, "dataValueInt", 0 );
            intDataValue = ExtractIntegerDataNumberFromSimpleElement( dataLongdValue  );
            _dataType = "LONG";
         }
         //else if(_dataType == "FLOAT" )
         else if( currentElement->getElementsByTagName(xercesString("dataValueNum"))->getLength() )
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
      }
   }
   
}
////////////////////////////////////////////////////////////
/*void GetData( VE_XML::XMLObject& data )
{
   data = *(XMLObjectFactory::Instance()->CreateXMLObjectCopy( _veXMLObject) );
}*/
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
   SetDataType( std::string("XMLOBJECT") );

   OneDStringArray* oneDString = new OneDStringArray();

   oneDString->SetArray( data );
   SetData( dataName, oneDString );

   delete oneDString;
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
   SetDataType( std::string("XMLOBJECT") );

   OneDDoubleArray* oneDDouble = new OneDDoubleArray();

   oneDDouble->SetArray( data );
   SetData( dataName, oneDDouble );

   delete oneDDouble;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< double > > data )
{
   _dataName = dataName;
   SetDataType( std::string("XMLOBJECT") );

   TwoDDoubleArray* twoDDouble = new TwoDDoubleArray();

   twoDDouble->SetArray( data );
   SetData( dataName, twoDDouble );

   delete twoDDouble;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< std::vector< double > > > data )
{
   _dataName = dataName;
   SetDataType( std::string("XMLOBJECT") );

   ThreeDDoubleArray* threeDDouble = new ThreeDDoubleArray();

   threeDDouble->SetArray( data );
   SetData( dataName, threeDDouble );

   delete threeDDouble;
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
   SetDataType( std::string("XMLOBJECT") );

   OneDIntArray* oneDInt = new OneDIntArray();

   oneDInt->SetArray( data );
   SetData( dataName, oneDInt );

   delete oneDInt;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< long > > data )
{
   _dataName = dataName;
   SetDataType( std::string("XMLOBJECT") );

   TwoDIntArray* twoDInt = new TwoDIntArray();

   twoDInt->SetArray( data );
   SetData( dataName, twoDInt );

   delete twoDInt;
}
////////////////////////////////////////////////////////////
void DataValuePair::SetData( std::string dataName, std::vector< std::vector< std::vector< long > > > data )
{
   _dataName = dataName;
   SetDataType( std::string("XMLOBJECT") );

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
   if ( _veXMLObject->GetObjectType() == "OneDStringArray" )
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
   if ( _veXMLObject->GetObjectType() == "OneDDoubleArray" )
   {
      data = dynamic_cast< OneDDoubleArray* >( _veXMLObject )->GetArray();
   }
   else
   {
      std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
   }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< double > >& data )
{
   if ( _veXMLObject->GetObjectType() == "TwoDDoubleArray" )
   {
      data = dynamic_cast< TwoDDoubleArray* >( _veXMLObject )->GetArray();
   }
   else
   {
      std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
   }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< std::vector< double > > >& data )
{
   if ( _veXMLObject->GetObjectType() == "ThreeDDoubleArray" )
   {
      data = dynamic_cast< ThreeDDoubleArray* >( _veXMLObject )->GetArray();
   }
   else
   {
      std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
   }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( long& data )
{
   data = intDataValue;
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< long >& data )
{
   if ( _veXMLObject->GetObjectType() == "OneDIntArray" )
   {
      data = dynamic_cast< OneDIntArray* >( _veXMLObject )->GetArray();
   }
   else
   {
      std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
   }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< long > >& data )
{
   if ( _veXMLObject->GetObjectType() == "TwoDIntArray" )
   {
      data = dynamic_cast< TwoDIntArray* >( _veXMLObject )->GetArray();
   }
   else
   {
      std::cerr << " ERROR : This DataValuePair does not contain the data you request " << std::endl;
   }
}
////////////////////////////////////////////////////////////
void DataValuePair::GetData( std::vector< std::vector< std::vector< long > > >& data )
{
   if ( _veXMLObject->GetObjectType() == "ThreeDIntArray" )
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

