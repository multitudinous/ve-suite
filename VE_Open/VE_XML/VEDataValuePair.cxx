#include "VE_XML/VEDataValuePair.h"
#include "VE_XML/VETransform.h"
#include "VE_XML/VEFloatArray.h"
#include <iostream>
#include <sstream>

using namespace VE_XML;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
VEDataValuePair::VEDataValuePair(DOMDocument* rootDoc,std::string type )
:VEXMLObject(rootDoc)
{
   _dataType = type;
   _dataName = '\0';

   _dataValue = 0;
   _dataArray = 0;
   _dataString = '\0';
   _dataTransform = 0;
}
///////////////////////////////////
VEDataValuePair::~VEDataValuePair()
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
}
////////////////////////////////////////////
void VEDataValuePair::SetDataType(std::string type)
{
   if(type == std::string("STRING")||
      type == std::string("FLOAT")||
      type == std::string("FARRAY")||
      type == std::string("TRANSFORM"))
   {
      _dataType = type;
   }
   else
   {
      std::cout<<"Invalid type specified in VEDataValuePair::SetDataType: "<<std::endl;
      std::cout<<type<<std::endl;
      std::cout<<"Valid types are:"<<std::endl;
      std::cout<<"STRING == a string value. "<<std::endl;
      std::cout<<"FLOAT == a single float value. "<<std::endl;
      std::cout<<"FARRAY == a float array. "<<std::endl;
      std::cout<<"TRANSFORM == a vetransform."<<std::endl;
   }
}
/////////////////////////////////////////////////////
void VEDataValuePair::SetDataString(std::string data)
{
   if(_dataType != std::string("STRING"))
   {
      std::cout<<"Invalid type passed into VEDataValuePair::SetDataString"<<std::endl;
      return;
   }
   if(!data.empty())
   {
      _dataString = data;
   }
}
///////////////////////////////////////////////
void VEDataValuePair::SetDataValue(float data)
{
   if(_dataType != std::string("FLOAT"))
   {
      std::cout<<"Invalid type passed into VEDataValuePair::SetCommandDataTransform"<<std::endl;
      return;
   }
   _dataValue = data;
}
////////////////////////////////////////////////////////////////////////
void VEDataValuePair::SetDataArray( VE_XML::VEFloatArray* input )
{
   if ( _dataType != std::string("FARRAY") )
   {
      std::cout<<"Invalid type passed into VEDataValuePair::SetCommandDataTransform"<<std::endl;
      return;
   }

   if( input )
   {
      if ( !_dataArray )
      {
         _dataArray = new VEFloatArray( *input );
      }
      else
      {
         *_dataArray = *input;
      }
   }
}
///////////////////////////////////////////////////////////////////
void VEDataValuePair::SetDataTransform(VE_XML::VETransform* xForm)
{
   if ( _dataType != std::string("TRANSFORM") )
   {
      std::cout<<"Invalid type passed into VEDataValuePair::SetDataTransform"<<std::endl;
      return;
   }

   if ( xForm )
   {
      if ( !_dataTransform )
      {
         _dataTransform = new VE_XML::VETransform( *xForm );
      }
      else
      {
         *_dataTransform = *xForm;
      }
   }

}
///////////////////////////////////////
void VEDataValuePair::_updateDataName()
{
   DOMElement* dataNameElement = _rootDocument->createElement(xercesString("dataName"));
   DOMText* dataName = _rootDocument->createTextNode(xercesString(_dataName.c_str()));
   dataNameElement->appendChild(dataName);
   _veElement->appendChild(dataNameElement);
}
//////////////////////////////////////////////
void VEDataValuePair::_updateDataValueNumber()
{
   DOMElement* dataValueNumElement = _rootDocument->createElement(xercesString("dataValueNum"));
   std::stringstream float2string;
   float2string<<_dataValue;
   DOMText* dataValue = _rootDocument->createTextNode( xercesString( float2string.str().c_str() ) );

   dataValueNumElement->appendChild(dataValue);
   _veElement->appendChild(dataValueNumElement);
}
//////////////////////////////////////////////
void VEDataValuePair::_updateDataValueString()
{
   DOMElement* dataValueStringElement = _rootDocument->createElement(xercesString("dataValueString"));
   DOMText* dataValueString = _rootDocument->createTextNode(xercesString(_dataString.c_str()));
   dataValueStringElement->appendChild(dataValueString);
   _veElement->appendChild(dataValueStringElement);
}
///////////////////////////////////////
void VEDataValuePair::_updateVEElement( std::string input )
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
   if(_dataType == std::string("FLOAT"))
   {
      _updateDataValueNumber();
   }
   else if(_dataType == std::string("STRING"))
   {
      _updateDataValueString();
   }
   else if(_dataType == std::string("FARRAY"))
   {
      _veElement->appendChild( _dataArray->GetXMLData( "dataArray" ) );
   }
   else if(_dataType == std::string("TRANSFORM"))
   {
      _veElement->appendChild( _dataTransform->GetXMLData( "dataTransform" ) );
   }
}
///////////////////////////////////////////////////
void VEDataValuePair::SetDataName(std::string name)
{
   if(!name.empty())
   {
      _dataName = name;
   }
}
//////////////////////////////////////////
std::string VEDataValuePair::GetDataName()
{
   return _dataName;
}
//////////////////////////////////////////
std::string VEDataValuePair::GetDataType()
{
   return _dataType;
}
////////////////////////////////////////////
std::string VEDataValuePair::GetDataString()
{
   if(_dataType == std::string("STRING"))
   {
      return _dataString;
   }
   return 0;
}
/////////////////////////////////////
double VEDataValuePair::GetDataValue()
{
   if(_dataType == std::string("FLOAT"))
   {
      return _dataValue;
   }
   return 0;
}
//////////////////////////////////////
VE_XML::VEFloatArray* VEDataValuePair::GetDataArray()
{
   if(_dataType == std::string("FARRAY"))
   {
      return _dataArray;
   }
   return 0;
}
////////////////////////////////////////////////////////
VE_XML::VETransform* VEDataValuePair::GetDataTransform()
{
   if(_dataType == std::string("TRANSFORM"))
   {
      return _dataTransform;
   }
   return 0;
}

////////////////////////////////////////////////////////////
void VEDataValuePair::SetObjectFromXMLData(DOMNode* element)
{
   DOMElement* currentElement = 0;
   if(element->getNodeType() == DOMNode::ELEMENT_NODE){
      currentElement = dynamic_cast<DOMElement*>(element);
   }

   if(currentElement){
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
         if(currentElement->getElementsByTagName(xercesString("dataValueString"))){
            subElements = currentElement->getElementsByTagName(xercesString("dataValueString"));

            DOMElement* dataValueStringName = dynamic_cast<DOMElement*>(subElements->item(0));
            if(dataValueStringName)
            {
               _dataName = ExtractDataStringFromSimpleElement(dataValueStringName);
               SetDataType(std::string("STRING"));
            }
         }else if(currentElement->getElementsByTagName(xercesString("dataValueNum"))){
            //get variables by tags
            DOMNodeList* subElements = 0;
            if(currentElement->getElementsByTagName(xercesString("dataValueNum"))){
               subElements = currentElement->getElementsByTagName(xercesString("dataValueNum"));

               DOMElement* dataValueNum = dynamic_cast<DOMElement*>(subElements->item(0));
               if(dataValueNum)
               {
                  _dataValue  = ExtractDataNumberFromSimpleElement(dataValueNum);
                  SetDataType(std::string("FLOAT"));
               }
            }
         }else if(currentElement->getElementsByTagName(xercesString("dataArray"))){
            subElements = currentElement->getElementsByTagName(xercesString("dataArray"));
            if(_dataArray)
            {
               delete _dataArray;
               _dataArray = 0;
            }
            _dataArray = new VE_XML::VEFloatArray(_rootDocument);
            _dataArray->SetObjectFromXMLData(subElements->item(0));
             SetDataType(std::string("FARRAY"));

         }else if(currentElement->getElementsByTagName(xercesString("dataTransform"))){
            subElements = currentElement->getElementsByTagName(xercesString("dataTransform"));
            if(_dataTransform)
            {
               delete _dataTransform;
               _dataTransform = 0;
            }
            _dataTransform = new VE_XML::VETransform(_rootDocument);
            _dataTransform->SetObjectFromXMLData(subElements->item(0));
             SetDataType(std::string("TRANSFORM"));
         }
      }
   }
   
}
   

