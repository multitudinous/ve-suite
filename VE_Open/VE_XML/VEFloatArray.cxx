#include <iostream>
#include <cstdlib>

#include "VE_Open/VE_XML/VEFloatArray.h"
using namespace VE_XML;
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
VEFloatArray::VEFloatArray(DOMDocument* rootDoc,unsigned int nElements)
:VEXMLObject(rootDoc)
{
   _nElements  = nElements;
   // These should match the schema for min and max occurances 
   // of the float array
   minIndex = 2;
   maxIndex = 4;
}
/////////////////////////////
//Destructor               //
/////////////////////////////
VEFloatArray::~VEFloatArray()
{
   if(_array.size())
      _array.clear();
}
///////////////////////////////////////////
VEFloatArray::VEFloatArray( const VEFloatArray& input )
:VEXMLObject(input)
{
   _nElements = input._nElements;
   _array = input._array;
   minIndex = input.minIndex;
   maxIndex = input.maxIndex;
}
/////////////////////////////////////////////////////
VEFloatArray& VEFloatArray::operator=( const VEFloatArray& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      VEXMLObject::operator =(input);
      _nElements = input._nElements;
      _array = input._array;
      minIndex = input.minIndex;
      maxIndex = input.maxIndex;
   }
   return *this;
}
/////////////////////////////////////////////////
void VEFloatArray::AddElementToArray(double value)
{
   _array.push_back(value);
   _nElements = static_cast< unsigned int >( _array.size() );
}
/////////////////////////////////////////////////////////////////
void VEFloatArray::SetArray( std::vector< double > input )
{
   if ( input.size() )
   {
      _array.clear();
      _array = input;
      _nElements = static_cast< unsigned int >( _array.size() );;
   }
}
//////////////////////////////////////////////////
double VEFloatArray::GetElement(unsigned int index)
{
   if(_array.at(index))
   {
      return _array.at(index);
   }
   std::cout<<"ERROR!!!"<<std::endl;
   std::cout<<"Invalid index: "<<index<<" in VEFloatArray::GetElement!!!"<<std::endl;
   return 0;
}
///////////////////////////////////////////////////
std::vector< double > VEFloatArray::GetArray( void )
{
   return _array;
}
////////////////////////////////////
void VEFloatArray::_updateVEElement( std::string input )
{
   if( !_veElement )
   {
      // name comes from verg.xsd
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }
   //Be sure to set the number of children (_nChildren) 
   //either here or in the updating subElements code
   //this will be based on the size of the double array
   _nChildren = static_cast< unsigned int >( _array.size() );

   //Add code here to update the specific sub elements
   for ( unsigned int i = 0; i < _array.size(); ++i )
   {
      // name comes from verg.xsd
      DOMElement* valueTag  = _rootDocument->createElement( xercesString("value") );
      _veElement->appendChild( valueTag );      
      DOMText* valueNum = _rootDocument->createTextNode( xercesString( _array.at( i ) ) );
      valueTag->appendChild( valueNum );
   }
}
////////////////////////////////////////////////////////////
void VEFloatArray::SetObjectFromXMLData(DOMNode* xmlInput)
{
   //TODO:fill in the values for the double array
   //this is currently maxed out at 4 in the schema but
   //we can adjust this to be larger if needed. Also it
   //has to be at least 2 elements according to the schema
   //_nElements = xerces->();
   DOMElement* currentElement = 0;
   if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(xmlInput);
   }
   
   if(currentElement)
   {   
      _array.clear();
      
      // do we need to delete the old one or does xerces handle this???
      //_nElements = xmlInput->getChildNodes()->getLength();
      DOMNodeList* nodeList = currentElement->getElementsByTagName(xercesString("value"));
      XMLSize_t numNodes = nodeList->getLength();
      _nElements = numNodes;
      if ( ( minIndex > numNodes ) && ( maxIndex < numNodes ) )
      {
         std::cerr << " ERROR : VEFloatArray::SetObjectFromXMLData :" << 
                     " This node has too few or too many children." << std::endl;
      }
   
      // This for loop may be wrong since the the text node and 
      // element may be seprate entities.
      // if that is the case then inside the for loop
      // we just need to get each text node child
      for ( XMLSize_t i = 0; i < numNodes; ++i )
      {
         //We know this about the node so we can cast it...
         DOMText* temp = dynamic_cast< DOMText* >( nodeList->item( i )->getFirstChild() );
         std::string stringVal( XMLString::transcode( temp->getData() ) );
         _array.push_back( std::atof( stringVal.c_str() ) );
      }
   }
   else
   {
      std::cerr << " ERROR : VEFloatArray::SetObjectFromXMLData :" << 
                  " This node has no children which means there is probably a problem." << std::endl;
   }
}

