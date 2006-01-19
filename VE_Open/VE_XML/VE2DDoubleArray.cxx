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
 * File:          $RCSfile: VE2DDoubleArray.cxx,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <iostream>
#include <cstdlib>

#include "VE_Open/VE_XML/VE2DDoubleArray.h"
#include "VE_Open/VE_XML/VE1DDoubleArray.h"
using namespace VE_XML;
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
VE2DDoubleArray::VE2DDoubleArray(DOMDocument* rootDoc,unsigned int nElements)
:VEXMLObject(rootDoc)
{
   _nElements  = nElements;
   // These should match the schema for min and max occurances 
   // of the float array
   minIndex = 2;
}
/////////////////////////////
//Destructor               //
/////////////////////////////
VE2DDoubleArray::~VE2DDoubleArray()
{
   for ( size_t i = 0; i < ve1DDoubleArray.size(); ++i )
   {
      delete ve1DDoubleArray.at( i );
   }
   ve1DDoubleArray.clear();
}
///////////////////////////////////////////
VE2DDoubleArray::VE2DDoubleArray( const VE2DDoubleArray& input )
:VEXMLObject(input)
{
   _nElements  = input._nElements;
   doubleArray = input.doubleArray;
   for ( size_t i = 0; i < input.ve1DDoubleArray.size(); ++i )
   {
      ve1DDoubleArray.push_back( new VE1DDoubleArray( *(input.ve1DDoubleArray.at( i )) ) );
   }
   minIndex = input.minIndex;
}
/////////////////////////////////////////////////////
VE2DDoubleArray& VE2DDoubleArray::operator=( const VE2DDoubleArray& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      VEXMLObject::operator =(input);
      _nElements = input._nElements;
      doubleArray = input.doubleArray;
      minIndex = input.minIndex;

      for ( size_t i = 0; i < ve1DDoubleArray.size(); ++i )
      {
         delete ve1DDoubleArray.at( i );
      }
      ve1DDoubleArray.clear();

      for ( size_t i = 0; i < input.ve1DDoubleArray.size(); ++i )
      {
         ve1DDoubleArray.push_back( new VE1DDoubleArray( *(input.ve1DDoubleArray.at( i )) ) );
      }
   }
   return *this;
}
/////////////////////////////////////////////////
void VE2DDoubleArray::AddElementToArray( std::vector< double > value )
{
   doubleArray.push_back(value);
   ve1DDoubleArray.push_back( new VE1DDoubleArray( _rootDocument ) );
   ve1DDoubleArray.back()->SetArray( doubleArray.back() );
   _nElements = static_cast< unsigned int >( doubleArray.size() );
}
/////////////////////////////////////////////////
void VE2DDoubleArray::AddElementToArray( VE1DDoubleArray* value )
{
   ve1DDoubleArray.push_back( value );
   doubleArray.push_back( ve1DDoubleArray.back()->GetArray() );
   _nElements = static_cast< unsigned int >( doubleArray.size() );
}
/////////////////////////////////////////////////////////////////
void VE2DDoubleArray::SetArray( std::vector< std::vector< double > > input )
{
   doubleArray.clear();
   doubleArray = input;
   _nElements = static_cast< unsigned int >( doubleArray.size() );
   // Clean old vector int
   for ( size_t i = 0; i < doubleArray.size(); ++i )
   {
      delete ve1DDoubleArray.at( i );
   }
   ve1DDoubleArray.clear();
   
   for ( size_t i = 0; i < doubleArray.size(); ++i )
   {
      ve1DDoubleArray.push_back( new VE1DDoubleArray( _rootDocument ) );
      ve1DDoubleArray.back()->SetArray( doubleArray.at( i ) );
   }
}
//////////////////////////////////////////////////
double VE2DDoubleArray::GetElement(unsigned int i, unsigned int j)
{
   try
   {
      return doubleArray.at( i ).at( j );
   }
   catch (...)
   {
      std::cout<< " ERROR!!! "<< std::endl
               << " Invalid index: "<< i << "," << j <<" in VE2DDoubleArray::GetElement!!!"<<std::endl;
      return 0;
   }
}
///////////////////////////////////////////////////
std::vector< std::vector< double > > VE2DDoubleArray::GetArray( void )
{
   return doubleArray;
}
////////////////////////////////////
void VE2DDoubleArray::_updateVEElement( std::string input )
{
   if( !_veElement )
   {
      // name comes from verg.xsd
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }
   //Be sure to set the number of children (_nChildren) 
   //either here or in the updating subElements code
   //this will be based on the size of the double array
   _nChildren = static_cast< unsigned int >( doubleArray.size() );

   //Add code here to update the specific sub elements
   // This acutally needs to be an array of 1d arrays
   for ( size_t i = 0; i < ve1DDoubleArray.size(); ++i )
   {
      // name comes from verg.xsd
      _veElement->appendChild( ve1DDoubleArray.at( i )->GetXMLData( "index2" ) );      
   }
}
////////////////////////////////////////////////////////////
void VE2DDoubleArray::SetObjectFromXMLData(DOMNode* xmlInput)
{
   //TODO:fill in the values for the double array
   //this is currently maxed out at 4 in the schema but
   //we can adjust this to be larger if needed. Also it
   //has to be at least 2 elements according to the schema
   //_nElements = xerces->();
   DOMElement* currentElement = 0;
   if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast< DOMElement* >(xmlInput);
   }
   
   if ( currentElement )
   {   
      doubleArray.clear();
     
      for ( size_t i = 0; i < ve1DDoubleArray.size(); ++i )
      {
         delete ve1DDoubleArray.at( i );
      }
      ve1DDoubleArray.clear();

      // do we need to delete the old one or does xerces handle this???
      //_nElements = xmlInput->getChildNodes()->getLength();
      DOMNodeList* nodeList = currentElement->getElementsByTagName(xercesString("index2"));
      XMLSize_t numNodes = nodeList->getLength();
      _nElements = numNodes;
      if ( minIndex > numNodes )
      {
         std::cerr << " ERROR : VE2DDoubleArray::SetObjectFromXMLData :" << 
                     " This node has too few or too many children." << std::endl;
      }
   
      // This for loop may be wrong since the the text node and 
      // element may be seprate entities.
      // if that is the case then inside the for loop
      // we just need to get each text node child

      // need to get 1d arrays from 1darray class and construct the raw vectors
      for ( XMLSize_t i = 0; i < numNodes; ++i )
      {
         //We know this about the node so we can cast it...
         ve1DDoubleArray.push_back( new VE1DDoubleArray( _rootDocument ) );
         ve1DDoubleArray.back()->SetObjectFromXMLData( nodeList->item( i ) );
         doubleArray.push_back( ve1DDoubleArray.back()->GetArray() );
      }
   }
   else
   {
      std::cerr << " ERROR : VE2DDoubleArray::SetObjectFromXMLData :" << 
                  " This node has no children which means there is probably a problem." << std::endl;
   }
}

