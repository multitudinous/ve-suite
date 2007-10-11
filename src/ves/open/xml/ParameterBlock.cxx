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

#include "ves/open/xml/ParameterBlock.h"
#include "ves/open/xml/Transform.h"
#include "ves/open/xml/DataValuePair.h"
XERCES_CPP_NAMESPACE_USE
#include <iostream>

using namespace VE_XML;

////////////////////////////////////////////////////
ParameterBlock::ParameterBlock(unsigned int id)
:XMLObject()
{
   _id = id;
   _dcs = new Transform();
   SetName("NULL");
   SetObjectType("ParameterBlock");
}
/////////////////////////////////////
ParameterBlock::~ParameterBlock()
{
   delete _dcs;
   _dcs = 0;

   if(_properties.size())
   {
      size_t nProps = _properties.size();
      for(size_t i = nProps - 1; i > -1; i--)
      {
         delete _properties.at(i);
      }
      _properties.clear();
   }
}
///////////////////////////////////////////
ParameterBlock::ParameterBlock( const ParameterBlock& input )
:XMLObject(input)
{
   _dcs = new Transform( *input._dcs );
   _id = input._id;
   paramName = input.paramName;

   for ( size_t i = 0; i < input._properties.size(); ++i )
   {
      _properties.push_back( new DataValuePair( *(input._properties.at(i)) ) );
   }
}
/////////////////////////////////////////////////////
ParameterBlock& ParameterBlock::operator=( const ParameterBlock& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      XMLObject::operator =(input);
      *_dcs = *input._dcs;
      _id = input._id;
      paramName = input.paramName;

      for ( size_t i = 0; i < _properties.size(); ++i )
      {
         delete _properties.at(i);
      }
      _properties.clear();

      for ( size_t i = 0; i < input._properties.size(); ++i )
      {
         _properties.push_back( new DataValuePair( *(input._properties.at(i)) ) );
      }
   }
   return *this;
}
////////////////////////////////////////////////
void ParameterBlock::SetBlockId(unsigned int id)
{
   _id = id;
}
///////////////////////////////////////////////////////////////////
void ParameterBlock::SetTransform(VE_XML::Transform* transform)
{
   *_dcs = *transform;
}
/////////////////////////////////////////////////////////////////
void ParameterBlock::AddProperty(VE_XML::DataValuePair* prop)
{
   _properties.push_back(prop);
}
//////////////////////////////////////////////////////////////////
//set the data from an string representing the xml              //
//////////////////////////////////////////////////////////////////
void ParameterBlock::SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput )
{
   //this will be tricky...
   DOMElement* currentElement = 0;
   if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
   {
      currentElement = dynamic_cast< DOMElement* >( xmlInput );
   }

   if ( currentElement )
   {
      if(currentElement->hasAttribute(xercesString("id")))
      {
         std::string emptyCheck;
         VE_XML::XMLObject::GetAttribute(currentElement,"id",emptyCheck);
         if(!emptyCheck.empty())
         {
            uuid = emptyCheck;
         }
      }
      //get variables by tags
      DOMElement* dataValueStringName = 0;
      //get the transform
      dataValueStringName = GetSubElement( currentElement, "transform", 0 );
      if ( _dcs )
      {
         delete _dcs;
         _dcs = 0;
      }
      _dcs = new Transform();
      _dcs->SetObjectFromXMLData( dataValueStringName );
      
      //Get the block id
      dataValueStringName = GetSubElement( currentElement, "blockID", 0 );
      _id = ExtractFromSimpleElement< unsigned int >( dataValueStringName );
      //Get the block name
      dataValueStringName = GetSubElement( currentElement, "blockName", 0 );
      paramName = ExtractFromSimpleElement< std::string >( dataValueStringName );
      //Get the properties
      for ( size_t i = 0; i < _properties.size(); ++i )
      {
         delete _properties.at( i );
      }
      _properties.clear();

      unsigned int numberOfProperties = currentElement->getElementsByTagName( xercesString("properties") )->getLength();
      for ( unsigned int i = 0; i < numberOfProperties; ++i )
      {
         dataValueStringName = GetSubElement( currentElement, "properties", i );
         _properties.push_back( new DataValuePair() );
         _properties.back()->SetObjectFromXMLData( dataValueStringName );
      }
   }
}
////////////////////////////////////////
void ParameterBlock::_updateVEElement( std::string input )
{
   //Add code here to update the specific sub elements
   SetSubElement( "blockID", _id );
   SetSubElement( "blockName", paramName );
   SetAttribute( "id", uuid );
   DOMElement* tempElement;
   tempElement = SetSubElement( "transform", _dcs );
   SetAttribute( "objectType", _dcs->GetObjectType(), tempElement );
   for ( size_t i = 0; i < _properties.size(); ++i )
   {
      tempElement = SetSubElement( "properties", _properties.at( i ) );
      SetAttribute( "objectType", _properties.at( i )->GetObjectType(), tempElement );
   }
}
/////////////////////////////////////////
unsigned int ParameterBlock::GetBlockId()
{
   return _id;
}
/////////////////////////////////////////////////////
VE_XML::Transform* ParameterBlock::GetTransform()
{
   return _dcs;
}
///////////////////////////////////////////////////////////////////////
VE_XML::DataValuePair* ParameterBlock::GetProperty( std::string name )
{
   size_t nProps = _properties.size();
   for ( size_t i = 0; i < nProps; i++)
   {
      if(_properties.at(i)->GetDataName() == name)
      {
         return _properties.at(i);
      }
   }
   /*
   _properties.push_back( new DataValuePair() );
   _properties.back()->SetDataName( name );
   */
   return 0;
}
/////////////////////////////////////////////////////////////////////////
VE_XML::DataValuePair* ParameterBlock::GetProperty( int index )
{
   try
   {
      return _properties.at( index );
   }
   catch (...)
   {
      if ( index >= 0 )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         _properties.push_back( new DataValuePair() );
         return _properties.back();
      }
   }
}
/////////////////////////////////////////////////////////////////////////
size_t ParameterBlock::GetNumberOfProperties( void )
{
   return _properties.size();
}
/////////////////////////////////////////////////////////////////////////
void ParameterBlock::RemoveProperty( unsigned int index )
{
   if ( index >= _properties.size() )
   {
      return;
   }

   std::vector< VE_XML::DataValuePair* >::iterator iter;
   for ( iter = _properties.begin(); iter != _properties.end(); ++iter )
   {
      if ( _properties.at( index ) == (*iter) )
      {
         delete _properties.at( index );
         _properties.erase( iter );
         break;
      }
   }
}
/////////////////////////////////////////////////////////////////////////
void ParameterBlock::SetName( std::string name )
{
   paramName = name;
}
/////////////////////////////////////////////////////////////////////////
std::string ParameterBlock::GetName( void )
{
   return paramName;
}

