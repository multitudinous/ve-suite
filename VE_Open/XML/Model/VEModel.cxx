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
 * File:          $RCSfile: VEModel.cxx,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/VE_XML/Model/VEModel.h"
#include "VE_Open/VE_XML/Model/VEPoint.h"
#include "VE_Open/VE_XML/Model/VEPort.h"
#include "VE_Open/VE_XML/VEDataValuePair.h"
#include "VE_Open/VE_XML/VEParameterBlock.h"
#include "VE_Open/VE_XML/CAD/CADNode.h"

using namespace VE_XML;
using namespace VE_CAD;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
VEModel::VEModel( DOMDocument* rootDoc )
:VEXMLObject( rootDoc )
{
   modelName = '\0';
   uniqueModelID = 0;
   iconFileName = '\0';
   iconLocation = new VEPoint( rootDoc );
   geometry = new CADNode( rootDoc, "oops" );
}
///////////////////////////////////
VEModel::~VEModel()
{
   for ( size_t i; i < ports.size(); ++i )
   {
      delete ports.at( i );
   }
   ports.clear();

   delete iconLocation;

   for ( size_t i; i < results.size(); ++i )
   {
      delete results.at( i );
   }
   results.clear();

   for ( size_t i; i < inputs.size(); ++i )
   {
      delete inputs.at( i );
   }
   inputs.clear();

   for ( size_t i; i < informationPackets.size(); ++i )
   {
      delete informationPackets.at( i );
   }
   informationPackets.clear();

   delete geometry;
}
///////////////////////////////////////////
VEModel::VEModel( const VEModel& input )
:VEXMLObject(input)
{
   modelName = input.modelName;
   uniqueModelID = input.uniqueModelID;
   iconFileName = input.iconFileName;

   for ( size_t i; i < input.ports.size(); ++i )
   {
      ports.push_back( new VEPort( *(input.ports.at( i )) ) );
   }

   iconLocation = new VEPoint( *(input.iconLocation) );

   for ( size_t i; i < input.results.size(); ++i )
   {
      results.push_back( new VEDataValuePair( *(input.results.at( i )) ) );
   }

   for ( size_t i; i < input.inputs.size(); ++i )
   {
      inputs.push_back( new VEDataValuePair( *(input.inputs.at( i )) ) );
   }

   for ( size_t i; i < input.informationPackets.size(); ++i )
   {
      informationPackets.push_back( new VEParameterBlock( *(input.informationPackets.at( i )) ) );
   }

   geometry = new CADNode( *(input.geometry) );
}
/////////////////////////////////////////////////////
VEModel& VEModel::operator=( const VEModel& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      VEXMLObject::operator =(input);
      modelName = input.modelName;
      uniqueModelID = input.uniqueModelID;
      iconFileName = input.iconFileName;

      for ( size_t i; i < ports.size(); ++i )
      {
         delete ports.at( i );
      }
      ports.clear();

      for ( size_t i; i < input.ports.size(); ++i )
      {
         ports.push_back( new VEPort( *(input.ports.at( i )) ) );
      }

      *iconLocation = *(input.iconLocation);

      for ( size_t i; i < results.size(); ++i )
      {
         delete results.at( i );
      }
      results.clear();

      for ( size_t i; i < input.results.size(); ++i )
      {
         results.push_back( new VEDataValuePair( *(input.results.at( i )) ) );
      }

      for ( size_t i; i < inputs.size(); ++i )
      {
         delete inputs.at( i );
      }
      inputs.clear();

      for ( size_t i; i < input.inputs.size(); ++i )
      {
         inputs.push_back( new VEDataValuePair( *(input.inputs.at( i )) ) );
      }

      for ( size_t i; i < informationPackets.size(); ++i )
      {
         delete informationPackets.at( i );
      }
      informationPackets.clear();

      for ( size_t i; i < input.informationPackets.size(); ++i )
      {
         informationPackets.push_back( new VEParameterBlock( *(input.informationPackets.at( i )) ) );
      }

      *geometry = *(input.geometry);
   }
   return *this;
}
////////////////////////////////////////////////////////////
void VEModel::SetModelName( std::string name )
{
   modelName = name;
}
////////////////////////////////////////////////////////////
void VEModel::SetModelID( unsigned int id )
{
   uniqueModelID = id;
}
////////////////////////////////////////////////////////////
void VEModel::SetIconFilename( std::string filename )
{
   iconFileName = filename;
}
////////////////////////////////////////////////////////////
void VEModel::SetObjectFromXMLData(DOMNode* element)
{
   DOMElement* currentElement = 0;
   if( element->getNodeType() == DOMNode::ELEMENT_NODE )
   {
      currentElement = dynamic_cast< DOMElement* >( element );
   }

   if ( currentElement )
   {
      //get variables by tags
      DOMElement* dataValueStringName = 0;

      {
         dataValueStringName = GetSubElement( currentElement, "name", 0 );
         modelName = ExtractDataStringFromSimpleElement( dataValueStringName );
      }

      {
         dataValueStringName = GetSubElement( currentElement, "ID", 0 );
         uniqueModelID = ExtractIntegerDataNumberFromSimpleElement( dataValueStringName );
      }

      {
         dataValueStringName = GetSubElement( currentElement, "icon", 0 );
         iconFileName = ExtractDataStringFromSimpleElement( dataValueStringName );
      }

      {
         dataValueStringName = GetSubElement( currentElement, "iconLocation", 0 );
         if ( iconLocation )
         {
            delete iconLocation;
            iconLocation = 0;
         }
         iconLocation = new VE_XML::VEPoint( _rootDocument );
         iconLocation->SetObjectFromXMLData( dataValueStringName );
      }

      {
         dataValueStringName = GetSubElement( currentElement, "geometry", 0 );
         if ( geometry )
         {
            delete geometry;
            geometry = 0;
         }
         geometry = new CADNode( _rootDocument, "oops"  );
         geometry->SetObjectFromXMLData( dataValueStringName );
      }

      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("ports") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "ports", i );
            ports.push_back( new VEPort( _rootDocument ) );
            ports.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }

      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("results") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "results", i );
            results.push_back( new VEDataValuePair( _rootDocument ) );
            results.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }

      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("inputs") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "inputs", i );
            results.push_back( new VEDataValuePair( _rootDocument ) );
            results.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }

      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("informationPackets") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "informationPackets", i );
            informationPackets.push_back( new VEParameterBlock( _rootDocument ) );
            informationPackets.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }
   }   
}
////////////////////////////////////////////////////////////
VEPoint* VEModel::GetIconLocation( void )
{
   return iconLocation;
}
////////////////////////////////////////////////////////////
VEDataValuePair* VEModel::GetResult( unsigned int i )
{
   try
   {
      return results.at( i );
   }
   catch (...)
   {
      if ( i > (results.size() + 1) )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         results.push_back( new VEDataValuePair( _rootDocument ) );
         return results.back();
      }
   }
}
////////////////////////////////////////////////////////////
unsigned int VEModel::GetNumberOfResults( void )
{
   return results.size();
}
////////////////////////////////////////////////////////////
VEDataValuePair* VEModel::GetInput( unsigned int i )
{
   try
   {
      return inputs.at( i );
   }
   catch (...)
   {
      if ( i > (inputs.size() + 1) )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         inputs.push_back( new VEDataValuePair( _rootDocument ) );
         return inputs.back();
      }
   }
}
////////////////////////////////////////////////////////////
unsigned int VEModel::GetNumberOfInputs( void )
{
   return inputs.size();
}
////////////////////////////////////////////////////////////
VEPort* VEModel::GetPort( unsigned int i )
{
   try
   {
      return ports.at( i );
   }
   catch (...)
   {
      if ( i > (ports.size() + 1) )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         ports.push_back( new VEPort( _rootDocument ) );
         return ports.back();
      }
   }
}
////////////////////////////////////////////////////////////
unsigned int VEModel::GetNumberOfPorts( void )
{
   return ports.size();
}
////////////////////////////////////////////////////////////
VEParameterBlock* VEModel::GetInformationPacket( unsigned int i )
{
   try
   {
      return informationPackets.at( i );
   }
   catch (...)
   {
      if ( i > (informationPackets.size() + 1) )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         informationPackets.push_back( new VEParameterBlock( _rootDocument ) );
         return informationPackets.back();
      }
   }
}
////////////////////////////////////////////////////////////
unsigned int VEModel::GetNumberOfInformationPackets( void )
{
   return informationPackets.size();
}
////////////////////////////////////////////////////////////
CADNode* VEModel::GetGeometry( void )
{
   return geometry;
}
///////////////////////////////////////
void VEModel::_updateVEElement( std::string input )
{
   //fill in datavalue pairs, via xerces, according to schema
   if ( !_veElement )
   {
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }

   // write all the elements according to verg_model.xsd
   for ( size_t i; i < ports.size(); ++i )
   {
      SetSubElement( "ports", ports.at( i ) );   
   }

   SetSubElement( "iconLocation", iconLocation );
   SetSubElement( "name", modelName );
   SetSubElement( "ID", uniqueModelID );
   SetSubElement( "icon", iconFileName );

   for ( size_t i; i < results.size(); ++i )
   {
      SetSubElement( "results", results.at( i ) );   
   }

   for ( size_t i; i < inputs.size(); ++i )
   {
      SetSubElement( "inputs", inputs.at( i ) );   
   }

   for ( size_t i; i < informationPackets.size(); ++i )
   {
      SetSubElement( "informationPackets", informationPackets.at( i ) );   
   }

   SetSubElement( "geometry", geometry );   
}
