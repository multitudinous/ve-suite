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

#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Port.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAssembly.h"

#include <sstream>

XERCES_CPP_NAMESPACE_USE
using namespace VE_XML;
using namespace VE_XML::VE_CAD;
using namespace VE_XML::VE_Model;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
Model::Model()
:XMLObject( )
{
   //modelName = '\0';
   uniqueModelID = 0;
   //iconFileName = '\0';
   iconLocation = new Point();
   geometry = 0;
   SetObjectType("Model");
   SetObjectNamespace("Model");
   //vendorUnit = '\0';
   modelAttribute = 0;
   iconScale = 1.0f;
   iconRotation = 0.0f;
   iconMirror = 0;
}
///////////////////////////////////
Model::~Model()
{
   for ( size_t i = 0; i < ports.size(); ++i )
   {
      delete ports.at( i );
   }
   ports.clear();

   delete iconLocation;

   for ( size_t i = 0; i < results.size(); ++i )
   {
      delete results.at( i );
   }
   results.clear();

   for ( size_t i = 0; i < inputs.size(); ++i )
   {
      delete inputs.at( i );
   }
   inputs.clear();

   for ( size_t i = 0; i < informationPackets.size(); ++i )
   {
      delete informationPackets.at( i );
   }
   informationPackets.clear();

   if ( geometry )
   {   
      delete geometry;
      geometry = 0;
   }
   
   if ( modelAttribute )
   {
      delete modelAttribute;
      modelAttribute = 0;
   }
}
///////////////////////////////////////////
Model::Model( const Model& input )
:XMLObject(input)
{
   modelName = input.modelName;
   uniqueModelID = input.uniqueModelID;
   iconFileName = input.iconFileName;
   vendorUnit = input.vendorUnit;
   iconScale = input.iconScale;
   iconRotation = input.iconRotation;
   iconMirror = input.iconMirror;
   
   for ( size_t i = 0; i < input.ports.size(); ++i )
   {
      ports.push_back( new Port( *(input.ports.at( i )) ) );
   }

   iconLocation = new Point( *(input.iconLocation) );

   for ( size_t i = 0; i < input.results.size(); ++i )
   {
      results.push_back( new Command( *(input.results.at( i )) ) );
   }

   for ( size_t i = 0; i < input.inputs.size(); ++i )
   {
      inputs.push_back( new Command( *(input.inputs.at( i )) ) );
   }

   for ( size_t i = 0; i < input.informationPackets.size(); ++i )
   {
      informationPackets.push_back( new ParameterBlock( *(input.informationPackets.at( i )) ) );
   }

   geometry = 0;
   if ( input.geometry )
   {
      geometry = new CADAssembly( *(input.geometry) );
   }

   modelAttribute = 0;
   if ( input.modelAttribute )
   {
      modelAttribute = new Command( *(input.modelAttribute) );
   }
}
/////////////////////////////////////////////////////
Model& Model::operator=( const Model& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      XMLObject::operator =(input);
      modelName = input.modelName;
      uniqueModelID = input.uniqueModelID;
      iconFileName = input.iconFileName;
      vendorUnit = input.vendorUnit;
      iconScale = input.iconScale;
      iconRotation = input.iconRotation;
      iconMirror = input.iconMirror;
      
      for ( size_t i = 0; i < ports.size(); ++i )
      {
         delete ports.at( i );
      }
      ports.clear();

      for ( size_t i = 0; i < input.ports.size(); ++i )
      {
         ports.push_back( new Port( *(input.ports.at( i )) ) );
      }

      *iconLocation = *(input.iconLocation);

      for ( size_t i = 0; i < results.size(); ++i )
      {
         delete results.at( i );
      }
      results.clear();

      for ( size_t i = 0; i < input.results.size(); ++i )
      {
         results.push_back( new Command( *(input.results.at( i )) ) );
      }

      for ( size_t i = 0; i < inputs.size(); ++i )
      {
         delete inputs.at( i );
      }
      inputs.clear();

      for ( size_t i = 0; i < input.inputs.size(); ++i )
      {
         inputs.push_back( new Command( *(input.inputs.at( i )) ) );
      }

      for ( size_t i = 0; i < informationPackets.size(); ++i )
      {
         delete informationPackets.at( i );
      }
      informationPackets.clear();

      for ( size_t i = 0; i < input.informationPackets.size(); ++i )
      {
         informationPackets.push_back( new ParameterBlock( *(input.informationPackets.at( i )) ) );
      }

      if ( input.geometry )
      {
         if ( !geometry )
         {
            geometry = new CADAssembly();
         }
         *geometry = *(input.geometry);
      }

      if ( input.modelAttribute )
      {
         if ( !modelAttribute )
         {
            modelAttribute = new Command();
         }
         *modelAttribute = *(input.modelAttribute);
      }
   }
   return *this;
}
////////////////////////////////////////////////////////////
void Model::SetModelName( std::string name )
{
   modelName = name;
}
////////////////////////////////////////////////////////////
void Model::SetModelID( unsigned int id )
{
   uniqueModelID = id;
}
////////////////////////////////////////////////////////////
void Model::SetIconFilename( std::string filename )
{
   iconFileName = filename;
}
////////////////////////////////////////////////////////////
void Model::SetObjectFromXMLData(DOMNode* element)
{
   DOMElement* currentElement = 0;
   if( element->getNodeType() == DOMNode::ELEMENT_NODE )
   {
      currentElement = dynamic_cast< DOMElement* >( element );
   }

   if( currentElement )
   {
       //Setup uuid for model element
       {
           std::string tempUuid;
           VE_XML::XMLObject::GetAttribute(currentElement, "id", tempUuid);
           if( !tempUuid.empty() )
           {
               uuid = tempUuid;
           }
       }
      //get variables by tags
      DOMElement* dataValueStringName = 0;

      {
         dataValueStringName = GetSubElement( currentElement, "name", 0 );
         if ( dataValueStringName )
         {
            modelName = ExtractFromSimpleElement< std::string >( dataValueStringName );
            dataValueStringName = 0;            
         }
         else
         {
            GetAttribute( currentElement, "name", modelName );
         }
      }

      GetAttribute( currentElement, "vendorUnit", vendorUnit );

      {
         dataValueStringName = GetSubElement( currentElement, "ID", 0 );
         if ( dataValueStringName )
         {
            uniqueModelID = ExtractFromSimpleElement< unsigned int >( dataValueStringName );
         }
         else
         {
            std::string idString;
            GetAttribute( currentElement, "ID", idString );  
            std::istringstream inputStream( idString );
            inputStream >> uniqueModelID;

         }
      }

      {
         dataValueStringName = GetSubElement( currentElement, "icon", 0 );
         if ( dataValueStringName )
         {
            iconFileName = ExtractFromSimpleElement< std::string >( dataValueStringName );
         }
         else
         {
            iconFileName = std::string( "no_icon" );
         }
         GetAttribute( dataValueStringName, "iconScale", iconScale );  
         GetAttribute( dataValueStringName, "iconRotation", iconRotation );    
         GetAttribute( dataValueStringName, "iconMirror", iconMirror );          
      }

      {
         dataValueStringName = GetSubElement( currentElement, "iconLocation", 0 );
         if ( iconLocation )
         {
            delete iconLocation;
            iconLocation = 0;
         }
         iconLocation = new Point();
         iconLocation->SetObjectFromXMLData( dataValueStringName );
      }

      //get the geometry nodes
      {
         if ( currentElement->getElementsByTagName( xercesString("geometry") )->getLength() > 0 )
         {
            dataValueStringName = GetSubElement( currentElement, "geometry", 0 );
            if ( geometry )
            {
               delete geometry;
               geometry = 0;
            }
            geometry = new CADAssembly("oops" );
            geometry->SetObjectFromXMLData( dataValueStringName );
         }
      }

      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("ports") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "ports", i );
            ports.push_back( new Port(  ) );
            ports.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }

      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("results") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "results", i );
            results.push_back( new Command(  ) );
            results.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }

      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("inputs") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "inputs", i );
            inputs.push_back( new Command(  ) );
            inputs.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }

      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("informationPackets") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "informationPackets", i );
            informationPackets.push_back( new ParameterBlock(  ) );
            informationPackets.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }

      //get the model attribute nodes
      {
         if ( currentElement->getElementsByTagName( xercesString("modelAttributes") )->getLength() > 0 )
         {
            dataValueStringName = GetSubElement( currentElement, "modelAttributes", 0 );
            if ( modelAttribute )
            {
               delete modelAttribute;
               modelAttribute = 0;
            }
            modelAttribute = new Command();
            modelAttribute->SetObjectFromXMLData( dataValueStringName );
         }
      }
   }   
}
////////////////////////////////////////////////////////////
std::string Model::GetModelName( void )
{
   return modelName;
}
////////////////////////////////////////////////////////////
unsigned int Model::GetModelID( void )
{
   return uniqueModelID;
}
////////////////////////////////////////////////////////////
std::string Model::GetIconFilename( void )
{
   return iconFileName;
}
////////////////////////////////////////////////////////////
Point* Model::GetIconLocation( void )
{
   return iconLocation;
}
////////////////////////////////////////////////////////////
Command* Model::GetResult( int i )
{
   try
   {
      return results.at( i );
   }
   catch (...)
   {
      if ( i >= 0 )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      //else
      {
         results.push_back( new Command(  ) );
         return results.back();
      }
   }
}
////////////////////////////////////////////////////////////
size_t Model::GetNumberOfResults( void )
{
   return results.size();
}
////////////////////////////////////////////////
Command* Model::GetInput(std::string inputName)
{
   for(size_t i = 0; i < inputs.size(); i++)
   {
      if(inputs.at(i)->GetCommandName() == inputName)
      {
         return inputs.at(i);
      }
   }
   return 0;
}
////////////////////////////////////////////////////////////
Command* Model::GetInput( int i )
{
   try
   {
      return inputs.at( i );
   }
   catch (...)
   {
      if ( i >= 0 )
      {
         std::cerr << " Model::GetInput The element request is out of sequence."
            << " Please ask for a lower number point or -1 to request new element." << std::endl;
      }  
      inputs.push_back( new Command() );
      return inputs.back();
   }
}
////////////////////////////////////////////////////////////
VE_XML::Command* Model::GetInput( void )
{
   inputs.push_back( new Command() );
   return inputs.back();
}
////////////////////////////////////////////////////////////
size_t Model::GetNumberOfInputs( void )
{
   return inputs.size();
}
////////////////////////////////////////////////////////////
Port* Model::GetPort(int i )
{
   try
   {
      return ports.at( i );
   }
   catch (...)
   {
      //if ( i > (ports.size() + 1) )
	  if ( i >=0)
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         ports.push_back( new Port(  ) );
         return ports.back();
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
Port* Model::GetPort( void )
{
   ports.push_back( new Port() );
   return ports.back();
}
////////////////////////////////////////////////////////////////////////////////
size_t Model::GetNumberOfPorts( void )
{
   return ports.size();
}
////////////////////////////////////////////////////////////////////////////////
void Model::RemovePort( unsigned int i )
{
    size_t count = 0;
    for( std::vector< Port* >::iterator iter = ports.begin(); 
        iter != ports.end(); ++iter )
    {
        if( count == i )
        {
            ports.erase( iter );
            break;
        }
        ++count;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Model::RemovePort( VE_XML::VE_Model::Port* removePort )
{
    for( std::vector< Port* >::iterator iter = ports.begin(); 
        iter != ports.end(); ++iter )
    {
        if( *iter == removePort )
        {
            ports.erase( iter );
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
/*////////////////////////////////////////////////////////////
Port* Model::GetOutputPort( unsigned int i )
{
   try
   {
      return outputPorts.at( i );
   }
   catch (...)
   {
      if ( i > ( outputPorts.size() + 1) )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         ports.push_back( new Port() );
         outputPorts.push_back( ports.back() );
         return ports.back();
      }
   }
}
////////////////////////////////////////////////////////////
size_t Model::GetNumberOfOutputPorts( void )
{
   return outputPorts.size();
}*/
//////////////////////////////////////////////////////////////////////
VE_XML::ParameterBlock* Model::GetInformationPacket( std::string name)
{

   for(std::vector<VE_XML::ParameterBlock*>::iterator iter = informationPackets.begin();
      iter != informationPackets.end(); ++iter)
   {
      if((*iter)->GetName() == name)
      {
         return (*iter);
      }
   }
   //std::cout<<"Parameter Block: "<<name<<std::endl;
   //std::cout<<"not found in Model: "<<uuid<<std::endl;
   return 0;
}
////////////////////////////////////////////////////////////
ParameterBlock* Model::GetInformationPacket( int i )
{
   try
   {
      return informationPackets.at( i );
   }
   catch (...)
   {
      if ( i >= 0 )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         informationPackets.push_back( new ParameterBlock(  ) );
         return informationPackets.back();
      }
   }
}
////////////////////////////////////////////////////////////
size_t Model::GetNumberOfInformationPackets( void )
{
   return informationPackets.size();
}
////////////////////////////////////////////////////////////
CADNode* Model::GetGeometry( void )
{
   return geometry;
}
////////////////////////////////////////////////////////////
CADNode* Model::AddGeometry( void )
{
   if ( geometry == 0 )
      geometry = new CADAssembly( "Model_Geometry" );

   return geometry;
}
////////////////////////////////////////////////////////////
void Model::DeleteGeometry( void )
{
   if ( geometry )
   {
      delete geometry;
      geometry = 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
void Model::RemoveInformationPacket( unsigned int i )
{
   std::vector< VE_XML::ParameterBlock* >::iterator iter;
   for ( iter = informationPackets.begin(); iter != informationPackets.end(); ++iter )
   {
      if ( informationPackets.at( i ) == (*iter) )
      {
         delete informationPackets.at( i );
         informationPackets.erase( iter );
      }
   }
}
//////////////////////////////////////////////////////
void Model::RemoveInformationPacket(std::string name )
{
   for(std::vector<VE_XML::ParameterBlock*>::iterator iter = informationPackets.begin();
      iter != informationPackets.end(); ++iter)
   {
      if((*iter)->GetName() == name)
      {
         delete *iter;
         informationPackets.erase( ++iter );
         return;
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void Model::_updateVEElement( std::string input )
{
   // write all the elements according to verg_model.xsd
   for ( size_t i = 0; i < ports.size(); ++i )
   {
      SetSubElement( "ports", ports.at( i ) );   
   }

   SetSubElement( "iconLocation", iconLocation );
   SetAttribute( "name", modelName );
   SetAttribute( "id", uuid );
   
   if ( vendorUnit.empty() )
   {
      vendorUnit = modelName;
   }
   SetAttribute( "vendorUnit", vendorUnit );
   //SetSubElement( "name", modelName );
   std::ostringstream dirStringStream;
   dirStringStream << uniqueModelID;
   SetAttribute( "ID", dirStringStream.str() );
   //SetSubElement( "ID", uniqueModelID );
   DOMElement* iconElement = SetSubElement( "icon", iconFileName );

   {
      std::stringstream int2string;
      int2string << iconScale;
      iconElement->setAttribute( xercesString( "iconScale" ), xercesString( int2string.str().c_str() )  );
   }

   {
      std::stringstream int2string;
      int2string << iconRotation;
      iconElement->setAttribute( xercesString( "iconRotation" ), xercesString( int2string.str().c_str() )  );      
   }

   {
      std::stringstream int2string;
      int2string << iconMirror;
      iconElement->setAttribute( xercesString( "iconMirror" ), xercesString( int2string.str().c_str() )  );      
   }
   
   for ( size_t i = 0; i < results.size(); ++i )
   {
      SetSubElement( "results", results.at( i ) );   
   }

   for ( size_t i = 0; i < inputs.size(); ++i )
   {
      SetSubElement( "inputs", inputs.at( i ) );   
   }

   for ( size_t i = 0; i < informationPackets.size(); ++i )
   {
      SetSubElement( "informationPackets", informationPackets.at( i ) );   
   }

   if ( geometry )
   {   
      SetSubElement( "geometry", geometry );   
   }

   if ( modelAttribute )
   {
      SetSubElement( "modelAttributes", modelAttribute );   
   }
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetVendorName( std::string vendorName )
{
   vendorUnit = vendorName;
}
////////////////////////////////////////////////////////////////////////////////
std::string Model::GetVendorName( void )
{
   return vendorUnit;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetModelAttribute( VE_XML::Command* modelAttribute )
{
   this->modelAttribute = modelAttribute;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::Command* Model::GetModelAttribute( void )
{
   return this->modelAttribute;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetIconRotation( float rotation )
{
   iconRotation = rotation;
}
////////////////////////////////////////////////////////////////////////////////
float Model::GetIconRotation( void )
{
   return iconRotation;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetIconScale( float scale )
{
   iconScale = scale;
}
////////////////////////////////////////////////////////////////////////////////
float Model::GetIconScale( void )
{
   return iconScale;
}
////////////////////////////////////////////////////////////////////////////////
void Model::SetIconMirror( int mirror )
{
   iconMirror = mirror;
}
////////////////////////////////////////////////////////////////////////////////
int Model::GetIconMirror( void )
{
   return iconMirror;
}