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
 * File:          $RCSfile: DataLoader.cxx,v $
 * Date modified: $Date: 2006-03-18 17:23:46 -0600 (Sat, 18 Mar 2006) $
 * Version:       $Rev: 3936 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Builder/Translator/DataLoader.h"

//////////////////////
//Constructor       //
//////////////////////
DataLoader::DataLoader()
{
   inputDataName =  '\0';
   // load up the translator map
}
///////////////////////
DataLoader::~DataLoader()
{
   // Clear the translator map
   //translatorMap
}
///////////////////////////////////////////
DataLoader::DataLoader( const DataLoader& input )
{
   ;
}
/////////////////////////////////////////////////////
DataLoader& DataLoader::operator=( const DataLoader& input)
{
   if ( this != &input )
   {
      ;
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////////
void DataLoader::AddDataValuePair(VE_XML::DataValuePair* commandValuePair)
{
   _dataValuePairs.push_back(commandValuePair);
   _nDataValuePairs = static_cast< unsigned int >( _dataValuePairs.size() );
   nameToDataValuePairMap[ _dataValuePairs.back()->GetDataName() ] = commandValuePair;
}
/////////////////////////////////
void DataLoader::_updateVEElement( std::string input )
{
   //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
   //_nChildren will be the number of dvPairs + the name of the command but we have to call the
   //update functions below to get the ndvPairs before we can calculate _nChildren

   //Add code here to update the specific sub elements
   _updateCommandName();
   _updateDataValuePairs();

   //_nChildren = 1 + _dataValuePairs.size();
}
////////////////////////////////////
void DataLoader::_updateCommandName()
{
   DOMElement* cmdNameElement = _rootDocument->createElement(xercesString("command"));
   DOMText* cmdName = _rootDocument->createTextNode(xercesString(_cmdName.c_str()));
   cmdNameElement->appendChild(cmdName);
   _veElement->appendChild(cmdNameElement);
}
///////////////////////////////////////
void DataLoader::_updateDataValuePairs()
{
   for ( size_t i = 0; i < _dataValuePairs.size();  ++i )
   {
      _dataValuePairs.at(i)->SetOwnerDocument(_rootDocument);
      _veElement->appendChild( _dataValuePairs.at( i )->GetXMLData( "parameter" ) );
   }
}
/////////////////////////////////////////////////////////
//set the data from an string representing the xml     //
/////////////////////////////////////////////////////////
void DataLoader::SetObjectFromXMLData(DOMNode* xmlInput)
{
   DOMElement* currentElement = 0;
   if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(xmlInput);
   }
   
   if(currentElement)
   {
      //break down the element
      {
         //get variables by tags
         DOMNodeList* subElements = currentElement->getElementsByTagName(xercesString("command"));
      
         //should only be the name of the command
         DOMElement* name = dynamic_cast< DOMElement* >( subElements->item(0) );
         if(name)
         {
            ExtractCmdNameFromElement(name);
         }
      }
      //break down the element
      {
         //get variables by tags
         DOMNodeList* subElements = currentElement->getElementsByTagName(xercesString("parameter"));
      

         //we can have as many dvpairs as we want so get them all and populate the list
         DOMElement* dataValuePairIn = 0;
         unsigned int nDVPairsIn = subElements->getLength();

         if( nDVPairsIn && _nDataValuePairs)
         {  
            //clear out old dvpairs
            for(unsigned int i = _nDataValuePairs -1; i > - 1;  i--){
               delete _dataValuePairs.at(i);
            }
            _dataValuePairs.clear();
            nameToDataValuePairMap.clear();
         }
         //read in new data value pairs
         for(unsigned int i = 0; i < nDVPairsIn; i++)
         {
            DOMElement* dvPairIn = dynamic_cast<DOMElement*>(subElements->item(i));
            if( dvPairIn )
            {
               VE_XML::DataValuePair* veDvp = new VE_XML::DataValuePair();
               veDvp->SetObjectFromXMLData(dvPairIn);
               _dataValuePairs.push_back(veDvp);
               nameToDataValuePairMap[ veDvp->GetDataName() ] = veDvp;
            }
         }
      }
   }
   _nDataValuePairs = static_cast< unsigned int >( _dataValuePairs.size() );
   //_nChildren = 1 + _dataValuePairs.size();
}
/////////////////////////////////////////////////////////////////////
void DataLoader::ExtractCmdNameFromElement(DOMElement* commandElement)
{
   _cmdName = ExtractDataStringFromSimpleElement( commandElement );
}
///////////////////////////////////////
std::string DataLoader::GetCommandName()
{
   return _cmdName;
}
///////////////////////////////////////
void DataLoader::SetCommandName( std::string name )
{
   _cmdName = name;
}
//////////////////////////////////////////////////////////////////////////////
VE_XML::DataValuePair* DataLoader::GetDataValuePair(std::string dataValueName)
{
   std::map< std::string, VE_XML::DataValuePair* >::iterator iter;
   iter = nameToDataValuePairMap.find( dataValueName );
   if ( iter != nameToDataValuePairMap.end() )
   {
      return iter->second;
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////
VE_XML::DataValuePair* DataLoader::GetDataValuePair( int index )
{
   try
   {
      return _dataValuePairs.at(index);
   }
   catch (...)
   {
      if ( index >= 0 )
      {
         std::cerr << " DataLoader::GetDataValuePair The element request is out of sequence."
            << " Please ask for a lower number point or -1 to request new element." << std::endl;
      }  
      _dataValuePairs.push_back( new DataValuePair() );
      return _dataValuePairs.back();
   }
}
///////////////////////////////////////////////////
unsigned int DataLoader::GetNumberOfDataValuePairs()
{
   return _dataValuePairs.size();
}
   
