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
 * File:          $RCSfile: Command.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include <iostream>
using namespace VE_XML;
//////////////////////
//Constructor       //
//////////////////////
Command::Command()
:XMLObject()
{
   _cmdName =  '\0';
   _nDataValuePairs = 0;
   _dataValuePairs.clear();
   SetObjectType("Command");
}
///////////////////////
Command::~Command()
{
   for ( size_t i = 0; i < _dataValuePairs.size(); ++i )
   {
      delete _dataValuePairs.at(i);
   }
   _dataValuePairs.clear();
   nameToDataValuePairMap.clear();
}
///////////////////////////////////////////
Command::Command( const Command& input )
:XMLObject(input)
{
   _cmdName =  input._cmdName;
   _nDataValuePairs = input._nDataValuePairs;
   for ( unsigned int i = 0; i < input._nDataValuePairs; ++i )
   {
      _dataValuePairs.push_back( new DataValuePair( (*(input._dataValuePairs.at(i))) ) );
      nameToDataValuePairMap[ _dataValuePairs.back()->GetDataName() ] = _dataValuePairs.back();
   }
}
/////////////////////////////////////////////////////
Command& Command::operator=( const Command& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      XMLObject::operator =(input);
      _cmdName =  input._cmdName;
      _nDataValuePairs = input._nDataValuePairs;

      int i  = _dataValuePairs.size() - 1;
      while(i >=0)
      {
         delete _dataValuePairs.at(i);
         i--;
      }
      _dataValuePairs.clear();
      nameToDataValuePairMap.clear();

      _nDataValuePairs = input._nDataValuePairs;
      for ( unsigned int i = 0; i < input._nDataValuePairs; ++i )
      {
         _dataValuePairs.push_back( new DataValuePair( (*(input._dataValuePairs.at(i))) ) );
         nameToDataValuePairMap[ _dataValuePairs.back()->GetDataName() ] = _dataValuePairs.back();
      }
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////////
void Command::AddDataValuePair(VE_XML::DataValuePair* commandValuePair)
{
   _dataValuePairs.push_back(commandValuePair);
   _nDataValuePairs = static_cast< unsigned int >( _dataValuePairs.size() );
   nameToDataValuePairMap[ _dataValuePairs.back()->GetDataName() ] = commandValuePair;
}
/////////////////////////////////
void Command::_updateVEElement( std::string input )
{
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString(input));
   }
   //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
   //_nChildren will be the number of dvPairs + the name of the command but we have to call the
   //update functions below to get the ndvPairs before we can calculate _nChildren

   //Add code here to update the specific sub elements
   _updateCommandName();
   _updateDataValuePairs();

   _nChildren = 1 + _nDataValuePairs;
}
////////////////////////////////////
void Command::_updateCommandName()
{
   DOMElement* cmdNameElement = _rootDocument->createElement(xercesString("command"));
   DOMText* cmdName = _rootDocument->createTextNode(xercesString(_cmdName.c_str()));
   cmdNameElement->appendChild(cmdName);
   _veElement->appendChild(cmdNameElement);
}
///////////////////////////////////////
void Command::_updateDataValuePairs()
{
   for(unsigned int i = 0; i < _nDataValuePairs;  i++){
      _dataValuePairs.at(i)->SetOwnerDocument(_rootDocument);
      _veElement->appendChild( _dataValuePairs.at( i )->GetXMLData( "parameter" ) );
   }
}
/////////////////////////////////////////////////////////
//set the data from an string representing the xml     //
/////////////////////////////////////////////////////////
void Command::SetObjectFromXMLData(DOMNode* xmlInput)
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
   _nChildren = 1 + _nDataValuePairs;
}
/////////////////////////////////////////////////////////////////////
void Command::ExtractCmdNameFromElement(DOMElement* commandElement)
{
   _cmdName = ExtractDataStringFromSimpleElement( commandElement );
}
///////////////////////////////////////
std::string Command::GetCommandName()
{
   return _cmdName;
}
///////////////////////////////////////
void Command::SetCommandName( std::string name )
{
   _cmdName = name;
}
//////////////////////////////////////////////////////////////////////////////
VE_XML::DataValuePair* Command::GetDataValuePair(std::string dataValueName)
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
VE_XML::DataValuePair* Command::GetDataValuePair(unsigned int index)
{
   try
   {
      return _dataValuePairs.at(index);
   }
   catch ( ... )
   {
      std::cout << " Invalid index = "
                  << index << " specified for Command::GetDataValuePair()"
                  << std::endl;
      return 0;
   }
}
///////////////////////////////////////////////////
unsigned int Command::GetNumberOfDataValuePairs()
{
   return _nDataValuePairs;
}
   
