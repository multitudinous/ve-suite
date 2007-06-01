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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/StateInfo.h"
#include "VE_Open/XML/Command.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_XML;

//////////////////////////
StateInfo::StateInfo()
:XMLObject()
{
   SetObjectType("StateInfo");
}
///////////////////////////
StateInfo::~StateInfo()
{
   ClearState();
}
////////////////////////////////////////////////////
void StateInfo::AddState(VE_XML::Command* state)
{
   _stateInfo.push_back(state);
}
//////////////////////////////
void StateInfo::ClearState()
{
   for ( size_t i = 0; i < _stateInfo.size(); ++i )
   {
      delete _stateInfo.at(i);
   }
   _stateInfo.clear();
}
////////////////////////////////////////////////////////////////////////////////
void StateInfo::_updateVEElement( std::string input )
{
   //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
   //this will be based on the number of commands stored in the state

   //Add code here to update the specific sub elements
   _updateCommands();
}
////////////////////////////////////////////////////////////////////////////////
void StateInfo::_updateCommands()
{
   size_t nCommands = _stateInfo.size();
   for(size_t i = 0; i < nCommands;  i++)
   {
      _stateInfo.at(i)->SetOwnerDocument(_rootDocument);
      _veElement->appendChild( _stateInfo.at(i)->GetXMLData( "Command" ) );
   }
   //_nChildren = static_cast< unsigned int>( nCommands );
}
/////////////////////////////////////////////////////////////
//set the data from an string representing the xml         //
/////////////////////////////////////////////////////////////
void StateInfo::SetObjectFromXMLData(DOMNode* xmlInput)
{
   DOMElement* currentElement = 0;
   if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE){
      currentElement = dynamic_cast<DOMElement*>(xmlInput);
   }

   //get variables by tags
   DOMNodeList* subElements = currentElement->getElementsByTagName(xercesString("Command"));

   //we can have as many dvpairs as we want so get them all and populate the list
   DOMElement* cmdsIn = 0;
   unsigned int nCmdsIn = subElements->getLength();

   size_t stateInfoSize = _stateInfo.size();
   if( nCmdsIn && stateInfoSize)
   {  
         //clear out old dvpairs
         for(size_t i = stateInfoSize -1; i > - 1;  i--){
            delete _stateInfo.at(i);
         }
         _stateInfo.clear();
    }
    //read in new commands
    for(unsigned int i = 0; i < nCmdsIn; i++){
      DOMElement* vecmdIn = dynamic_cast<DOMElement*>(subElements->item(i));
      if(vecmdIn)
      {
         VE_XML::Command* Command = new VE_XML::Command();
         Command->SetObjectFromXMLData(vecmdIn);
         Command->SetOwnerDocument(_rootDocument);
         _stateInfo.push_back(Command);
      }
   }
   
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::Command* StateInfo::GetState(std::string name)
{
   size_t nStates = _stateInfo.size();
   for(unsigned int i = 0; i < nStates; i++)
   {
      if(_stateInfo.at(i)->GetCommandName() == name)
      {
         return _stateInfo.at(i);
      }
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::Command* StateInfo::GetState(unsigned int index)
{
   return _stateInfo.at(index);
}
////////////////////////////////////////////////////////////////////////////////
std::vector< VE_XML::Command* > StateInfo::GetStateVector( void )
{
   return _stateInfo;
}
///////////////////////////////////////////////////// 
StateInfo::StateInfo( const StateInfo& input)
:XMLObject(input)
{
   for(size_t i = 0; i < input._stateInfo.size(); i++)
   {
      _stateInfo.push_back( new Command( *(input._stateInfo.at(i)) ) );
   }
}
/////////////////////////////////////////////////////////
StateInfo& StateInfo::operator= ( const StateInfo& input)
{
   if( this != &input )
   {
      ClearState();
      for(size_t i = 0; i < input._stateInfo.size(); i++)
      {
         _stateInfo.push_back( new Command( *(input._stateInfo.at(i)) ) );
      }
   }
   return *this;
}
