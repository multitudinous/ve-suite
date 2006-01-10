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
 * File:          $RCSfile: VEStateInfo.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/VE_XML/VEStateInfo.h"
#include "VE_Open/VE_XML/VECommand.h"

using namespace VE_XML;

//////////////////////////
VEStateInfo::VEStateInfo(DOMDocument* rootDoc)
:VEXMLObject(rootDoc)
{
}
///////////////////////////
VEStateInfo::~VEStateInfo()
{
   ClearState();
}
////////////////////////////////////////////////////
void VEStateInfo::AddState(VE_XML::VECommand* state)
{
   _stateInfo.push_back(state);
}
//////////////////////////////
void VEStateInfo::ClearState()
{
   if(_stateInfo.size())
   {
      size_t nStateInfo = _stateInfo.size();
      for ( size_t i = nStateInfo - 1; i > -1; i--)
      {
         delete _stateInfo.at(i);
      }
      _stateInfo.clear();
   }
}
////////////////////////////////////
void VEStateInfo::_updateVEElement( std::string input )
{
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString("veStateInfo"));
   }
   //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
   //this will be based on the number of commands stored in the state

   //Add code here to update the specific sub elements
   _updateCommands();
}
///////////////////////////////////
void VEStateInfo::_updateCommands()
{
   size_t nCommands = _stateInfo.size();
   for(size_t i = 0; i < nCommands;  i++){
      _veElement->appendChild( _stateInfo.at(i)->GetXMLData( "veCommand" ) );
   }
   _nChildren = static_cast< unsigned int>( nCommands );
}
/////////////////////////////////////////////////////////////
//set the data from an string representing the xml         //
/////////////////////////////////////////////////////////////
void VEStateInfo::SetObjectFromXMLData(DOMNode* xmlInput)
{
   DOMElement* currentElement = 0;
   if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE){
      currentElement = dynamic_cast<DOMElement*>(xmlInput);
   }

   //get variables by tags
   DOMNodeList* subElements = currentElement->getElementsByTagName(xercesString("command"));

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
         VE_XML::VECommand* veCommand = new VE_XML::VECommand(_rootDocument);
         veCommand->SetObjectFromXMLData(vecmdIn);
         _stateInfo.push_back(veCommand);
      }
   }
   
}
//////////////////////////////////////////////////////////
VE_XML::VECommand* VEStateInfo::GetState(std::string name)
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
////////////////////////////////////////////////////////////
VE_XML::VECommand* VEStateInfo::GetState(unsigned int index)
{
   return _stateInfo.at(index);
}
