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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Conductor/Framework/BaseDialog.h"

#include <iostream>
#include <wx/msgdlg.h>

#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

////////////////////////////////////////////////////
//Here is the constructor with passed in pointers //
////////////////////////////////////////////////////
BaseDialog::BaseDialog (wxWindow* parent, int id,std::string title)
:wxDialog((wxWindow*) parent, id, title.c_str(), wxDefaultPosition, wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX|wxCLOSE_BOX))
{
   _vjObsPtr = 0;
   _commandName = "";
}
/////////////////////////////////////////////////////
BaseDialog::~BaseDialog()
{
}
//////////////////////////////////////////////////////////
void BaseDialog::SetVjObsPtr(VjObs_ptr xplorerCom)
{
   _vjObsPtr = xplorerCom;
}
//////////////////////////////////////////////
void BaseDialog::ClearInstructions()
{
   ///deleting the command deletes the memory but
   ///we need to insure that the vector is clear
   _instructions.clear();
   _commandName.clear();
}
/////////////////////////////////////////
void BaseDialog::_sendCommandsToXplorer()
{
   //std::cout<<"---Sending commands to Xplorer---"<<std::endl;
   VE_XML::Command* newCommand = new VE_XML::Command();

   for(size_t i =0; i < _instructions.size(); i++)
   {
      newCommand->AddDataValuePair(_instructions.at(i));
   }

   newCommand->SetCommandName(_commandName);

   std::string commandString("returnString");

   VE_XML::XMLReaderWriter commandWriter;
   commandWriter.UseStandaloneDOMDocumentManager();
   commandWriter.WriteToString();
   
   std::pair<VE_XML::Command*,std::string> nodeTagPair;
   nodeTagPair.first = newCommand;
   nodeTagPair.second = std::string("vecommand");
   std::vector< std::pair<VE_XML::XMLObject*,std::string> > nodeToWrite;
   nodeToWrite.push_back(nodeTagPair);

   commandWriter.WriteXMLDocument(nodeToWrite,commandString,"Command");

   char* tempDoc = new char[ commandString.size() + 1 ];
   tempDoc = CORBA::string_dup( commandString.c_str() );

   if ( !CORBA::is_nil( _vjObsPtr ) && !commandString.empty() )
   {
      try
      {
         //std::cout<<"---The command to send---"<<std::endl;
         //std::cout<<tempDoc<<std::endl;
         // CORBA releases the allocated memory so we do not have to
         _vjObsPtr->SetCommandString( tempDoc );
      }
      catch ( ... )
      {
         wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                        "Communication Failure", wxOK | wxICON_INFORMATION );
         delete [] tempDoc;
      }
   }
   else
   {
      delete [] tempDoc;
   }
   //Clean up memory
   delete newCommand;
}

