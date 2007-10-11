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
#include "ves/conductor/util/BaseDialog.h"

#include <iostream>

#include <wx/msgdlg.h>
#include <wx/image.h>
#include <wx/button.h>
#include <wx/icon.h>

#include "ves/open/xml/XMLReaderWriter.h"
#include "ves/open/xml/Command.h"
#include "ves/open/xml/DataValuePair.h"
#include "ves/conductor/util/CORBAServiceList.h"

//#include "VE_Installer/installer/installerImages/ve_icon32x32.xpm"
using namespace VE_Conductor::GUI_Utilities;
////////////////////////////////////////////////////
//Here is the constructor with passed in pointers //
////////////////////////////////////////////////////
BaseDialog::BaseDialog (wxWindow* parent, int id,std::string title)
:wxDialog((wxWindow*) parent, id, wxString(title.c_str(), wxConvUTF8), wxDefaultPosition, wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX|wxCLOSE_BOX))
{
   //_vjObsPtr = 0;
   _commandName = "";
   //SetIcon( ve_icon32x32_xpm );
}
/////////////////////////////////////////////////////
BaseDialog::~BaseDialog()
{
}
//////////////////////////////////////////////////////////
void BaseDialog::SetVjObsPtr(VjObs_ptr xplorerCom)
{
   //_vjObsPtr = xplorerCom;
}
//////////////////////////////////////////////
void BaseDialog::ClearInstructions()
{
   ///deleting the command deletes the memory but
   ///we need to insure that the vector is clear
   _instructions.clear();
   _commandName.clear();
}
//////////////////////////////////////////////////////
void BaseDialog::_addOKButton(wxSizer* buttonRowSizer)
{
   buttonRowSizer->Add(new wxButton(this,wxID_OK,_("OK")),0,wxALIGN_CENTER);
}
/////////////////////////////////////////////////////////
void BaseDialog::_addCancelButton(wxSizer* buttonRowSizer)
{
   buttonRowSizer->Add(new wxButton(this,wxID_CANCEL,_("Cancel")),0,wxALIGN_CENTER);
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
   
   std::pair<VE_XML::Command*,std::string> nodeTagPair;
   nodeTagPair.first = newCommand;
   nodeTagPair.second = std::string("vecommand");
   std::vector< std::pair<VE_XML::XMLObject*,std::string> > nodeToWrite;
   nodeToWrite.push_back(nodeTagPair);

   commandWriter.WriteXMLDocument(nodeToWrite,commandString,"Command");

   //char* tempDoc = new char[ commandString.size() + 1 ];
   //tempDoc = CORBA::string_dup( commandString.c_str() );

   if ( !commandString.empty() )
   {
      try
      {
         //std::cout<<"---The command to send---"<<std::endl;
         //std::cout<<tempDoc<<std::endl;
         // CORBA releases the allocated memory so we do not have to
         //_vjObsPtr->SetCommandString( tempDoc );
         VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer(newCommand);
         delete newCommand;
      }
      catch ( ... )
      {
         wxMessageBox( _("Send data to VE-Xplorer failed. Probably need to disconnect and reconnect."), 
                        _("Communication Failure"), wxOK | wxICON_INFORMATION );
         delete newCommand;
      }
   }
   else
   {
      delete newCommand;
   }

   ClearInstructions();
}
