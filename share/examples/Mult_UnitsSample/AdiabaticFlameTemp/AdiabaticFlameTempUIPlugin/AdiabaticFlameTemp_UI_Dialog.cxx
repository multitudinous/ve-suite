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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "AdiabaticFlameTemp_UI_Dialog.h"

BEGIN_EVENT_TABLE(AdiabaticFlameTemp_UI_Dialog, UIDialog)
	 EVT_BUTTON			(CLOSE_EXCEL,			        AdiabaticFlameTemp_UI_Dialog::_onCloseExcel)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(AdiabaticFlameTemp_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
AdiabaticFlameTemp_UI_Dialog
::AdiabaticFlameTemp_UI_Dialog
(wxWindow* parent, int id,
  double* perc_theor_error,
  long* closesheets)
: UIDialog((wxWindow *) parent, id, "AdiabaticFlameTemp"),
  p_perc_theor_error(perc_theor_error),
  p_closesheets(closesheets)
{
   (*p_perc_theor_error) = 0;
   (*p_closesheets) = 0;
   closeSheets = 0;

  	wxStaticBox* listBox = new wxStaticBox(this, -1, "Use the text control to enter the input value", wxDefaultPosition,wxDefaultSize,wxCAPTION);

   wxStaticText* _errorLabel = new wxStaticText(this, -1, wxT("Enter Percent Theoretical Error "));
	_errorentry = new wxTextCtrl(this, -1, wxT("500"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   wxBoxSizer* _errorGroup  = new wxBoxSizer(wxHORIZONTAL);
	_errorGroup->Add(_errorLabel,1,wxALIGN_LEFT|wxEXPAND);
   _errorGroup->Add(_errorentry,1,wxALIGN_RIGHT|wxEXPAND);

   _closeExcelButton = new wxButton(this,CLOSE_EXCEL,wxT("Close Spreadsheet"));
	_updateButton = new wxButton(this,wxID_OK,wxT("Update"));
   wxBoxSizer* _buttonGroup  = new wxBoxSizer(wxHORIZONTAL);
	_buttonGroup->Add(_closeExcelButton,1,wxALIGN_LEFT|wxEXPAND);
   _buttonGroup->Add(_updateButton,1,wxALIGN_RIGHT|wxEXPAND);

   wxStaticBoxSizer* _listGroup = new wxStaticBoxSizer(listBox,wxVERTICAL);
	_listGroup->Add(_errorGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _listGroup->Add(_buttonGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxBoxSizer* _mainSizer = new wxBoxSizer(wxVERTICAL);
   _mainSizer->Add(_listGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(_mainSizer);
	_mainSizer->Fit(this);

}

/////////////////////////////////////////////////////
AdiabaticFlameTemp_UI_Dialog
::~AdiabaticFlameTemp_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool AdiabaticFlameTemp_UI_Dialog::TransferDataFromWindow()
{
   wxString txt;

	txt  = _errorentry->GetValue();
	(*p_perc_theor_error) = atof(txt.c_str());

   (*p_closesheets) = closeSheets;
   closeSheets = 0;

  return true;
}

////////////////////////////////////////////////////
bool AdiabaticFlameTemp_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void AdiabaticFlameTemp_UI_Dialog::Lock(bool l)
{
}

void AdiabaticFlameTemp_UI_Dialog::_onCloseExcel(wxCommandEvent& event)
{
	closeSheets = 1;	
}
