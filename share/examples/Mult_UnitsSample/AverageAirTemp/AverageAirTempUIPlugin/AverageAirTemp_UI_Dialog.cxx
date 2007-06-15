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
#include "AverageAirTemp_UI_Dialog.h"

BEGIN_EVENT_TABLE(AverageAirTemp_UI_Dialog, UIDialog)
	 EVT_BUTTON			(CLOSE_EXCEL,			        AverageAirTemp_UI_Dialog::_onCloseExcel)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(AverageAirTemp_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
AverageAirTemp_UI_Dialog
::AverageAirTemp_UI_Dialog
(wxWindow* parent, int id,
  double* intakediam,
  double* airvel,
  double* intaketemp,
  double* airinlettemp,
  double* intakelength,
  long* closesheets)
: UIDialog((wxWindow *) parent, id, "AverageAirTemp"),
  p_intakediam(intakediam),
  p_airvel(airvel),
  p_intaketemp(intaketemp),
  p_airinlettemp(airinlettemp),
  p_intakelength(intakelength),
  p_closesheets(closesheets)
{
   (*p_intakediam) = 0;
   (*p_airvel) = 0;
   (*p_intaketemp) = 0;
   (*p_airinlettemp) = 0;
   (*p_intakelength) = 0;
   (*p_closesheets) = 0;
   closeSheets = 0;

   wxStaticBox* listBox = new wxStaticBox(this, -1, "Use the text controls to enter the input values", wxDefaultPosition,wxDefaultSize,wxCAPTION);

   wxStaticText* _intakediamLabel = new wxStaticText(this, -1, wxT("Enter Intake Diameter (cm) "));
	_intakediamentry = new wxTextCtrl(this, -1, wxT("6.0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   wxBoxSizer* _intakediamGroup  = new wxBoxSizer(wxHORIZONTAL);
	_intakediamGroup->Add(_intakediamLabel,1,wxALIGN_LEFT|wxEXPAND);
   _intakediamGroup->Add(_intakediamentry,1,wxALIGN_RIGHT|wxEXPAND);

   wxStaticText* _airvelLabel = new wxStaticText(this, -1, wxT("Enter Air Velocity (m/s) "));
	_airvelentry = new wxTextCtrl(this, -1, wxT("1.0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   wxBoxSizer* _airvelGroup  = new wxBoxSizer(wxHORIZONTAL);
	_airvelGroup->Add(_airvelLabel,1,wxALIGN_LEFT|wxEXPAND);
   _airvelGroup->Add(_airvelentry,1,wxALIGN_RIGHT|wxEXPAND);

   wxStaticText* _intaketempLabel = new wxStaticText(this, -1, wxT("Enter Intake Temp (K) "));
	_intaketempentry = new wxTextCtrl(this, -1, wxT("800"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   wxBoxSizer* _intaketempGroup  = new wxBoxSizer(wxHORIZONTAL);
	_intaketempGroup->Add(_intaketempLabel,1,wxALIGN_LEFT|wxEXPAND);
   _intaketempGroup->Add(_intaketempentry,1,wxALIGN_RIGHT|wxEXPAND);

   wxStaticText* _airinlettempLabel = new wxStaticText(this, -1, wxT("Enter Air Inlet Temp (K) "));
	_airinlettempentry = new wxTextCtrl(this, -1, wxT("400"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   wxBoxSizer* _airinlettempGroup  = new wxBoxSizer(wxHORIZONTAL);
	_airinlettempGroup->Add(_airinlettempLabel,1,wxALIGN_LEFT|wxEXPAND);
   _airinlettempGroup->Add(_airinlettempentry,1,wxALIGN_RIGHT|wxEXPAND);

   wxStaticText* _intakelengthLabel = new wxStaticText(this, -1, wxT("Enter Intake Length (cm) "));
	_intakelengthentry = new wxTextCtrl(this, -1, wxT("20.0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   wxBoxSizer* _intakelengthGroup  = new wxBoxSizer(wxHORIZONTAL);
	_intakelengthGroup->Add(_intakelengthLabel,1,wxALIGN_LEFT|wxEXPAND);
   _intakelengthGroup->Add(_intakelengthentry,1,wxALIGN_RIGHT|wxEXPAND);


   _closeExcelButton = new wxButton(this,CLOSE_EXCEL,wxT("Close Spreadsheet"));
	_updateButton = new wxButton(this,wxID_OK,wxT("Update"));
   wxBoxSizer* _buttonGroup  = new wxBoxSizer(wxHORIZONTAL);
	_buttonGroup->Add(_closeExcelButton,1,wxALIGN_LEFT|wxEXPAND);
   _buttonGroup->Add(_updateButton,1,wxALIGN_RIGHT|wxEXPAND);

   wxStaticBoxSizer* _listGroup = new wxStaticBoxSizer(listBox,wxVERTICAL);
	_listGroup->Add(_intakediamGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _listGroup->Add(_airvelGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _listGroup->Add(_intaketempGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _listGroup->Add(_airinlettempGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _listGroup->Add(_intakelengthGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
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
AverageAirTemp_UI_Dialog
::~AverageAirTemp_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool AverageAirTemp_UI_Dialog::TransferDataFromWindow()
{
   wxString txt;

	txt  = _intakediamentry->GetValue();
	(*p_intakediam) = atof(txt.c_str());

   txt  = _airvelentry->GetValue();
	(*p_airvel) = atof(txt.c_str());

   txt  = _intaketempentry->GetValue();
	(*p_intaketemp) = atof(txt.c_str());

   txt  = _airinlettempentry->GetValue();
	(*p_airinlettemp) = atof(txt.c_str());

   txt  = _intakelengthentry->GetValue();
	(*p_intakelength) = atof(txt.c_str());

   (*p_closesheets) = closeSheets;
   closeSheets = 0;

  return true;
}

////////////////////////////////////////////////////
bool AverageAirTemp_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void AverageAirTemp_UI_Dialog::Lock(bool l)
{
}

void AverageAirTemp_UI_Dialog::_onCloseExcel(wxCommandEvent& event)
{
	closeSheets = 1;	
}
