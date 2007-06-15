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
#include "SampleMFC_Gauges_UI_Dialog.h"

BEGIN_EVENT_TABLE(SampleMFC_Gauges_UI_Dialog, UIDialog)
   EVT_RADIOBOX		(CALC_METHOD_RADIOBOX,		SampleMFC_Gauges_UI_Dialog::_onCalcMethod)
	 EVT_BUTTON			(CLOSE_EXCEL,			        SampleMFC_Gauges_UI_Dialog::_onCloseExcel)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(SampleMFC_Gauges_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
SampleMFC_Gauges_UI_Dialog
::SampleMFC_Gauges_UI_Dialog
(wxWindow* parent, int id,
  double* dbl1,
  double* dbl2,
  long* int1,
	long* int2,
  vector<double>* dbllist)
: UIDialog((wxWindow *) parent, id, "SampleMFC_Gauges"),
  p_dbl1(dbl1),
  p_dbl2(dbl2),
  p_int1(int1),
	p_int2(int2),
  p_dbllist(dbllist)
{
	(*p_dbl1) = 0;
	(*p_dbl2) = 0;
	(*p_int1) = 0;
	(*p_int2) = 0;

	closeSheets = 0;

	wxStaticBox* listsBox = new wxStaticBox(this, -1, "Use the combo boxes to select the input values", wxDefaultPosition,wxDefaultSize,wxCAPTION);

	wxString calcstr[] = { wxT("Calculation1"),
														 wxT("Calculation2"),
														 wxT("Calculation3")};

	_selcalcRBox = new wxRadioBox(this, CALC_METHOD_RADIOBOX, wxT("Select The Calculation Method"),
                                                  wxDefaultPosition, wxDefaultSize, 3,
                                                     calcstr, 1, wxRA_SPECIFY_COLS);

	wxString list1str[] = { wxT("2"),
													wxT("4"),
													wxT("6"),
													wxT("8"),
													wxT("10")};
	_list1sel = new wxComboBox(this,LIST1_COMBOBOX , wxT("Select a Value"),wxDefaultPosition, wxDefaultSize,5,list1str, wxCB_DROPDOWN);

	wxString list2str[] = { wxT("3"),
													wxT("5"),
													wxT("7"),
													wxT("9"),
													wxT("11")};
	_list2sel = new wxComboBox(this,LIST2_COMBOBOX , wxT("Select a Value"),wxDefaultPosition, wxDefaultSize,5,list2str, wxCB_DROPDOWN);

	wxString list3str[] = { wxT("5.1"),
													wxT("5.3"),
													wxT("5.5"),
													wxT("5.7"),
													wxT("5.9")};
	_list3sel = new wxComboBox(this,LIST3_COMBOBOX , wxT("Select a Value"),wxDefaultPosition, wxDefaultSize,5,list3str, wxCB_DROPDOWN);

	wxBoxSizer* _listSizer = new wxBoxSizer(wxVERTICAL);
	_listSizer->Add(_list1sel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	_listSizer->Add(_list2sel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	_listSizer->Add(_list3sel,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	wxStaticText* _scaleLabel = new wxStaticText(this, -1, wxT("Enter a Scale Factor "));
	_scalefactorentry = new wxTextCtrl(this, -1, wxT("1.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* _scaleGroup  = new wxBoxSizer(wxHORIZONTAL);
	_scaleGroup->Add(_scaleLabel,1,wxALIGN_LEFT|wxEXPAND);
  _scaleGroup->Add(_scalefactorentry,1,wxALIGN_RIGHT|wxEXPAND);

  _addfactorSlider = new wxSlider(this, ADD_SLIDER,0,-100,100,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_LABELS );

  wxStaticBoxSizer* _listsGroup = new wxStaticBoxSizer(listsBox,wxVERTICAL);
	_listsGroup->Add(_listSizer,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	_closeExcelButton = new wxButton(this,CLOSE_EXCEL,wxT("Close Spreadsheet"));
	_updateButton = new wxButton(this,wxID_OK,wxT("Update"));

	wxBoxSizer* _mainSizer = new wxBoxSizer(wxVERTICAL);
  _mainSizer->Add(_selcalcRBox,5,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	_mainSizer->AddSpacer( 25 );
  _mainSizer->Add(_listsGroup,5,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	_mainSizer->AddSpacer( 25 );
	_mainSizer->Add(_scaleGroup,1,wxALIGN_CENTER_HORIZONTAL);
	_mainSizer->AddSpacer( 25 );
  _mainSizer->Add(_addfactorSlider,2,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	_mainSizer->AddSpacer( 25 );
  _mainSizer->Add(_closeExcelButton,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	_mainSizer->AddSpacer( 25 );
  _mainSizer->Add(_updateButton,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);


  //set this flag and let wx handle alignment
  SetAutoLayout(true);
  //assign the group to the panel
  SetSizer(_mainSizer);
	_mainSizer->Fit(this);

	_scalefactorentry->Enable( false );
	_addfactorSlider->Enable( false );

}

/////////////////////////////////////////////////////
SampleMFC_Gauges_UI_Dialog
::~SampleMFC_Gauges_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool SampleMFC_Gauges_UI_Dialog::TransferDataFromWindow()
{
	wxString txt;

	(*p_dbllist).clear();
	txt = _list1sel->GetValue();
	(*p_dbllist).push_back(atof(txt.c_str()));
	txt = _list2sel->GetValue();
	(*p_dbllist).push_back(atof(txt.c_str()));
	txt = _list3sel->GetValue();
	(*p_dbllist).push_back(atof(txt.c_str()));

	txt  = _scalefactorentry->GetValue();
	(*p_dbl1) = atof(txt.c_str());

	(*p_dbl2) = _addfactorSlider->GetValue();

	(*p_int1) = _selcalcRBox->GetSelection();

	(*p_int2) = closeSheets;

	closeSheets = 0;

  return true;
}

////////////////////////////////////////////////////
bool SampleMFC_Gauges_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void SampleMFC_Gauges_UI_Dialog::Lock(bool l)
{
}

void SampleMFC_Gauges_UI_Dialog::_onCalcMethod(wxCommandEvent& event)
{
	if ( _selcalcRBox->GetSelection() == 0 )
	{
		_scalefactorentry->Enable( false );
		_addfactorSlider->Enable( false );
	}
	else if ( _selcalcRBox->GetSelection() == 1 )
	{
		_scalefactorentry->Enable( true );
		_addfactorSlider->Enable( false );
	}
	else if ( _selcalcRBox->GetSelection() == 2 )
	{
		_scalefactorentry->Enable( false );
		_addfactorSlider->Enable( true );
	}

}

void SampleMFC_Gauges_UI_Dialog::_onCloseExcel(wxCommandEvent& event)
{
	closeSheets = 1;	
}
