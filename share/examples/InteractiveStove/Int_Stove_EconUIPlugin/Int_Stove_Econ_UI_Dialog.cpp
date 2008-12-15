#include "Int_Stove_Econ_UI_Dialog.h"
#include <wx/wx.h>

BEGIN_EVENT_TABLE(Int_Stove_Econ_UI_Dialog, UIDialog)
	 EVT_BUTTON			(CLOSE_EXCEL,			        Int_Stove_Econ_UI_Dialog::_onCloseExcel)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(Int_Stove_Econ_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
Int_Stove_Econ_UI_Dialog
::Int_Stove_Econ_UI_Dialog
(wxWindow* parent, int id,
  std::vector< double >* cost_array,
  long* closesheets)
: UIDialog((wxWindow *) parent, id, "IntStoveEcon"),
  p_cost_array(cost_array),
  p_closesheets(closesheets)
{
	(*p_cost_array).clear();
	(*p_closesheets) = 0;
	closeSheets = 0;

	wxStaticBox* listBox = new wxStaticBox(this, -1, "Use the text controls to enter the input values", wxDefaultPosition,wxDefaultSize,wxCAPTION);

   wxStaticText* _base_mat_cost_Label = new wxStaticText(this, -1, wxT("Enter The Base Stove Material Cost "));
   _base_mat_cost_entry = new wxTextCtrl(this, -1, wxT("100"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   wxBoxSizer* _base_mat_cost  = new wxBoxSizer(wxHORIZONTAL);
   _base_mat_cost->Add(_base_mat_cost_Label,1,wxALIGN_LEFT|wxEXPAND);
   _base_mat_cost->Add(_base_mat_cost_entry,1,wxALIGN_RIGHT|wxEXPAND);

   wxStaticText* _base_const_cost_Label = new wxStaticText(this, -1, wxT("Enter The Base Stove Construction Cost "));
   _base_const_cost_entry = new wxTextCtrl(this, -1, wxT("100"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   wxBoxSizer* _base_const_cost  = new wxBoxSizer(wxHORIZONTAL);
   _base_const_cost->Add(_base_const_cost_Label,1,wxALIGN_LEFT|wxEXPAND);
   _base_const_cost->Add(_base_const_cost_entry,1,wxALIGN_RIGHT|wxEXPAND);

   wxStaticText* _baffle_mat_cost_Label = new wxStaticText(this, -1, wxT("Enter The Baffle Material Cost per sq ft "));
   _baffle_mat_cost_entry = new wxTextCtrl(this, -1, wxT("6"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   wxBoxSizer* _baffle_mat_cost  = new wxBoxSizer(wxHORIZONTAL);
   _baffle_mat_cost->Add(_baffle_mat_cost_Label,1,wxALIGN_LEFT|wxEXPAND);
   _baffle_mat_cost->Add(_baffle_mat_cost_entry,1,wxALIGN_RIGHT|wxEXPAND);


   _closeExcelButton = new wxButton(this,CLOSE_EXCEL,wxT("Close Spreadsheet"));
	_updateButton = new wxButton(this,wxID_OK,wxT("Update"));
   wxBoxSizer* _buttonGroup  = new wxBoxSizer(wxHORIZONTAL);
	_buttonGroup->Add(_closeExcelButton,1,wxALIGN_LEFT|wxEXPAND);
   _buttonGroup->Add(_updateButton,1,wxALIGN_RIGHT|wxEXPAND);

   wxStaticBoxSizer* _listGroup = new wxStaticBoxSizer(listBox,wxVERTICAL);
   _listGroup->Add(_base_mat_cost,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _listGroup->Add(_base_const_cost,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   _listGroup->Add(_baffle_mat_cost,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
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
Int_Stove_Econ_UI_Dialog
::~Int_Stove_Econ_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Int_Stove_Econ_UI_Dialog::TransferDataFromWindow()
{
   wxString txt;

	txt  = _base_mat_cost_entry->GetValue();
	(*p_cost_array).push_back(atof(txt.c_str()));

	txt  = _base_const_cost_entry->GetValue();
	(*p_cost_array).push_back(atof(txt.c_str()));

	txt  = _baffle_mat_cost_entry->GetValue();
	(*p_cost_array).push_back(atof(txt.c_str()));

   (*p_closesheets) = closeSheets;
   closeSheets = 0;

  return true;
}

////////////////////////////////////////////////////
bool Int_Stove_Econ_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void Int_Stove_Econ_UI_Dialog::Lock(bool l)
{
}

void Int_Stove_Econ_UI_Dialog::_onCloseExcel(wxCommandEvent& event)
{
	closeSheets = 1;	
}

