#include "GasHeatExchanger_UI.h"

IMPLEMENT_DYNAMIC_CLASS(GasHeatExchanger_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
GasHeatExchanger_UI_Dialog
::GasHeatExchanger_UI_Dialog
(wxWindow* parent, int id,
  double* desired_temp)
: UIDialog((wxWindow *) parent, id, "GasHeatExchanger"),
  p_desired_temp(desired_temp)
{
  wxBoxSizer* toptop= new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);

  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  wxBoxSizer* data_row = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data_row, 0); 
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin
  
  wxBoxSizer *data_first_row = new wxBoxSizer(wxHORIZONTAL);

  data_row->Add(10, 5, 0);
  data_row->Add(data_first_row, 0);
  data_row->Add(10, 3, 0);
  
  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);  

  wxStaticText * label1 = new wxStaticText(this, -1, " Desired Temperature (K) ", wxDefaultPosition, wxSize(200, 17));
  t_desired_temp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  
  data_first_row->Add(label1);
  data_first_row->Add(t_desired_temp);
  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  
}

/////////////////////////////////////////////////////
GasHeatExchanger_UI_Dialog
::~GasHeatExchanger_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool GasHeatExchanger_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_desired_temp->GetValue();
  (*p_desired_temp) = atof(txt.c_str());
  return true;
}

////////////////////////////////////////////////////
bool GasHeatExchanger_UI_Dialog::TransferDataToWindow()
{
  wxString txt1;

  txt1<<(*p_desired_temp);
  t_desired_temp->SetValue(txt1);

  return true;
}

void GasHeatExchanger_UI_Dialog::Lock(bool l)
{
}

