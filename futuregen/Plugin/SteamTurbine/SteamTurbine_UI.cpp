#include "SteamTurbine_UI.h"

IMPLEMENT_DYNAMIC_CLASS(SteamTurbine_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
SteamTurbine_UI_Dialog
::SteamTurbine_UI_Dialog
(wxWindow* parent, int id,
  double* ad_eff,
  double* pressure_drop)
: UIDialog((wxWindow *) parent, id, "SteamTurbine"),
  p_ad_eff(ad_eff),
  p_pressure_drop(pressure_drop)
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
  wxBoxSizer *data_second_row = new wxBoxSizer(wxHORIZONTAL);

  data_row->Add(10, 5, 0);
  data_row->Add(data_first_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_second_row, 0);
  data_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  wxStaticText * label0 = new wxStaticText(this, -1, " Isentropic Efficiency: ", wxDefaultPosition, wxSize(200, 17));
  t_ad_eff = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_row->Add(label0);
  data_first_row->Add(t_ad_eff);

  wxStaticText * label1 = new wxStaticText(this, -1, " Pressure Drop (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_pressure_drop = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_pressure_drop);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  
}

/////////////////////////////////////////////////////
SteamTurbine_UI_Dialog
::~SteamTurbine_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool SteamTurbine_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_ad_eff->GetValue();
  (*p_ad_eff) = atof(txt.c_str());
  
  txt = t_pressure_drop->GetValue();
  (*p_pressure_drop) = atof(txt.c_str());

  return true;
}

////////////////////////////////////////////////////
bool SteamTurbine_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2;
   
  txt1<<(*p_ad_eff);
  t_ad_eff->SetValue(txt1);

  txt2<<(*p_pressure_drop);
  t_pressure_drop->SetValue(txt2);

  return true;
}

void SteamTurbine_UI_Dialog::Lock(bool l)
{
}

