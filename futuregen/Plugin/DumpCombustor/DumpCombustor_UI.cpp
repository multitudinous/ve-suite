#include "DumpCombustor_UI.h"

IMPLEMENT_DYNAMIC_CLASS(DumpCombustor_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
DumpCombustor_UI_Dialog
::DumpCombustor_UI_Dialog
(wxWindow* parent, int id,
  double* desired_temp,
  double* air_temp,
  double* air_humidity,
  double* ambient_pres)
: UIDialog((wxWindow *) parent, id, "DumpCombustor"),
  p_desired_temp(desired_temp),
  p_air_temp(air_temp),
  p_air_humidity(air_humidity),
  p_ambient_pres(ambient_pres)
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
  wxBoxSizer *data_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_row = new wxBoxSizer(wxHORIZONTAL);

  data_row->Add(10, 5, 0);
  data_row->Add(data_first_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_second_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_third_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_forth_row, 0);
  data_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  wxStaticText * label0 = new wxStaticText(this, -1, " Desired Temperature (K): ", wxDefaultPosition, wxSize(200, 17));
  t_desired_temp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_row->Add(label0);
  data_first_row->Add(t_desired_temp);

  wxStaticText * label1 = new wxStaticText(this, -1, " Air Temperature (K) ", wxDefaultPosition, wxSize(200, 17));
  t_air_temp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_air_temp);

  wxStaticText * label2 = new wxStaticText(this, -1, " Air Relative Humidity (%) ", wxDefaultPosition, wxSize(200, 17));
  t_air_humidity = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_air_humidity);

  wxStaticText * label3 = new wxStaticText(this, -1, " Ambient Pressure (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_ambient_pres = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_ambient_pres);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  
}

/////////////////////////////////////////////////////
DumpCombustor_UI_Dialog
::~DumpCombustor_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool DumpCombustor_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_desired_temp->GetValue();
  (*p_desired_temp) = atof(txt.c_str());
  
  txt = t_air_temp->GetValue();
  (*p_air_temp) = atof(txt.c_str());

  txt = t_air_humidity->GetValue();
  (*p_air_humidity) = atof(txt.c_str());

  txt = t_ambient_pres->GetValue();
  (*p_ambient_pres) = atof(txt.c_str());

  return true;
}

////////////////////////////////////////////////////
bool DumpCombustor_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4;
   
  txt1<<(*p_desired_temp);
  t_desired_temp->SetValue(txt1);

  txt2<<(*p_air_temp);
  t_air_temp->SetValue(txt2);

  txt3<<(*p_air_humidity);
  t_air_humidity->SetValue(txt3);

  txt4<<(*p_ambient_pres);
  t_ambient_pres->SetValue(txt4);

  return true;
}

void DumpCombustor_UI_Dialog::Lock(bool l)
{
}

