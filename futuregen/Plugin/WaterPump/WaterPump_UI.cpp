#include "WaterPump_UI.h"

BEGIN_EVENT_TABLE(WaterPump_UI_Dialog, UIDialog)
  EVT_RADIOBUTTON(R_CASETYPE_EVA, WaterPump_UI_Dialog::OnCaseTypeChange)
  EVT_RADIOBUTTON(R_CASETYPE_DES, WaterPump_UI_Dialog::OnCaseTypeChange)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(WaterPump_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
WaterPump_UI_Dialog
::WaterPump_UI_Dialog
(wxWindow* parent, int id,
  double* eff,
  double* pressure_out,
  double* pressure_change,
  long* case_type)
: UIDialog((wxWindow *) parent, id, "WaterPump"),
  p_eff(eff),
  p_pressure_out(pressure_out),
  p_pressure_change(pressure_change),
  p_case_type(case_type)
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
  
  r_case_type_eva = new wxRadioButton(this, R_CASETYPE_EVA, _T(" Evaluation Mode "), wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  r_case_type_des = new wxRadioButton(this, R_CASETYPE_DES, _T(" Design Mode "), wxDefaultPosition, wxDefaultSize);
  data_first_row->Add(r_case_type_eva);
  data_first_row->Add(r_case_type_des);

  wxStaticText * label1 = new wxStaticText(this, -1, " Isentropic Efficiency (%) ", wxDefaultPosition, wxSize(200, 17));
  t_eff = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_eff);

  wxStaticText * label2 = new wxStaticText(this, -1, " Outlet Pressure (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_pressure_out = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_pressure_out);

  wxStaticText * label3 = new wxStaticText(this, -1, " Pressure Change (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_pressure_change = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_pressure_change);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  
}

/////////////////////////////////////////////////////
WaterPump_UI_Dialog
::~WaterPump_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool WaterPump_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_eff->GetValue();
  (*p_eff) = atof(txt.c_str());
  
  txt = t_pressure_out->GetValue();
  (*p_pressure_out) = atof(txt.c_str());

  txt = t_pressure_change->GetValue();
  (*p_pressure_change) = atof(txt.c_str());

  *p_case_type = r_case_type_des->GetValue();

  return true;
}

////////////////////////////////////////////////////
bool WaterPump_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3;

  txt1<<(*p_eff);
  t_eff->SetValue(txt1);

  txt2<<(*p_pressure_out);
  t_pressure_out->SetValue(txt2);

  txt3<<(*p_pressure_change);
  t_pressure_change->SetValue(txt3);

  wxCommandEvent event;
  if (*p_case_type)
    {
      r_case_type_eva->SetValue(false);
      r_case_type_des->SetValue(true);
    }
  else
    {
      r_case_type_eva->SetValue(true);
      r_case_type_des->SetValue(false);
    }

   OnCaseTypeChange(event);
  return true;
}

void WaterPump_UI_Dialog::Lock(bool l)
{
}

///////////////////////////////////////////////////
void WaterPump_UI_Dialog::OnCaseTypeChange(wxCommandEvent &event)
{
  if (r_case_type_eva->GetValue())
    {
      t_pressure_change->Enable(true);
      t_pressure_out->Enable(false);
    }
  else
    {
      t_pressure_change->Enable(false);
      t_pressure_out->Enable(true);
    }
}
