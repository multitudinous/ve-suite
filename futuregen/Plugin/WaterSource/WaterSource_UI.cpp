#include "WaterSource_UI.h"

BEGIN_EVENT_TABLE(WaterSource_UI_Dialog, UIDialog)
  EVT_RADIOBUTTON(R_CASETYPE_EVA, WaterSource_UI_Dialog::OnCaseTypeChange)
  EVT_RADIOBUTTON(R_CASETYPE_DES, WaterSource_UI_Dialog::OnCaseTypeChange)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(WaterSource_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
WaterSource_UI_Dialog
::WaterSource_UI_Dialog
(wxWindow* parent, int id,
  double* temp,
  double* pres,
  double* enth,
  double* flow,
  long* case_type)
: UIDialog((wxWindow *) parent, id, "WaterSource"),
  p_temp(temp),
  p_pres(pres),
  p_enth(enth),
  p_flow(flow),
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
  wxBoxSizer *data_fifth_row = new wxBoxSizer(wxHORIZONTAL);

  data_row->Add(10, 5, 0);
  data_row->Add(data_first_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_second_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_third_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_forth_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_fifth_row, 0);
  data_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  r_case_type_eva = new wxRadioButton(this, R_CASETYPE_EVA, _T(" Temperature/Pressure "), wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  r_case_type_des = new wxRadioButton(this, R_CASETYPE_DES, _T(" Pressure/Enthalpy "), wxDefaultPosition, wxDefaultSize);
  data_first_row->Add(r_case_type_eva);
  data_first_row->Add(r_case_type_des);

  wxStaticText * label1 = new wxStaticText(this, -1, " Temperature (K): ", wxDefaultPosition, wxSize(200, 17));
  t_temp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_temp);

  wxStaticText * label2 = new wxStaticText(this, -1, " Pressure (Pa): ", wxDefaultPosition, wxSize(200, 17));
  t_pres = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_pres);

  wxStaticText * label3 = new wxStaticText(this, -1, " Enthalpy (J/kg): ", wxDefaultPosition, wxSize(200, 17));
  t_enth = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_enth);

  wxStaticText * label4 = new wxStaticText(this, -1, " Flowrate (kg/s): ", wxDefaultPosition, wxSize(200, 17));
  t_flow = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_fifth_row->Add(label4);
  data_fifth_row->Add(t_flow);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);     
}

/////////////////////////////////////////////////////
WaterSource_UI_Dialog
::~WaterSource_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool WaterSource_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_temp->GetValue();
  (*p_temp) = atof(txt.c_str());
  
  txt = t_pres->GetValue();
  (*p_pres) = atof(txt.c_str());

  txt = t_enth->GetValue();
  (*p_enth) = atof(txt.c_str());

  txt = t_flow->GetValue();
  (*p_flow) = atof(txt.c_str());

  *p_case_type = r_case_type_des->GetValue();
  return true;
}

////////////////////////////////////////////////////
bool WaterSource_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4;
  
  txt1<<(*p_temp);
  t_temp->SetValue(txt1);

  txt2<<(*p_pres);
  t_pres->SetValue(txt2);

  txt3<<(*p_enth);
  t_enth->SetValue(txt3);

  txt4<<(*p_flow);
  t_flow->SetValue(txt4);

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

void WaterSource_UI_Dialog::Lock(bool l)
{
}

///////////////////////////////////////////////////
void WaterSource_UI_Dialog::OnCaseTypeChange(wxCommandEvent &event)
{
  if (r_case_type_eva->GetValue())
    {
      t_enth->Enable(false);
      t_temp->Enable(true);
    }
  else
    {
      t_enth->Enable(true);
      t_temp->Enable(false);
    }
}

