#include "CatalyticConverter_UI.h"

BEGIN_EVENT_TABLE(CatalyticConverter_UI_Dialog, UIDialog)
  EVT_RADIOBUTTON(R_CASETYPE_EVA, CatalyticConverter_UI_Dialog::OnCaseTypeChange)
  EVT_RADIOBUTTON(R_CASETYPE_DES, CatalyticConverter_UI_Dialog::OnCaseTypeChange)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(CatalyticConverter_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
CatalyticConverter_UI_Dialog
::CatalyticConverter_UI_Dialog
(wxWindow* parent, int id,
  double* effect,
  double* site_den,
  double* hydDiameter,
  double* length,
  double* conversion,
  double* velocity,
  long* case_type)
: UIDialog((wxWindow *) parent, id, "CatalyticConverter"),
  p_effect(effect),
  p_site_den(site_den),
  p_hydDiameter(hydDiameter),
  p_length(length),
  p_conversion(conversion),
  p_velocity(velocity),
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
  wxBoxSizer *data_sixth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_seventh_row = new wxBoxSizer(wxHORIZONTAL);

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
  data_row->Add(data_sixth_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_seventh_row, 0);
  data_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  r_case_type_eva = new wxRadioButton(this, R_CASETYPE_EVA, _T(" Evaluation Mode "), wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  r_case_type_des = new wxRadioButton(this, R_CASETYPE_DES, _T(" Design Mode "), wxDefaultPosition, wxDefaultSize);
  data_first_row->Add(r_case_type_eva);
  data_first_row->Add(r_case_type_des);

  wxStaticText * label1 = new wxStaticText(this, -1, " Effectiveness Factor ", wxDefaultPosition, wxSize(200, 17));
  t_effect = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_effect);

  wxStaticText * label2 = new wxStaticText(this, -1, " Site Density ", wxDefaultPosition, wxSize(200, 17));
  t_site_den = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_site_den);

  wxStaticText * label3 = new wxStaticText(this, -1, " Hydraulic Diameter (m) ", wxDefaultPosition, wxSize(200, 17));
  t_hydDiameter = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_hydDiameter);

  wxStaticText * label4 = new wxStaticText(this, -1, " Length (m) ", wxDefaultPosition, wxSize(200, 17));
  t_length = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_fifth_row->Add(label4);
  data_fifth_row->Add(t_length);

  wxStaticText * label5 = new wxStaticText(this, -1, " Conversion (%) ", wxDefaultPosition, wxSize(200, 17));
  t_conversion = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_sixth_row->Add(label5);
  data_sixth_row->Add(t_conversion);

  wxStaticText * label6 = new wxStaticText(this, -1, " Inlet Velocity (m/sec) ", wxDefaultPosition, wxSize(200, 17));
  t_velocity = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_seventh_row->Add(label6);
  data_seventh_row->Add(t_velocity);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  
}

/////////////////////////////////////////////////////
CatalyticConverter_UI_Dialog
::~CatalyticConverter_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool CatalyticConverter_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_effect->GetValue();
  (*p_effect) = atof(txt.c_str());
  
  txt = t_site_den->GetValue();
  (*p_site_den) = atof(txt.c_str());

  txt = t_hydDiameter->GetValue();
  (*p_hydDiameter) = atof(txt.c_str());

  txt = t_length->GetValue();
  (*p_length) = atof(txt.c_str());

  txt = t_conversion->GetValue();
  (*p_conversion) = atof(txt.c_str());
  
  txt = t_velocity->GetValue();
  (*p_velocity) = atof(txt.c_str());
  
  *p_case_type = r_case_type_des->GetValue();

  return true;
}

////////////////////////////////////////////////////
bool CatalyticConverter_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6;

  txt1<<(*p_effect);
  t_effect->SetValue(txt1);

  txt2<<(*p_site_den);
  t_site_den->SetValue(txt2);

  txt3<<(*p_hydDiameter);
  t_hydDiameter->SetValue(txt3);

  txt4<<(*p_length);
  t_length->SetValue(txt4);

  txt5<<(*p_conversion);
  t_conversion->SetValue(txt5);

  txt6<<(*p_velocity);
  t_velocity->SetValue(txt6);

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

void CatalyticConverter_UI_Dialog::Lock(bool l)
{
}

///////////////////////////////////////////////////
void CatalyticConverter_UI_Dialog::OnCaseTypeChange(wxCommandEvent &event)
{
  if (r_case_type_eva->GetValue())
    {
      t_length->Enable(true);
      t_conversion->Enable(false);
    }
  else
    {
      t_length->Enable(false);
      t_conversion->Enable(true);
    }
}
