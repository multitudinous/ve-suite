#include "MembraneReactor_UI.h"

BEGIN_EVENT_TABLE(MembraneReactor_UI_Dialog, UIDialog)
  EVT_RADIOBUTTON(R_CASETYPE_EVA, MembraneReactor_UI_Dialog::OnCaseTypeChange)
  EVT_RADIOBUTTON(R_CASETYPE_DES, MembraneReactor_UI_Dialog::OnCaseTypeChange)
  EVT_CHECKBOX(F_PRE_MR, MembraneReactor_UI_Dialog::OnF_PRE_MRChange)
  EVT_CHECKBOX(F_H2O_CO, MembraneReactor_UI_Dialog::OnF_H2O_COChange)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(MembraneReactor_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
MembraneReactor_UI_Dialog
::MembraneReactor_UI_Dialog
(wxWindow* parent, int id,
  double* memb_diameter,
  double* Pd_thickness,
  double* L_rxr,
  double* CO_conv_want,
  double* shell_diameter,
  double* mr_inlet_temp,
  double* H2O_CO,
  long* case_type,
  long* n_modules,
  long* f_pre_mr,
  long* f_H2O_CO)
: UIDialog((wxWindow *) parent, id, "MembraneReactor"),
  p_memb_diameter(memb_diameter),
  p_Pd_thickness(Pd_thickness),
  p_L_rxr(L_rxr),
  p_CO_conv_want(CO_conv_want),
  p_shell_diameter(shell_diameter),
  p_mr_inlet_temp(mr_inlet_temp),
  p_H2O_CO(H2O_CO),
  p_case_type(case_type),
  p_n_modules(n_modules),
  p_f_pre_mr(f_pre_mr),
  p_f_H2O_CO(f_H2O_CO)
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
  wxBoxSizer *data_eighth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_ninth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_tenth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_eleventh_row = new wxBoxSizer(wxHORIZONTAL);
  
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
  data_row->Add(data_eighth_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_ninth_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_tenth_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_eleventh_row, 0);
  data_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  r_case_type_eva = new wxRadioButton(this, R_CASETYPE_EVA, _T(" Evaluation Mode "), wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  r_case_type_des = new wxRadioButton(this, R_CASETYPE_DES, _T(" Design Mode "), wxDefaultPosition, wxDefaultSize);
  data_first_row->Add(r_case_type_eva);
  data_first_row->Add(r_case_type_des);

  wxStaticText * label1 = new wxStaticText(this, -1, " Membrane Diameter (m) ", wxDefaultPosition, wxSize(200, 17));
  t_memb_diameter = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_memb_diameter);

  wxStaticText * label2 = new wxStaticText(this, -1, " Membrane Thickness (m) ", wxDefaultPosition, wxSize(200, 17));
  t_Pd_thickness = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_Pd_thickness);

  wxStaticText * label3 = new wxStaticText(this, -1, " Reactor Length (m) ", wxDefaultPosition, wxSize(200, 17));
  t_L_rxr = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_L_rxr);

  wxStaticText * label4 = new wxStaticText(this, -1, " Desired CO conversion ", wxDefaultPosition, wxSize(200, 17));
  t_CO_conv_want = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_fifth_row->Add(label4);
  data_fifth_row->Add(t_CO_conv_want);

  wxStaticText * label5 = new wxStaticText(this, -1, " Number of Modules ", wxDefaultPosition, wxSize(200, 17));
  t_n_modules = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_sixth_row->Add(label5);
  data_sixth_row->Add(t_n_modules);

  wxStaticText * label6 = new wxStaticText(this, -1, " Shell Diameter (m) ", wxDefaultPosition, wxSize(200, 17));
  t_shell_diameter = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_seventh_row->Add(label6);
  data_seventh_row->Add(t_shell_diameter);

  c_f_pre_mr = new wxCheckBox(this, F_PRE_MR, "Allow Preshift Reactor ", wxDefaultPosition, wxSize(280,20));
  data_eighth_row->Add(c_f_pre_mr);

  wxStaticText * label7 = new wxStaticText(this, -1, " M.R. inlet temp (K) ", wxDefaultPosition, wxSize(200, 17));
  t_mr_inlet_temp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_ninth_row->Add(label7);
  data_ninth_row->Add(t_mr_inlet_temp);

  c_f_H2O_CO = new wxCheckBox(this, F_H2O_CO, "Specity H2O : CO ", wxDefaultPosition, wxSize(280,20));
  data_tenth_row->Add(c_f_H2O_CO);

  wxStaticText * label8 = new wxStaticText(this, -1, " H2O : CO (ratio) ", wxDefaultPosition, wxSize(200, 17));
  t_H2O_CO = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_eleventh_row->Add(label8);
  data_eleventh_row->Add(t_H2O_CO);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);    
}

/////////////////////////////////////////////////////
MembraneReactor_UI_Dialog
::~MembraneReactor_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool MembraneReactor_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_memb_diameter->GetValue();
  (*p_memb_diameter) = atof(txt.c_str());
  
  txt = t_Pd_thickness->GetValue();
  (*p_Pd_thickness) = atof(txt.c_str());

  txt = t_L_rxr->GetValue();
  (*p_L_rxr) = atof(txt.c_str());

  txt = t_CO_conv_want->GetValue();
  (*p_CO_conv_want) = atof(txt.c_str());

  txt = t_n_modules->GetValue();
  (*p_n_modules) = atoi(txt.c_str());

  txt = t_shell_diameter->GetValue();
  (*p_shell_diameter) = atof(txt.c_str());

  txt = t_mr_inlet_temp->GetValue();
  (*p_mr_inlet_temp) = atof(txt.c_str());

  txt = t_H2O_CO->GetValue();
  (*p_H2O_CO) = atof(txt.c_str());

  *p_case_type = r_case_type_des->GetValue();
  *p_f_pre_mr = c_f_pre_mr->GetValue();
  *p_f_H2O_CO = c_f_H2O_CO->GetValue();

  return true;
}

////////////////////////////////////////////////////
bool MembraneReactor_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6,txt7, txt8;
  
  txt1<<(*p_memb_diameter);
  t_memb_diameter->SetValue(txt1);

  txt2<<(*p_Pd_thickness);
  t_Pd_thickness->SetValue(txt2);

  txt3<<(*p_L_rxr);
  t_L_rxr->SetValue(txt3);

  txt4<<(*p_CO_conv_want);
  t_CO_conv_want->SetValue(txt4);

  txt5<<(*p_shell_diameter);
  t_shell_diameter->SetValue(txt5);

  txt6<<(*p_mr_inlet_temp);
  t_mr_inlet_temp->SetValue(txt6);

  txt7<<(*p_H2O_CO);
  t_H2O_CO->SetValue(txt7);

  txt8<<(*p_n_modules);
  t_n_modules->SetValue(txt8);

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

  if (*p_f_pre_mr)
    c_f_pre_mr->SetValue(true);
  else
    c_f_pre_mr->SetValue(false);
  
  if (*p_f_H2O_CO)
    c_f_H2O_CO->SetValue(true);
  else
    c_f_H2O_CO->SetValue(false);
  
  OnCaseTypeChange(event);
  OnF_PRE_MRChange(event);
  OnF_H2O_COChange(event);
  
  return true;
}

void MembraneReactor_UI_Dialog::Lock(bool l)
{
}

///////////////////////////////////////////////////
void MembraneReactor_UI_Dialog::OnCaseTypeChange(wxCommandEvent &event)
{
  if (r_case_type_eva->GetValue())
    {
      t_L_rxr->Enable(true);
      t_CO_conv_want->Enable(false);
    }
  else
    {
      t_L_rxr->Enable(false);
      t_CO_conv_want->Enable(true);
    }
}

///////////////////////////////////////////////////
void MembraneReactor_UI_Dialog::OnF_PRE_MRChange(wxCommandEvent &event)
{
  if (c_f_pre_mr->GetValue())
    t_mr_inlet_temp->Enable(true);
  else
    t_mr_inlet_temp->Enable(false);
    
}

///////////////////////////////////////////////////
void MembraneReactor_UI_Dialog::OnF_H2O_COChange(wxCommandEvent &event)
{
  if (c_f_H2O_CO->GetValue())
    t_H2O_CO->Enable(true);
  else
    t_H2O_CO->Enable(false);
    
}
