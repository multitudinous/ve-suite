#include "ChlorineBed_UI.h"

BEGIN_EVENT_TABLE(ChlorineBed_UI_Dialog, UIDialog)
  EVT_RADIOBUTTON(R_HCL_EFF, ChlorineBed_UI_Dialog::OnHCLChange)
  EVT_RADIOBUTTON(R_HCL_PPM, ChlorineBed_UI_Dialog::OnHCLChange)
  EVT_RADIOBUTTON(R_PRES_SPEC, ChlorineBed_UI_Dialog::OnPRESChange)
  EVT_RADIOBUTTON(R_PRES_CALC, ChlorineBed_UI_Dialog::OnPRESChange)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(ChlorineBed_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
ChlorineBed_UI_Dialog
::ChlorineBed_UI_Dialog
(wxWindow* parent, int id,
  double* Temp_change,
  double* HCL_eff,
  double* HCL_ppm,
  double* Pres_drop,
  double* Bed_diameter,
  double* Bed_depth,
  double* Bed_void_frac,
  double* Particle_size,
  double* Particle_sphericity,
  long* rHCL_eff_ppm,
  long* rPresDrop_spec_calc)
: UIDialog((wxWindow *) parent, id, "ChlorineBed"),
  p_Temp_change(Temp_change),
  p_HCL_eff(HCL_eff),
  p_HCL_ppm(HCL_ppm),
  p_Pres_drop(Pres_drop),
  p_Bed_diameter(Bed_diameter),
  p_Bed_depth(Bed_depth),
  p_Bed_void_frac(Bed_void_frac),
  p_Particle_size(Particle_size),
  p_Particle_sphericity(Particle_sphericity),
  p_rHCL_eff_ppm(rHCL_eff_ppm),
  p_rPresDrop_spec_calc(rPresDrop_spec_calc)
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

  wxStaticBox *hcl_box = new wxStaticBox(this, -1, "HCL");
  wxStaticBox *pres_box = new wxStaticBox(this, -1, "Pressure Drop");

  wxBoxSizer* temp_row = new wxBoxSizer(wxHORIZONTAL);
  wxStaticBoxSizer* hcl_row = new wxStaticBoxSizer(hcl_box, wxVERTICAL);
  wxStaticBoxSizer* pres_row = new wxStaticBoxSizer(pres_box, wxVERTICAL);
  wxBoxSizer *ok_row=new wxBoxSizer(wxHORIZONTAL);


  top_sizer->Add(5,10); //the top margin
  top_sizer->Add(temp_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 5, 0);
  top_sizer->Add(hcl_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 5, 0);
  top_sizer->Add(pres_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin

  wxStaticText * label0 = new wxStaticText(this, -1, " Temperature change (F) ", wxDefaultPosition, wxSize(200, 17));
  t_Temp_change =  new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  temp_row->Add(label0);
  temp_row->Add(t_Temp_change);

  
  r_HCL_eff = new wxRadioButton(this, R_HCL_EFF, _T(" HCL efficiency (%) "), wxDefaultPosition, wxSize(200, 20), wxRB_GROUP);
  t_HCL_eff = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  
  r_HCL_ppm = new wxRadioButton(this, R_HCL_PPM, _T(" HCL ppm : "),wxDefaultPosition, wxSize(200, 20));
  r_HCL_ppm->SetValue(true);
  t_HCL_ppm = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));

  wxBoxSizer* hcl_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* hcl_second_row = new wxBoxSizer(wxHORIZONTAL);

  hcl_row->Add(10, 5, 0);
  hcl_row->Add(hcl_first_row);
  hcl_row->Add(10, 3, 0);
  hcl_row->Add(hcl_second_row);
  hcl_row->Add(10, 3, 0);

  hcl_first_row->Add(r_HCL_eff);
  hcl_first_row->Add(t_HCL_eff);
  hcl_second_row->Add(r_HCL_ppm);
  hcl_second_row->Add(t_HCL_ppm);

  wxBoxSizer* spec_pres_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* calc_pres_row = new wxBoxSizer(wxVERTICAL);

  pres_row->Add(10, 5, 0);
  pres_row->Add(spec_pres_row);
  pres_row->Add(10, 3, 0);
  pres_row->Add(calc_pres_row);
  pres_row->Add(10, 3, 0);
  
  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);

  
  r_PresDrop_spec = new wxRadioButton(this, R_PRES_SPEC, _T(" Specify Pressure Drop (psi): "), wxDefaultPosition, wxSize(200, 20), wxRB_GROUP);
  r_PresDrop_spec->SetValue(true);
  t_Pres_drop = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));

  spec_pres_row->Add(r_PresDrop_spec);
  spec_pres_row->Add(t_Pres_drop);

  r_PresDrop_calc = new wxRadioButton(this, R_PRES_CALC, _T(" Calculate Pressure Drop: "), wxDefaultPosition, wxSize(200, 20));

  wxBoxSizer* cp_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* cp_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* cp_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* cp_forth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* cp_fifth_row = new wxBoxSizer(wxHORIZONTAL);

  calc_pres_row->Add(r_PresDrop_calc);
  calc_pres_row->Add(5, 3, 0);
  calc_pres_row->Add(cp_first_row);
  calc_pres_row->Add(5, 3, 0);
  calc_pres_row->Add(cp_second_row);
  calc_pres_row->Add(5, 3, 0);
  calc_pres_row->Add(cp_third_row);
  calc_pres_row->Add(5, 3, 0);
  calc_pres_row->Add(cp_forth_row);
  calc_pres_row->Add(5, 3, 0);
  calc_pres_row->Add(cp_fifth_row);
  calc_pres_row->Add(5, 5, 0);

    
  wxStaticText * label1 = new wxStaticText(this, -1, " Bed diameter (m) ", wxDefaultPosition, wxSize(200, 17));
  t_Bed_diameter = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  cp_first_row->Add(label1);
  cp_first_row->Add(t_Bed_diameter);

  wxStaticText * label2 = new wxStaticText(this, -1, " Bed depth (m) ", wxDefaultPosition, wxSize(200, 17));
  t_Bed_depth = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  cp_second_row->Add(label2);
  cp_second_row->Add(t_Bed_depth);

  wxStaticText * label3 = new wxStaticText(this, -1, " Bed void fraction ", wxDefaultPosition, wxSize(200, 17));
  t_Bed_void_frac = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  cp_third_row->Add(label3);
  cp_third_row->Add(t_Bed_void_frac);
  
  wxStaticText * label4 = new wxStaticText(this, -1, " Particle size (m) ", wxDefaultPosition, wxSize(200, 17));
  t_Particle_size = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  cp_forth_row->Add(label4);
  cp_forth_row->Add(t_Particle_size);

  wxStaticText * label5 = new wxStaticText(this, -1, " Particle sphericity ", wxDefaultPosition, wxSize(200, 17));
  t_Particle_sphericity = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  cp_fifth_row->Add(label5);
  cp_fifth_row->Add(t_Particle_sphericity);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);
    
}

/////////////////////////////////////////////////////
ChlorineBed_UI_Dialog
::~ChlorineBed_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool ChlorineBed_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_Temp_change->GetValue();
  (*p_Temp_change) = atof(txt.c_str());
  
  txt = t_HCL_eff->GetValue();
  (*p_HCL_eff) = atof(txt.c_str());

  txt = t_HCL_ppm->GetValue();
  (*p_HCL_ppm) = atof(txt.c_str());
  
  txt = t_Pres_drop->GetValue();
  (*p_Pres_drop) = atof(txt.c_str());
  
  txt = t_Bed_diameter->GetValue();
  (*p_Bed_diameter) = atof(txt.c_str());

  txt = t_Bed_depth->GetValue();
  (*p_Bed_depth) = atof(txt.c_str());

  txt = t_Bed_void_frac->GetValue();
  (*p_Bed_void_frac) = atof(txt.c_str());

  txt = t_Particle_size->GetValue();
  (*p_Particle_size) = atof(txt.c_str());

  txt = t_Particle_sphericity->GetValue();
  (*p_Particle_sphericity) = atof(txt.c_str());

  *p_rHCL_eff_ppm = r_HCL_ppm->GetValue();
  
  *p_rPresDrop_spec_calc = r_PresDrop_calc->GetValue();


  return true;
}

////////////////////////////////////////////////////
bool ChlorineBed_UI_Dialog::TransferDataToWindow()
{

  wxString txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt8, txt9;
   
  txt1<<(*p_Temp_change);
  t_Temp_change->SetValue(txt1);

  txt2<<(*p_HCL_eff);
  t_HCL_eff->SetValue(txt2);

  txt3<<(*p_HCL_ppm);
  t_HCL_ppm->SetValue(txt3);

  txt4<<(*p_Pres_drop);
  t_Pres_drop->SetValue(txt4);

  txt5<<(*p_Bed_diameter);
  t_Bed_diameter->SetValue(txt5);

  txt6<<(*p_Bed_depth);
  t_Bed_depth->SetValue(txt6);

  txt7<<(*p_Bed_void_frac);
  t_Bed_void_frac->SetValue(txt7);

  txt8<<(*p_Particle_size);
  t_Particle_size->SetValue(txt8);

  txt9<<(*p_Particle_sphericity);
  t_Particle_sphericity->SetValue(txt9);

  wxCommandEvent event;
  if (*p_rHCL_eff_ppm)
    {
      r_HCL_eff->SetValue(false);
      r_HCL_ppm->SetValue(true);
    }
  else
    {
      r_HCL_eff->SetValue(true);
      r_HCL_ppm->SetValue(false);
    }
  if (*p_rPresDrop_spec_calc)
    {
      r_PresDrop_spec->SetValue(false);
      r_PresDrop_calc->SetValue(true);
    }
  else
    {
      r_PresDrop_spec->SetValue(true);
      r_PresDrop_calc->SetValue(false);
    }


  OnHCLChange(event);
  OnPRESChange(event);

  return true;
}

///////////////////////////////////////////////////
void ChlorineBed_UI_Dialog::Lock(bool l)
{
}

///////////////////////////////////////////////////
void ChlorineBed_UI_Dialog::OnHCLChange(wxCommandEvent &event)
{
  if (r_HCL_eff->GetValue())
    {
      t_HCL_eff->Enable(true);
      t_HCL_ppm->Enable(false);
    }
  else
    {
      t_HCL_eff->Enable(false);
      t_HCL_ppm->Enable(true);
    }
}

///////////////////////////////////////////////////
void ChlorineBed_UI_Dialog::OnPRESChange(wxCommandEvent &event)
{
  if (r_PresDrop_spec->GetValue())
    {
      t_Pres_drop->Enable(true);
      t_Bed_diameter->Enable(false);
      t_Bed_depth->Enable(false);
      t_Bed_void_frac->Enable(false);
      t_Particle_size->Enable(false);
      t_Particle_sphericity->Enable(false);
    }
  else
    {
      t_Pres_drop->Enable(false);
      t_Bed_diameter->Enable(true);
      t_Bed_depth->Enable(true);
      t_Bed_void_frac->Enable(true);
      t_Particle_size->Enable(true);
      t_Particle_sphericity->Enable(true);
    }
}
