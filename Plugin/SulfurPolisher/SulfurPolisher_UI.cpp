#include "SulfurPolisher_UI.h"

BEGIN_EVENT_TABLE(SulfurPolisher_UI_Dialog, UIDialog)
  EVT_RADIOBUTTON(R_H2S_EFF, SulfurPolisher_UI_Dialog::OnH2SChange)
  EVT_RADIOBUTTON(R_H2S_PPM, SulfurPolisher_UI_Dialog::OnH2SChange)
  EVT_RADIOBUTTON(R_COS_EFF, SulfurPolisher_UI_Dialog::OnCOSChange)
  EVT_RADIOBUTTON(R_COS_PPM, SulfurPolisher_UI_Dialog::OnCOSChange)
  EVT_RADIOBUTTON(R_PRES_SPEC, SulfurPolisher_UI_Dialog::OnPRESChange)
  EVT_RADIOBUTTON(R_PRES_CALC, SulfurPolisher_UI_Dialog::OnPRESChange)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(SulfurPolisher_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
SulfurPolisher_UI_Dialog
::SulfurPolisher_UI_Dialog
(wxWindow* parent, int id,
  double* Temp_change,
  double* H2S_eff,
  double* H2S_ppm,
  double* COS_eff,
  double* COS_ppm,
  double* Pres_drop,
  double* Bed_diameter,
  double* Bed_depth,
  double* Bed_void_frac,
  double* Particle_size,
  double* Particle_sphericity,
  long* rH2S_eff_ppm,
  long* rCOS_eff_ppm,
  long* rPresDrop_spec_calc)
: UIDialog((wxWindow *) parent, id, "SulfurPolisher"),
  p_Temp_change(Temp_change),
  p_H2S_eff(H2S_eff),
  p_H2S_ppm(H2S_ppm),
  p_COS_eff(COS_eff),
  p_COS_ppm(COS_ppm),
  p_Pres_drop(Pres_drop),
  p_Bed_diameter(Bed_diameter),
  p_Bed_depth(Bed_depth),
  p_Bed_void_frac(Bed_void_frac),
  p_Particle_size(Particle_size),
  p_Particle_sphericity(Particle_sphericity),
  p_rH2S_eff_ppm(rH2S_eff_ppm),
  p_rCOS_eff_ppm(rCOS_eff_ppm),
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

  wxStaticBox *h2s_box = new wxStaticBox(this, -1, "H2S");
  wxStaticBox *cos_box = new wxStaticBox(this, -1, "COS");
  wxStaticBox *pres_box = new wxStaticBox(this, -1, "Pressure Drop");

  wxBoxSizer* temp_row = new wxBoxSizer(wxHORIZONTAL);
  wxStaticBoxSizer* h2s_row = new wxStaticBoxSizer(h2s_box, wxVERTICAL);
  wxStaticBoxSizer* cos_row = new wxStaticBoxSizer(cos_box, wxVERTICAL);
  wxStaticBoxSizer* pres_row = new wxStaticBoxSizer(pres_box, wxVERTICAL);
  wxBoxSizer *ok_row=new wxBoxSizer(wxHORIZONTAL);


  top_sizer->Add(5,10); //the top margin
  top_sizer->Add(temp_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 5, 0);
  top_sizer->Add(h2s_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 5, 0);
  top_sizer->Add(cos_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 5, 0);
  top_sizer->Add(pres_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin

  wxStaticText * label0 = new wxStaticText(this, -1, " Temperature change (F) ", wxDefaultPosition, wxSize(200, 17));
  t_Temp_change =  new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  temp_row->Add(label0);
  temp_row->Add(t_Temp_change);

  
  r_H2S_eff = new wxRadioButton(this, R_H2S_EFF, _T(" H2S efficiency (%) "), wxDefaultPosition, wxSize(200, 20), wxRB_GROUP);
  t_H2S_eff = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  
  r_H2S_ppm = new wxRadioButton(this, R_H2S_PPM, _T(" H2S ppm : "),wxDefaultPosition, wxSize(200, 20));
  r_H2S_ppm->SetValue(true);
  t_H2S_ppm = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));

  wxBoxSizer* h2s_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* h2s_second_row = new wxBoxSizer(wxHORIZONTAL);

  h2s_row->Add(10, 5, 0);
  h2s_row->Add(h2s_first_row);
  h2s_row->Add(10, 3, 0);
  h2s_row->Add(h2s_second_row);
  h2s_row->Add(10, 3, 0);

  h2s_first_row->Add(r_H2S_eff);
  h2s_first_row->Add(t_H2S_eff);
  h2s_second_row->Add(r_H2S_ppm);
  h2s_second_row->Add(t_H2S_ppm);

  r_COS_eff = new wxRadioButton(this, R_COS_EFF, _T(" COS efficiency (%) "), wxDefaultPosition, wxSize(200, 20), wxRB_GROUP);
  t_COS_eff = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  
  r_COS_ppm = new wxRadioButton(this, R_COS_PPM, _T(" COS ppm : "),wxDefaultPosition, wxSize(200, 20));
  r_COS_ppm->SetValue(true);
  t_COS_ppm = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));

  wxBoxSizer* cos_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* cos_second_row = new wxBoxSizer(wxHORIZONTAL);

  cos_row->Add(10, 5, 0);
  cos_row->Add(cos_first_row);
  cos_row->Add(10, 3, 0);
  cos_row->Add(cos_second_row);
  cos_row->Add(10, 3, 0);

  cos_first_row->Add(r_COS_eff);
  cos_first_row->Add(t_COS_eff);
  cos_second_row->Add(r_COS_ppm);
  cos_second_row->Add(t_COS_ppm);

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
SulfurPolisher_UI_Dialog
::~SulfurPolisher_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool SulfurPolisher_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_Temp_change->GetValue();
  (*p_Temp_change) = atof(txt.c_str());
  
  txt = t_H2S_eff->GetValue();
  (*p_H2S_eff) = atof(txt.c_str());

  txt = t_H2S_ppm->GetValue();
  (*p_H2S_ppm) = atof(txt.c_str());

  txt = t_COS_eff->GetValue();
  (*p_COS_eff) = atof(txt.c_str());

  txt = t_COS_ppm->GetValue();
  (*p_COS_ppm) = atof(txt.c_str());
  
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

  *p_rH2S_eff_ppm = r_H2S_ppm->GetValue();

  *p_rCOS_eff_ppm = r_COS_ppm->GetValue();
  
  *p_rPresDrop_spec_calc = r_PresDrop_calc->GetValue();
  return true;
}

////////////////////////////////////////////////////
bool SulfurPolisher_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt8, txt9,
    txt10, txt11;
   
  txt1<<(*p_Temp_change);
  t_Temp_change->SetValue(txt1);

  txt2<<(*p_H2S_eff);
  t_H2S_eff->SetValue(txt2);

  txt3<<(*p_H2S_ppm);
  t_H2S_ppm->SetValue(txt3);

  txt10<<(*p_COS_eff);
  t_COS_eff->SetValue(txt2);

  txt11<<(*p_COS_ppm);
  t_COS_ppm->SetValue(txt3);

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
  if (*p_rH2S_eff_ppm)
    {
      r_H2S_eff->SetValue(false);
      r_H2S_ppm->SetValue(true);
    }
  else
    {
      r_H2S_eff->SetValue(true);
      r_H2S_ppm->SetValue(false);
    }

  if (*p_rCOS_eff_ppm)
    {
      r_COS_eff->SetValue(false);
      r_COS_ppm->SetValue(true);
    }
  else
    {
      r_COS_eff->SetValue(true);
      r_COS_ppm->SetValue(false);
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


  OnH2SChange(event);
  OnCOSChange(event);
  OnPRESChange(event);

  return true;
}

void SulfurPolisher_UI_Dialog::Lock(bool l)
{
}

///////////////////////////////////////////////////
void SulfurPolisher_UI_Dialog::OnH2SChange(wxCommandEvent &event)
{
  if (r_H2S_eff->GetValue())
    {
      t_H2S_eff->Enable(true);
      t_H2S_ppm->Enable(false);
    }
  else
    {
      t_H2S_eff->Enable(false);
      t_H2S_ppm->Enable(true);
    }
}

///////////////////////////////////////////////////
void SulfurPolisher_UI_Dialog::OnCOSChange(wxCommandEvent &event)
{
  if (r_COS_eff->GetValue())
    {
      t_COS_eff->Enable(true);
      t_COS_ppm->Enable(false);
    }
  else
    {
      t_COS_eff->Enable(false);
      t_COS_ppm->Enable(true);
    }
}

///////////////////////////////////////////////////
void SulfurPolisher_UI_Dialog::OnPRESChange(wxCommandEvent &event)
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
