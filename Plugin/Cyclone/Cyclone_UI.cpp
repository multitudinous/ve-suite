#include "Cyclone_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Cyclone_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
Cyclone_UI_Dialog
::Cyclone_UI_Dialog
(wxWindow* parent, int id,
  double* diameter,
  double* particle_turn_count,
  double* velocity_heads)
: UIDialog((wxWindow *) parent, id, "Cyclone"),
  p_diameter(diameter),
  p_particle_turn_count(particle_turn_count),
  p_velocity_heads(velocity_heads)
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

  data_row->Add(10, 5, 0);
  data_row->Add(data_first_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_second_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_third_row, 0);
  data_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  wxStaticText * label0 = new wxStaticText(this, -1, " Cyclone Diameter (m): ", wxDefaultPosition, wxSize(200, 17));
  t_diameter = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_row->Add(label0);
  data_first_row->Add(t_diameter);

  wxStaticText * label1 = new wxStaticText(this, -1, " Particle Turn Count-N: ", wxDefaultPosition, wxSize(200, 17));
  t_particle_turn_count = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_particle_turn_count);

  wxStaticText * label2 = new wxStaticText(this, -1, " Velocity Heads-K: ", wxDefaultPosition, wxSize(200, 17));
  t_velocity_heads = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_velocity_heads);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);
}

/////////////////////////////////////////////////////
Cyclone_UI_Dialog
::~Cyclone_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Cyclone_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_diameter->GetValue();
  (*p_diameter) = atof(txt.c_str());
  
  txt = t_particle_turn_count->GetValue();
  (*p_particle_turn_count) = atof(txt.c_str());

  txt = t_velocity_heads->GetValue();
  (*p_velocity_heads) = atof(txt.c_str());

  return true;
}

////////////////////////////////////////////////////
bool Cyclone_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3;
   
  txt1<<(*p_diameter);
  t_diameter->SetValue(txt1);

  txt2<<(*p_particle_turn_count);
  t_particle_turn_count->SetValue(txt2);

  txt3<<(*p_velocity_heads);
  t_velocity_heads->SetValue(txt3);
  
  return true;
}

void Cyclone_UI_Dialog::Lock(bool l)
{
}

