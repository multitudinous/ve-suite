#include "Recuperator_UI.h"

BEGIN_EVENT_TABLE(Recuperator_UI_Dialog, UIDialog)
  EVT_CHECKBOX(FINS, Recuperator_UI_Dialog::OnFinsChange)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(Recuperator_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
Recuperator_UI_Dialog
::Recuperator_UI_Dialog
(wxWindow* parent, int id,
  double* Sl,
  double* St,
  double* tube_id,
  double* tube_od,
  double* tube_length,
  double* int_press_drop,
  double* ext_press_drop,
  double* fin_effect,
  string* arrangement,
  string* tube_config,
  long* num_tubeL,
  long* num_tubeX,
  long* use_fins)
: UIDialog((wxWindow *) parent, id, "Recuperator"),
  p_Sl(Sl),
  p_St(St),
  p_tube_id(tube_id),
  p_tube_od(tube_od),
  p_tube_length(tube_length),
  p_int_press_drop(int_press_drop),
  p_ext_press_drop(ext_press_drop),
  p_fin_effect(fin_effect),
  p_arrangement(arrangement),
  p_tube_config(tube_config),
  p_num_tubeL(num_tubeL),
  p_num_tubeX(num_tubeX),
  p_use_fins(use_fins)
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
  wxBoxSizer *data_twelvth_row = new wxBoxSizer(wxHORIZONTAL);

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
  data_row->Add(data_twelvth_row, 0);
  data_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  wxStaticText * label0 = new wxStaticText(this, -1, " Arrangement ", wxDefaultPosition, wxSize(140, 17));
  wxString arrangement_val[] = {wxT("Counter Flow"), wxT("Parallel Flow"), wxT("Cross Flow"), wxT("Shell & Tube")};
  cb_arrangement = new wxComboBox(this, -1, wxT("Shell & Tube"), wxDefaultPosition, wxSize(140, 20), 4, arrangement_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  data_first_row->Add(label0);
  data_first_row->Add(cb_arrangement);

  wxStaticText * label1 = new wxStaticText(this, -1, " Tube Configuration ", wxDefaultPosition, wxSize(140, 17));
  wxString tube_config_val[] = {wxT("Inline"), wxT("Staggered")};
  cb_tube_config = new wxComboBox(this, -1, wxT("Inline"), wxDefaultPosition, wxSize(140, 20), 2, tube_config_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  data_second_row->Add(label1);
  data_second_row->Add(cb_tube_config);

  wxStaticText * label2 = new wxStaticText(this, -1, " Number of Tubes (Long) ", wxDefaultPosition, wxSize(200, 17));
  t_num_tubeL = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_num_tubeL);

  wxStaticText * label3 = new wxStaticText(this, -1, " Number of Tubes (Cross) ", wxDefaultPosition, wxSize(200, 17));
  t_num_tubeX = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_num_tubeX);

  wxStaticText * label4 = new wxStaticText(this, -1, " Tube Long Pitch-Sl (m) ", wxDefaultPosition, wxSize(200, 17));
  t_Sl = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_fifth_row->Add(label4);
  data_fifth_row->Add(t_Sl);

  wxStaticText * label5 = new wxStaticText(this, -1, " Tube Cross Pitch-St (m) ", wxDefaultPosition, wxSize(200, 17));
  t_St = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_sixth_row->Add(label5);
  data_sixth_row->Add(t_St);

  wxStaticText * label6 = new wxStaticText(this, -1, " Tube Inner Diameter (m) ", wxDefaultPosition, wxSize(200, 17));
  t_tube_id = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_seventh_row->Add(label6);
  data_seventh_row->Add(t_tube_id);

  wxStaticText * label7 = new wxStaticText(this, -1, " Tube Outer Diameter (m) ", wxDefaultPosition, wxSize(200, 17));
  t_tube_od = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_eighth_row->Add(label7);
  data_eighth_row->Add(t_tube_od);

  wxStaticText * label8 = new wxStaticText(this, -1, " Tube Length (m) ", wxDefaultPosition, wxSize(200, 17));
  t_tube_length = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_ninth_row->Add(label8);
  data_ninth_row->Add(t_tube_length);

  wxStaticText * label9 = new wxStaticText(this, -1, " Internal Pressure Drop (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_int_press_drop = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_tenth_row->Add(label9);
  data_tenth_row->Add(t_int_press_drop);

  wxStaticText * label10 = new wxStaticText(this, -1, " External Pressure Drop (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_ext_press_drop = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_eleventh_row->Add(label10);
  data_eleventh_row->Add(t_ext_press_drop);

  c_use_fins = new wxCheckBox(this, FINS, "Finned Tubes", wxDefaultPosition, wxSize(100,20));
  wxStaticText* label11 = new wxStaticText(this, -1, " Fin Effectiveness", wxDefaultPosition, wxSize(100, 20));
  t_fin_effect = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_twelvth_row->Add(c_use_fins);
  data_twelvth_row->Add(label11);
  data_twelvth_row->Add(t_fin_effect);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  
}

/////////////////////////////////////////////////////
Recuperator_UI_Dialog
::~Recuperator_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Recuperator_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_num_tubeL->GetValue();
  (*p_num_tubeL) = atoi(txt.c_str());
  
  txt = t_num_tubeX->GetValue();
  (*p_num_tubeX) = atoi(txt.c_str());

  txt = t_Sl->GetValue();
  (*p_Sl) = atof(txt.c_str());

  txt = t_St->GetValue();
  (*p_St) = atof(txt.c_str());
  
  txt = t_tube_id->GetValue();
  (*p_tube_id) = atof(txt.c_str());

  txt = t_tube_od->GetValue();
  (*p_tube_od) = atof(txt.c_str());

  txt = t_tube_length->GetValue();
  (*p_tube_length) = atof(txt.c_str());
  
  txt = t_int_press_drop->GetValue();
  (*p_int_press_drop) = atof(txt.c_str());

  txt = t_ext_press_drop->GetValue();
  (*p_ext_press_drop) = atof(txt.c_str());

  txt = t_fin_effect->GetValue();
  (*p_fin_effect) = atof(txt.c_str());

  txt = cb_arrangement->GetValue();
  (*p_arrangement) = txt.c_str();
  
  txt = cb_tube_config->GetValue();
  (*p_tube_config) = txt.c_str();

  *p_use_fins = c_use_fins->GetValue();
  
  return true;
}

////////////////////////////////////////////////////
bool Recuperator_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6,txt7, txt8, txt9, txt10;

  txt1<<(*p_num_tubeL);
  t_num_tubeL->SetValue(txt1);

  txt2<<(*p_num_tubeX);
  t_num_tubeX->SetValue(txt2);

  txt3<<(*p_Sl);
  t_Sl->SetValue(txt3);

  txt4<<(*p_St);
  t_St->SetValue(txt4);

  txt5<<(*p_tube_id);
  t_tube_id->SetValue(txt5);

  txt6<<(*p_tube_od);
  t_tube_od->SetValue(txt6);

  txt7<<(*p_tube_length);
  t_tube_length->SetValue(txt7);

  txt8<<(*p_int_press_drop);
  t_int_press_drop->SetValue(txt8);

  txt9<<(*p_ext_press_drop);
  t_ext_press_drop->SetValue(txt9);

  txt10<<(*p_fin_effect);
  t_fin_effect->SetValue(txt10);

  cb_arrangement->SetValue(p_arrangement->c_str());

  cb_tube_config->SetValue(p_tube_config->c_str());

  wxCommandEvent event;
  if (*p_use_fins)
    c_use_fins->SetValue(true);
  else
    c_use_fins->SetValue(false);

  OnFinsChange(event);
  return true;
}

void Recuperator_UI_Dialog::Lock(bool l)
{
}

///////////////////////////////////////////////////
void Recuperator_UI_Dialog::OnFinsChange(wxCommandEvent &event)
{
  if (c_use_fins->GetValue())
    t_fin_effect->Enable(true);
  else
    t_fin_effect->Enable(false);
    
}
