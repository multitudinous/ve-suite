#include "Condenser_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Condenser_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
Condenser_UI_Dialog
::Condenser_UI_Dialog
(wxWindow* parent, int id,
  double* tube_id,
  double* tube_od,
  double* tube_length,
  double* int_press_drop,
  double* ext_press_drop,
  long* num_tubeH,
  long* num_tubeV)
: UIDialog((wxWindow *) parent, id, "Condenser"),
  p_tube_id(tube_id),
  p_tube_od(tube_od),
  p_tube_length(tube_length),
  p_int_press_drop(int_press_drop),
  p_ext_press_drop(ext_press_drop),
  p_num_tubeH(num_tubeH),
  p_num_tubeV(num_tubeV)
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
  
  wxStaticText * label0 = new wxStaticText(this, -1, " Number of Tubes (Horiz) ", wxDefaultPosition, wxSize(200, 17));
  t_num_tubeH = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_first_row->Add(label0);
  data_first_row->Add(t_num_tubeH);

  wxStaticText * label1 = new wxStaticText(this, -1, " Number of Tubes (Vert) ", wxDefaultPosition, wxSize(200, 17));
  t_num_tubeV = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_num_tubeV);

  wxStaticText * label2 = new wxStaticText(this, -1, " Tube Inner Diameter (m) ", wxDefaultPosition, wxSize(200, 17));
  t_tube_id = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_tube_id);

  wxStaticText * label3 = new wxStaticText(this, -1, " Tube Outer Diameter (m) ", wxDefaultPosition, wxSize(200, 17));
  t_tube_od = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_tube_od);

  wxStaticText * label4 = new wxStaticText(this, -1, " Tube Length (m) ", wxDefaultPosition, wxSize(200, 17));
  t_tube_length = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_fifth_row->Add(label4);
  data_fifth_row->Add(t_tube_length);

  wxStaticText * label5 = new wxStaticText(this, -1, " Internal Pressure Drop (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_int_press_drop = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_sixth_row->Add(label5);
  data_sixth_row->Add(t_int_press_drop);

  wxStaticText * label6 = new wxStaticText(this, -1, " External Pressure Drop (Pa) ", wxDefaultPosition, wxSize(200, 17));
  t_ext_press_drop = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_seventh_row->Add(label6);
  data_seventh_row->Add(t_ext_press_drop);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);   
}

/////////////////////////////////////////////////////
Condenser_UI_Dialog
::~Condenser_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Condenser_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
    
  txt = t_num_tubeH->GetValue();
  (*p_num_tubeH) = atof(txt.c_str());
  
  txt = t_num_tubeV->GetValue();
  (*p_num_tubeV) = atof(txt.c_str());

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

  return true;
}

////////////////////////////////////////////////////
bool Condenser_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6, txt7;
   
  txt1<<(*p_num_tubeH);
  t_num_tubeH->SetValue(txt1);

  txt2<<(*p_num_tubeV);
  t_num_tubeV->SetValue(txt2);

  txt3<<(*p_tube_id);
  t_tube_id->SetValue(txt3);

  txt4<<(*p_tube_od);
  t_tube_od->SetValue(txt4);

  txt5<<(*p_tube_length);
  t_tube_length->SetValue(txt5);

  txt6<<(*p_int_press_drop);
  t_int_press_drop->SetValue(txt6);

  txt7<<(*p_ext_press_drop);
  t_ext_press_drop->SetValue(txt7);

  return true;
}

void Condenser_UI_Dialog::Lock(bool l)
{
}

