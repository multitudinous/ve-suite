#include "SOFC_UI.h"

IMPLEMENT_DYNAMIC_CLASS(SOFC_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
SOFC_UI_Dialog
::SOFC_UI_Dialog
(wxWindow* parent, int id,
  double* a_thickness,
  double* c_thickness,
  double* e_thickness,
  double* a_A,
  double* a_E,
  double* c_A,
  double* c_E,
  double* e_A,
  double* e_E,
  double* cell_area,
  double* num_cell,
  double* fuel_util,
  double* press_drop)
: UIDialog((wxWindow *) parent, id, "SOFC"),
  p_a_thickness(a_thickness),
  p_c_thickness(c_thickness),
  p_e_thickness(e_thickness),
  p_a_A(a_A),
  p_a_E(a_E),
  p_c_A(c_A),
  p_c_E(c_E),
  p_e_A(e_A),
  p_e_E(e_E),
  p_cell_area(cell_area),
  p_num_cell(num_cell),
  p_fuel_util(fuel_util),
  p_press_drop(press_drop)
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

  wxStaticBox *gui1_box = new wxStaticBox(this, -1, "Construction");
  wxStaticBox *gui2_box = new wxStaticBox(this, -1, "Cell Resistance: R = Ae^(E/t)");
  wxStaticBox *gui3_box = new wxStaticBox(this, -1, "Cells");
  
  wxStaticBoxSizer *data1_row = new wxStaticBoxSizer(gui1_box, wxVERTICAL);
  wxStaticBoxSizer *data2_row = new wxStaticBoxSizer(gui2_box, wxVERTICAL);
  wxStaticBoxSizer *data3_row = new wxStaticBoxSizer(gui3_box, wxVERTICAL);
  wxBoxSizer* data4_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data1_row, 0); 
  top_sizer->Add(10, 5, 0); 
  top_sizer->Add(data2_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(data3_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(data4_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin

  wxBoxSizer *data1_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data1_forth_row = new wxBoxSizer(wxHORIZONTAL);
  
  data1_row->Add(10, 5, 0);
  data1_row->Add(data1_first_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_second_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_third_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data1_forth_row, 0);
  data1_row->Add(10, 3, 0);

  wxBoxSizer *data2_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data2_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data2_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data2_forth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data2_fifth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data2_sixth_row = new wxBoxSizer(wxHORIZONTAL);
  
  data2_row->Add(10, 5, 0);
  data2_row->Add(data2_first_row, 0);
  data2_row->Add(10, 3, 0);
  data2_row->Add(data2_second_row, 0);
  data2_row->Add(10, 3, 0);
  data2_row->Add(data2_third_row, 0);
  data2_row->Add(10, 3, 0);
  data2_row->Add(data2_forth_row, 0);
  data2_row->Add(10, 3, 0);
  data2_row->Add(data2_fifth_row, 0);
  data2_row->Add(10, 3, 0);
  data2_row->Add(data2_sixth_row, 0);
  data2_row->Add(10, 3, 0);

  wxBoxSizer *data3_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data3_second_row = new wxBoxSizer(wxHORIZONTAL);
  
  data3_row->Add(10, 5, 0);
  data3_row->Add(data3_first_row, 0);
  data3_row->Add(10, 3, 0);
  data3_row->Add(data3_second_row, 0);
  data3_row->Add(10, 3, 0);

  wxStaticText * label0 = new wxStaticText(this, -1, " Fuel Utilization (%) ", wxDefaultPosition, wxSize(200, 17));
  t_fuel_util = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data4_row->Add(label0);
  data4_row->Add(t_fuel_util);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);

  wxStaticText * label1 = new wxStaticText(this, -1, " Anode Thickness (cm)", wxDefaultPosition, wxSize(200, 17));
  t_a_thickness = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_first_row->Add(label1);
  data1_first_row->Add(t_a_thickness);

  wxStaticText * label2 = new wxStaticText(this, -1, " Cathode Thickness (cm)", wxDefaultPosition, wxSize(200, 17));
  t_c_thickness = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_second_row->Add(label2);
  data1_second_row->Add(t_c_thickness);

  wxStaticText * label3 = new wxStaticText(this, -1, " Electrolyte Thickness (cm)", wxDefaultPosition, wxSize(200, 17));
  t_e_thickness = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_third_row->Add(label3);
  data1_third_row->Add(t_e_thickness);

  wxStaticText * label4 = new wxStaticText(this, -1, " Fuel Cell Pressure Drop (Pa)", wxDefaultPosition, wxSize(200, 17));
  t_press_drop = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data1_forth_row->Add(label4);
  data1_forth_row->Add(t_press_drop);

  wxStaticText * label5 = new wxStaticText(this, -1, " Anode, A", wxDefaultPosition, wxSize(200, 17));
  t_a_A = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data2_first_row->Add(label5);
  data2_first_row->Add(t_a_A);

  wxStaticText * label6 = new wxStaticText(this, -1, " Anode, E", wxDefaultPosition, wxSize(200, 17));
  t_a_E = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data2_second_row->Add(label6);
  data2_second_row->Add(t_a_E);

  wxStaticText * label7 = new wxStaticText(this, -1, " Cathode, A", wxDefaultPosition, wxSize(200, 17));
  t_c_A = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data2_third_row->Add(label7);
  data2_third_row->Add(t_c_A);

  wxStaticText * label8 = new wxStaticText(this, -1, " Cathode, E", wxDefaultPosition, wxSize(200, 17));
  t_c_E = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data2_forth_row->Add(label8);
  data2_forth_row->Add(t_c_E);

  wxStaticText * label9 = new wxStaticText(this, -1, " Electrolyte, A", wxDefaultPosition, wxSize(200, 17));
  t_e_A = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data2_fifth_row->Add(label9);
  data2_fifth_row->Add(t_e_A);

  wxStaticText * label10 = new wxStaticText(this, -1, " Electrolyte, E", wxDefaultPosition, wxSize(200, 17));
  t_e_E = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data2_sixth_row->Add(label10);
  data2_sixth_row->Add(t_e_E);

  wxStaticText * label11 = new wxStaticText(this, -1, " Area per unit cell (cm^2)", wxDefaultPosition, wxSize(200, 17));
  t_cell_area = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data3_first_row->Add(label11);
  data3_first_row->Add(t_cell_area);

  wxStaticText * label12 = new wxStaticText(this, -1, " Number of cells", wxDefaultPosition, wxSize(200, 17));
  t_num_cell = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data3_second_row->Add(label12);
  data3_second_row->Add(t_num_cell);
  
  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  
}

/////////////////////////////////////////////////////
SOFC_UI_Dialog
::~SOFC_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool SOFC_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_a_thickness->GetValue();
  (*p_a_thickness) = atof(txt.c_str());
  
  txt = t_c_thickness->GetValue();
  (*p_c_thickness) = atof(txt.c_str());

  txt = t_e_thickness->GetValue();
  (*p_e_thickness) = atof(txt.c_str());
  
  txt = t_a_A->GetValue();
  (*p_a_A) = atof(txt.c_str());
  
  txt = t_a_E->GetValue();
  (*p_a_E) = atof(txt.c_str());
  
  txt = t_c_A->GetValue();
  (*p_c_A) = atof(txt.c_str());
  
  txt = t_c_E->GetValue();
  (*p_c_E) = atof(txt.c_str());
  
  txt = t_e_A->GetValue();
  (*p_e_A) = atof(txt.c_str());
  
  txt = t_e_E->GetValue();
  (*p_e_E) = atof(txt.c_str());
  
  txt = t_cell_area->GetValue();
  (*p_cell_area) = atof(txt.c_str());
  
  txt = t_num_cell->GetValue();
  (*p_num_cell) = atof(txt.c_str());
  
  txt = t_fuel_util->GetValue();
  (*p_fuel_util) = atof(txt.c_str());
  
  txt = t_press_drop->GetValue();
  (*p_press_drop) = atof(txt.c_str());

  return true;
}

////////////////////////////////////////////////////
bool SOFC_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt8, txt9,
    txt10, txt11, txt12, txt13;
   
  txt1<<(*p_a_thickness);
  t_a_thickness->SetValue(txt1);

  txt2<<(*p_c_thickness);
  t_c_thickness->SetValue(txt2);

  txt3<<(*p_e_thickness);
  t_e_thickness->SetValue(txt3);

  txt4<<(*p_a_A);
  t_a_A->SetValue(txt4);

  txt5<<(*p_a_E);
  t_a_E->SetValue(txt5);

  txt6<<(*p_c_A);
  t_c_A->SetValue(txt6);

  txt7<<(*p_c_E);
  t_c_E->SetValue(txt7);

  txt8<<(*p_e_A);
  t_e_A->SetValue(txt8);

  txt9<<(*p_e_E);
  t_e_E->SetValue(txt9);

  txt10<<(*p_cell_area);
  t_cell_area->SetValue(txt10);

  txt11<<(*p_num_cell);
  t_num_cell->SetValue(txt11);

  txt12<<(*p_fuel_util);
  t_fuel_util->SetValue(txt12);

  txt13<<(*p_press_drop);
  t_press_drop->SetValue(txt13);
  
  return true;
}

void SOFC_UI_Dialog::Lock(bool l)
{
}

