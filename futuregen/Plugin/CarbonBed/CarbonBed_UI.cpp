#include "CarbonBed_UI.h"

IMPLEMENT_DYNAMIC_CLASS(CarbonBed_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
CarbonBed_UI_Dialog
::CarbonBed_UI_Dialog
(wxWindow* parent, int id,
  double* press_drop,
  double* part_diam,
  double* bulk_density,
  double* temp,
  double* press,
  double* cr_time,
  double* porosity,
  double* res_time,
  long* carbon_type)
: UIDialog((wxWindow *) parent, id, "CarbonBed"),
  p_press_drop(press_drop),
  p_part_diam(part_diam),
  p_bulk_density(bulk_density),
  p_temp(temp),
  p_press(press),
  p_cr_time(cr_time),
  p_porosity(porosity),
  p_res_time(res_time),
  p_carbon_type(carbon_type)
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
  wxStaticBox *data1_box = new wxStaticBox(this, -1, "Parameters");
  wxStaticBoxSizer* data1_row = new wxStaticBoxSizer(data1_box, wxVERTICAL);

  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data_row, 0); 
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(data1_row, 0);
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
 
  data_row->Add(10, 5, 0);
  data_row->Add(data_first_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_second_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data_third_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data_forth_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data_fifth_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data_sixth_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data_seventh_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data_eighth_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data_ninth_row, 0);
  data1_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  //wxStaticText * label0 = new wxStaticText(this, -1, " Carbon Type ", wxDefaultPosition, wxSize(100, 17));
  //wxString carbon_val[] = {wxT("Bituminous Derived Carbon"), wxT("Lignite Derived Carbon")};

  //cb_carbon_type = new wxComboBox(this, -1, wxT("Bituminous Derived Carbon"), wxDefaultPosition, wxSize(180, 20), 2, carbon_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  //data_first_row->Add(label0);
  //data_first_row->Add(cb_carbon_type);

  wxStaticText * label1 = new wxStaticText(this, -1, "Allowable Pressure Drop (psi/ft)", wxDefaultPosition, wxSize(200, 17));
  t_press_drop = new wxTextCtrl(this, -1, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_press_drop);

  wxStaticText * label2 = new wxStaticText(this, -1, "Carbon Particle Diameter (mm)", wxDefaultPosition, wxSize(200, 17));
  t_part_diam = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_part_diam);

  wxStaticText * label3 = new wxStaticText(this, -1, "Bulk Density (kg/m^3) ", wxDefaultPosition, wxSize(200, 17));
  t_bulk_density = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_bulk_density);

  wxStaticText * label4 = new wxStaticText(this, -1, "Operating Temperature (K) ", wxDefaultPosition, wxSize(200, 17));
  t_temp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_fifth_row->Add(label4);
  data_fifth_row->Add(t_temp);

  wxStaticText * label5 = new wxStaticText(this, -1, "Operating Pressure (psi) ", wxDefaultPosition, wxSize(200, 17));
  t_press = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_sixth_row->Add(label5);
  data_sixth_row->Add(t_press);

  wxStaticText * label6 = new wxStaticText(this, -1, "Carbon Replacement Time (years) ", wxDefaultPosition, wxSize(200, 17));
  t_cr_time = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_seventh_row->Add(label6);
  data_seventh_row->Add(t_cr_time);

  wxStaticText * label7 = new wxStaticText(this, -1, "Carbon Bed Porosity ", wxDefaultPosition, wxSize(200, 17));
  t_porosity = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_eighth_row->Add(label7);
  data_eighth_row->Add(t_porosity);

  wxStaticText * label8 = new wxStaticText(this, -1, "Absorber Residence Time (sec) ", wxDefaultPosition, wxSize(200, 17));
  t_res_time = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_ninth_row->Add(label8);
  data_ninth_row->Add(t_res_time);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);    
}

/////////////////////////////////////////////////////
CarbonBed_UI_Dialog
::~CarbonBed_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool CarbonBed_UI_Dialog::TransferDataFromWindow()
{

  wxString txt;
  
  txt = t_press_drop->GetValue();
  (*p_press_drop) = atof(txt.c_str());
  
  txt = t_part_diam->GetValue();
  (*p_part_diam) = atof(txt.c_str());

  txt = t_bulk_density->GetValue();
  (*p_bulk_density) = atof(txt.c_str());

  txt = t_temp->GetValue();
  (*p_temp) = atof(txt.c_str());

  txt = t_press->GetValue();
  (*p_press) = atof(txt.c_str());

  txt = t_cr_time->GetValue();
  (*p_cr_time) = atof(txt.c_str());

  txt = t_porosity->GetValue();
  (*p_porosity) = atof(txt.c_str());

  txt = t_res_time->GetValue();
  (*p_res_time) = atof(txt.c_str());


  //(*p_carbon_type) = cb_carbon_type->GetSelection();
  return true;
}

////////////////////////////////////////////////////
bool CarbonBed_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt8;
  
  txt1<<(*p_press_drop);
  t_press_drop->SetValue(txt1);

  txt2<<(*p_part_diam);
  t_part_diam->SetValue(txt2);
  
  txt3<<(*p_bulk_density);
  t_bulk_density->SetValue(txt3);
  
  txt4<<(*p_temp);
  t_temp->SetValue(txt4);
  
  txt5<<(*p_press);
  t_press->SetValue(txt5);
  
  txt6<<(*p_cr_time);
  t_cr_time->SetValue(txt6);
  
  txt7<<(*p_porosity);
  t_porosity->SetValue(txt7);
  
  txt8<<(*p_res_time);
  t_res_time->SetValue(txt8);
    
  //cb_carbon_type->SetSelection(*p_carbon_type);
  
  return true;
}

void CarbonBed_UI_Dialog::Lock(bool l)
{
}

