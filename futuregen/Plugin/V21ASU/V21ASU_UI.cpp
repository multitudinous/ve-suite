#include "V21ASU_UI.h"

IMPLEMENT_DYNAMIC_CLASS(V21ASU_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
V21ASU_UI_Dialog
::V21ASU_UI_Dialog
(wxWindow* parent, int id,
  double* o2_temp,
  double* o2_pres,
  double* o2_purity,
  double* n2_temp,
  double* n2_pres)
: UIDialog((wxWindow *) parent, id, "V21ASU"),
  p_o2_temp(o2_temp),
  p_o2_pres(o2_pres),
  p_o2_purity(o2_purity),
  p_n2_temp(n2_temp),
  p_n2_pres(n2_pres)
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

  wxStaticBox *o2_box = new wxStaticBox(this, -1, "Oxygen Stream");
  wxStaticBox *n2_box = new wxStaticBox(this, -1, "Nitrogen Stream");
  
  wxStaticBoxSizer *o2_row = new wxStaticBoxSizer(o2_box, wxVERTICAL);
  wxStaticBoxSizer *n2_row = new wxStaticBoxSizer(n2_box, wxVERTICAL);
  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(o2_row, 0); 
  top_sizer->Add(10, 5, 0); 
  top_sizer->Add(n2_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin

  wxBoxSizer *o2_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *o2_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *o2_third_row = new wxBoxSizer(wxHORIZONTAL);
  
  o2_row->Add(10, 5, 0);
  o2_row->Add(o2_first_row, 0);
  //o2_row->Add(10, 3, 0);
  o2_row->Add(o2_second_row, 0);
  o2_row->Add(10, 3, 0);
  o2_row->Add(o2_third_row, 0);
  o2_row->Add(10, 3, 0);

  wxBoxSizer *n2_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *n2_second_row = new wxBoxSizer(wxHORIZONTAL);
  
  n2_row->Add(10, 5, 0);
  n2_row->Add(n2_first_row, 0);
  n2_row->Add(10, 5, 0);
  n2_row->Add(n2_second_row, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);

  wxStaticText * label0 = new wxStaticText(this, -1, " Temperature (K)", wxDefaultPosition, wxSize(150, 17));
  t_o2_temp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  o2_first_row->Add(label0);
  o2_first_row->Add(t_o2_temp);

  //wxStaticText * label1 = new wxStaticText(this, -1, " Pressure (Pa)", wxDefaultPosition, wxSize(150, 17));
  //t_o2_pres = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  //o2_second_row->Add(label1);
  //o2_second_row->Add(t_o2_pres);

  wxStaticText * label2 = new wxStaticText(this, -1, " Purity (%)", wxDefaultPosition, wxSize(150, 17));
  t_o2_purity = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  o2_third_row->Add(label2);
  o2_third_row->Add(t_o2_purity);

  wxStaticText * label3 = new wxStaticText(this, -1, " Temperature (K)", wxDefaultPosition, wxSize(150, 17));
  t_n2_temp = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  n2_first_row->Add(label3);
  n2_first_row->Add(t_n2_temp);

  //wxStaticText * label4 = new wxStaticText(this, -1, " Pressure (K)", wxDefaultPosition, wxSize(150, 17));
  //t_n2_pres = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  //n2_second_row->Add(label4);
  //n2_second_row->Add(t_n2_pres);
  
  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);
  
}

/////////////////////////////////////////////////////
V21ASU_UI_Dialog
::~V21ASU_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool V21ASU_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_o2_temp->GetValue();
  (*p_o2_temp) = atof(txt.c_str());
  
  //txt = t_o2_pres->GetValue();
  //(*p_o2_pres) = atof(txt.c_str());

  txt = t_o2_purity->GetValue();
  (*p_o2_purity) = atof(txt.c_str());
  
  txt = t_n2_temp->GetValue();
  (*p_n2_temp) = atof(txt.c_str());
  
  //txt = t_n2_pres->GetValue();
  //(*p_n2_pres) = atof(txt.c_str());
  
  return true;
}

////////////////////////////////////////////////////
bool V21ASU_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5;
   
  txt1<<(*p_o2_temp);
  t_o2_temp->SetValue(txt1);

  //txt2<<(*p_o2_pres);
  //t_o2_pres->SetValue(txt2);

  txt3<<(*p_o2_purity);
  t_o2_purity->SetValue(txt3);

  txt4<<(*p_n2_temp);
  t_n2_temp->SetValue(txt4);

  //txt5<<(*p_n2_pres);
  //t_n2_pres->SetValue(txt5);
  
    return true;
}

void V21ASU_UI_Dialog::Lock(bool l)
{
}

