#include "AGR_UI.h"

IMPLEMENT_DYNAMIC_CLASS(AGR_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
AGR_UI_Dialog
::AGR_UI_Dialog
(wxWindow* parent, int id,
  double* solv_mw,
  double* solv_den,
  long* solv_type,
  long* tray_type)
: UIDialog((wxWindow *) parent, id, "AGR"),
  p_solv_mw(solv_mw),
  p_solv_den(solv_den),
  p_solv_type(solv_type),
  p_tray_type(tray_type)
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
  wxStaticBox *data1_box = new wxStaticBox(this, -1, "Solvent");
  wxStaticBoxSizer* data1_row = new wxStaticBoxSizer(data1_box, wxVERTICAL);

  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  //top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data_row, 0); 
  //top_sizer->Add(10, 5, 0);
  top_sizer->Add(data1_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin
  
  //wxBoxSizer *data_first_row = new wxBoxSizer(wxHORIZONTAL);
  //wxBoxSizer *data_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_row = new wxBoxSizer(wxHORIZONTAL);
 
  //data_row->Add(10, 5, 0);
  //data_row->Add(data_first_row, 0);
  //data_row->Add(10, 3, 0);
  //data_row->Add(data_second_row, 0);
  //data1_row->Add(10, 3, 0);
  data1_row->Add(data_third_row, 0);
  data1_row->Add(10, 3, 0);
  data1_row->Add(data_forth_row, 0);
  data1_row->Add(10, 3, 0);
 
  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  //wxStaticText * label0 = new wxStaticText(this, -1, " Tray Type ", wxDefaultPosition, wxSize(140, 17));
  //wxString tray_val[] = {wxT("Valv"), wxT("Bubble"), wxT("Sieve")};

  //cb_tray_type = new wxComboBox(this, -1, wxT("Valv"), wxDefaultPosition, wxSize(140, 20), 3, tray_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  //data_first_row->Add(label0);
  //data_first_row->Add(cb_tray_type);

  //wxStaticText * label1 = new wxStaticText(this, -1, " Solvent Type", wxDefaultPosition, wxSize(140, 17));
  //wxString solv_val[] = {wxT("MDEA"), wxT("Selexol"), wxT("Rectisol")};
  //cb_solv_type = new wxComboBox(this, -1, wxT("MDEA"), wxDefaultPosition, wxSize(140, 20), 3, solv_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  //data_second_row->Add(label1);
  //data_second_row->Add(cb_solv_type);

  wxStaticText * label2 = new wxStaticText(this, -1, " Molecular Weight (kg/kmol) ", wxDefaultPosition, wxSize(200, 17));
  t_solv_mw = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_solv_mw);

  wxStaticText * label3 = new wxStaticText(this, -1, " Density (kg/m^3) ", wxDefaultPosition, wxSize(200, 17));
  t_solv_den = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_solv_den);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);    
}

/////////////////////////////////////////////////////
AGR_UI_Dialog
::~AGR_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool AGR_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = t_solv_mw->GetValue();
  (*p_solv_mw) = atof(txt.c_str());
  
  txt = t_solv_den->GetValue();
  (*p_solv_den) = atof(txt.c_str());

  //(*p_solv_type) = cb_solv_type->GetSelection();
  //(*p_tray_type) = cb_tray_type->GetSelection();

  return true;
}

////////////////////////////////////////////////////
bool AGR_UI_Dialog::TransferDataToWindow()
{
  
  wxString txt1, txt2;
  
  txt1<<(*p_solv_mw);
  t_solv_mw->SetValue(txt1);

  txt2<<(*p_solv_den);
  t_solv_den->SetValue(txt2);
  
  //cb_solv_type->SetSelection(*p_solv_type);

  //cb_tray_type->SetSelection(*p_tray_type);
  
    return true;
}

void AGR_UI_Dialog::Lock(bool l)
{
}

