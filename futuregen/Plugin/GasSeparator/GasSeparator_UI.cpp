#include "GasSeparator_UI.h"
#include "thermo.h"

IMPLEMENT_DYNAMIC_CLASS(GasSeparator_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
GasSeparator_UI_Dialog
::GasSeparator_UI_Dialog
(wxWindow* parent, int id,
  double* purity,
  double* remain,
  string* specie)
: UIDialog((wxWindow *) parent, id, "GasSeparator"),
  p_purity(purity),
  p_remain(remain),
  p_specie(specie)
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

  wxStaticText * label0 = new wxStaticText(this, -1, " Specie : ", wxDefaultPosition, wxSize(140, 17));

  cb_specie=new wxComboBox(this, -1, wxT(""), wxDefaultPosition, wxSize(80, 20), 0, NULL, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);

  //Now try to read the thermo file to get the species list
  // Available species
  thermo *thrmo;
  std::string therm_path = "therm";
  thrmo = new thermo(therm_path);
    
  const std::map<std::string, int>& name_map = thrmo->get_nam_spec();
  
  map<std::string, int>::const_iterator iter;
  for(iter=name_map.begin(); iter!=name_map.end(); iter++)
    cb_specie->Append((iter->first).c_str());
  
  cb_specie->SetSelection(0);
  
  data_first_row->Add(label0);
  data_first_row->Add(cb_specie);

  wxStaticText * label1 = new wxStaticText(this, -1, " Purity (%) ", wxDefaultPosition, wxSize(140, 17));
  t_purity = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));

  data_second_row->Add(label1);
  data_second_row->Add(t_purity);

  wxStaticText * label2 = new wxStaticText(this, -1, " Remaining (%) ", wxDefaultPosition, wxSize(140, 17));
  t_remain = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_remain);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  
  delete thrmo;
  
}

/////////////////////////////////////////////////////
GasSeparator_UI_Dialog
::~GasSeparator_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool GasSeparator_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = cb_specie->GetValue();
  (*p_specie) = txt.c_str();
  
  txt = t_purity->GetValue();
  (*p_purity) = atof(txt.c_str());

  txt = t_remain->GetValue();
  (*p_remain) = atof(txt.c_str());
  
  return true;
}

////////////////////////////////////////////////////
bool GasSeparator_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2;

  txt1<<(*p_purity);
  t_purity->SetValue(txt1);

  txt2<<(*p_remain);
  t_remain->SetValue(txt2);

  if ((*p_specie)!="")
    cb_specie->SetValue(p_specie->c_str());

  return true;
}

void GasSeparator_UI_Dialog::Lock(bool l)
{
}

