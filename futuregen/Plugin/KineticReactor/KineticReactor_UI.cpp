#include "KineticReactor_UI.h"

BEGIN_EVENT_TABLE(KineticReactor_UI_Dialog, UIDialog)
  EVT_RADIOBOX(R_CASETYPE, KineticReactor_UI_Dialog::OnCaseTypeChange)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(KineticReactor_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
KineticReactor_UI_Dialog
::KineticReactor_UI_Dialog
(wxWindow* parent, int id,
  double* res_time,
  double* qloss,
  double* quench_rate,
  string* work_dir,
  long* case_type)
: UIDialog((wxWindow *) parent, id, "KineticReactor"),
  p_res_time(res_time),
  p_qloss(qloss),
  p_quench_rate(quench_rate),
  p_work_dir(work_dir),
  p_case_type(case_type)
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

  top_sizer->Add(10, 5, 0); //the top margin
  top_sizer->Add(data_row, 0); 
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin
  
  wxBoxSizer *data_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_forth_row = new wxBoxSizer(wxHORIZONTAL);

  //data_row->Add(10, 5, 0);
  data_row->Add(data_first_row, 0);
  data_row->Add(10, 6, 0);
  data_row->Add(data_second_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_third_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_forth_row, 0);
  data_row->Add(10, 3, 0);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  wxString temp[]={_T("PFR CONP"), _T("PFR TTIM"), _T("PSR")};
  r_case_type = new wxRadioBox(this, R_CASETYPE, _T(" Case Type "), wxDefaultPosition, wxSize(280, 45), 3, temp);
  data_first_row->Add(r_case_type);

  wxStaticText * label1 = new wxStaticText(this, -1, " Residence Time (s) ", wxDefaultPosition, wxSize(200, 17));
  t_res_time = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label1);
  data_second_row->Add(t_res_time);

  wxStaticText * label2 = new wxStaticText(this, -1, " Desired Conversion ", wxDefaultPosition, wxSize(200, 17));
  t_quench_rate = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label2);
  data_third_row->Add(t_quench_rate);

  wxStaticText * label3 = new wxStaticText(this, -1, " Fraction Heatloss ", wxDefaultPosition, wxSize(200, 17));
  t_qloss = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_forth_row->Add(label3);
  data_forth_row->Add(t_qloss);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);    
}

/////////////////////////////////////////////////////
KineticReactor_UI_Dialog
::~KineticReactor_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool KineticReactor_UI_Dialog::TransferDataFromWindow()
{  
  wxString txt;
  
  txt = t_res_time->GetValue();
  (*p_res_time) = atof(txt.c_str());
  
  txt = t_qloss->GetValue();
  (*p_qloss) = atof(txt.c_str());

  txt = t_quench_rate->GetValue();
  (*p_quench_rate) = atof(txt.c_str());


  *p_case_type = r_case_type->GetSelection();
  
  return true;
}

////////////////////////////////////////////////////
bool KineticReactor_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3;

  txt1<<(*p_res_time);
  t_res_time->SetValue(txt1);

  txt2<<(*p_qloss);
  t_qloss->SetValue(txt2);

  txt3<<(*p_quench_rate);
  t_quench_rate->SetValue(txt3);

  wxCommandEvent event;
  r_case_type->SetSelection(*p_case_type);

  OnCaseTypeChange(event);
  return true;
}

void KineticReactor_UI_Dialog::Lock(bool l)
{
  
}

void KineticReactor_UI_Dialog::OnCaseTypeChange(wxCommandEvent &event)
{
  int sel;
  sel = r_case_type->GetSelection();

  switch (sel)
    {
    case 0:
      t_res_time->Enable(true);
      t_quench_rate->Enable(false);
      t_qloss->Enable(false);
      break;
    case 1:
      t_res_time->Enable(false);
      t_quench_rate->Enable(true);
      t_qloss->Enable(false);
      break;
    case 2:
      t_res_time->Enable(false);
      t_quench_rate->Enable(false);
      t_qloss->Enable(true);
      break;
    }
}
