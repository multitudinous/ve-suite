
#include "SRS_UI.h"
#include "wx/combobox.h"

IMPLEMENT_DYNAMIC_CLASS(SRS_UI_Dialog, UIDialog);

SRS_UI_Dialog::SRS_UI_Dialog(wxWindow* parent, int id, double* tail_gas_sre_d, double *h2s_rmv_eff_d, double *sulf_rcv_eff_d)
  : UIDialog((wxWindow *) parent, id, "SRS"),
    tail_gas_sre_d_(tail_gas_sre_d),
    h2s_rmv_eff_d_(h2s_rmv_eff_d),
    sulf_rcv_eff_d_(sulf_rcv_eff_d)
{
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  wxSize sz(280, 20); // the size for the label
  top_sizer->Add(10, 10, 1);
  top_sizer->Add(first_row, 1);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(second_row, 1);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(third_row, 1);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 1, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);

  wxStaticText * label1 = new wxStaticText(this, -1, "    SRS H2S Removal Efficiency", wxDefaultPosition, sz);
  h2s_rmv_eff = new wxTextCtrl(this,  H2S_RMV_EFF, wxT(" "), wxDefaultPosition, wxSize(80, 20));
  wxStaticText * label2 = new wxStaticText(this, -1, " %", wxDefaultPosition, wxSize(25, 20));

  first_row->Add(label1);
  first_row->Add(h2s_rmv_eff);
  first_row->Add(label2);

  h2s_rmv_eff->Enable(true);

  wxStaticText * label3 = new wxStaticText(this, -1, "    Claus Plant Sulfur Recovery Efficiency", wxDefaultPosition, sz);
  sulf_rcv_eff_cp = new wxTextCtrl(this,  SULF_RCV_EFF_CP, wxT(" "), wxDefaultPosition, wxSize(80, 20));
  wxStaticText * label4 = new wxStaticText(this, -1, " %", wxDefaultPosition, wxSize(25, 20));
  
  second_row->Add(label3);
  second_row->Add(sulf_rcv_eff_cp);
  second_row->Add(label4);

  sulf_rcv_eff_cp->Enable(true);

  wxStaticText * label5 = new wxStaticText(this, -1, "    Tail Gas Treatment Sulfur Recovery Eff.", wxDefaultPosition, sz);
  tail_gas_sre = new wxTextCtrl(this,  TAIL_GAS_SRE, wxT(" "), wxDefaultPosition, wxSize(80, 20));
  wxStaticText * label6 = new wxStaticText(this, -1, " %", wxDefaultPosition, wxSize(40, 20));
  
  third_row->Add(label5);
  third_row->Add(tail_gas_sre);
  third_row->Add(label6);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  SetSizer(top_sizer);
  SetAutoLayout(TRUE);
  top_sizer->Fit(this);
  
}

SRS_UI_Dialog::~SRS_UI_Dialog()
{
}

bool SRS_UI_Dialog::TransferDataFromWindow()
{
  wxString txt1;
  
  txt1=tail_gas_sre->GetValue();
  (*tail_gas_sre_d_)=atof(txt1.c_str());
  
  if((*tail_gas_sre_d_)>100.0 || (*tail_gas_sre_d_)<0.0)
    {
      wxMessageBox("Tail Gas Treatment SRE should be between 0 and 100", "Error!");
      return false;
    }
  
  txt1=h2s_rmv_eff->GetValue();
  (*h2s_rmv_eff_d_) = atof(txt1.c_str());

  if((*h2s_rmv_eff_d_)>100 || (*h2s_rmv_eff_d_)<0)
    {
      wxMessageBox("Efficiency should be between 0 and 100", "Error!");
      return false;
    }

  txt1=sulf_rcv_eff_cp->GetValue();
  (*sulf_rcv_eff_d_) = atof(txt1.c_str());

  if((*sulf_rcv_eff_d_)>100 || (*sulf_rcv_eff_d_)<0)
    {
      wxMessageBox("Efficiency should be between 0 and 100", "Error!");
      return false;
    }
  
  
  return true;
}

bool SRS_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3;
  
  txt1<<(*tail_gas_sre_d_);
  tail_gas_sre->SetValue(txt1);
  txt2<<(*h2s_rmv_eff_d_);
  h2s_rmv_eff->SetValue(txt2);
  txt3<<(*sulf_rcv_eff_d_);
  sulf_rcv_eff_cp->SetValue(txt3);
  
  return true;
}
