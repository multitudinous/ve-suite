#include "wgsr_UI.h"


IMPLEMENT_DYNAMIC_CLASS(WGSR_UI_Dialog, UIDialog);

WGSR_UI_Dialog::WGSR_UI_Dialog(wxWindow* parent, int id, double* co_conv_eff_d, double* steam_added_d)
  : UIDialog((wxWindow *) parent, id, "WGSR"),
    co_conv_eff_d_(co_conv_eff_d),
    steam_added_d_(steam_added_d)
{
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  wxSize sz(240, 20); // the size for the label
  top_sizer->Add(10, 10, 1);
  top_sizer->Add(first_row, 1);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(second_row, 1);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 1, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);

  wxStaticText * label1 = new wxStaticText(this, -1, "   CO Conversion Efficiency (%)", wxDefaultPosition, sz);
  co_conv_eff = new wxTextCtrl(this,  CO_CONV_EFF, wxT(" "), wxDefaultPosition, wxSize(80, 20));
  wxStaticText * label2 = new wxStaticText(this, -1, " ", wxDefaultPosition, wxSize(10, 20));

  first_row->Add(label1);
  first_row->Add(co_conv_eff);
  first_row->Add(label2);

  co_conv_eff->Enable(true);

  wxStaticText * label3 = new wxStaticText(this, -1, "   Steam Added (mH2O/mCO)", wxDefaultPosition, sz);
  steam_added = new wxTextCtrl(this,  STEAM_ADDED, wxT(" "), wxDefaultPosition, wxSize(80, 20));
  wxStaticText * label4 = new wxStaticText(this, -1, " ", wxDefaultPosition, wxSize(10, 20));
  
  second_row->Add(label3);
  second_row->Add(steam_added);
  second_row->Add(label4);

  steam_added->Enable(true);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  SetSizer(top_sizer);
  SetAutoLayout(TRUE);
  top_sizer->Fit(this);
  
}

WGSR_UI_Dialog::~WGSR_UI_Dialog()
{
}

bool WGSR_UI_Dialog::TransferDataFromWindow()
{
  wxString txt1, txt2;
  
  txt1=co_conv_eff->GetValue();
  txt2=steam_added->GetValue();

  *co_conv_eff_d_=atof(txt1.c_str());
  *steam_added_d_=atof(txt2.c_str());

  return true;
}

bool WGSR_UI_Dialog::TransferDataToWindow()  // function built into wx
{
  wxString txt1, txt2;
  
  txt1<<(*co_conv_eff_d_);
  txt2<<(*steam_added_d_);

  co_conv_eff->SetValue(txt1);
  steam_added->SetValue(txt2);
  
  return true;
}
