#include "Power_UI.h"
#include "wx/combobox.h"

IMPLEMENT_DYNAMIC_CLASS(POWER_UI_Dialog, UIDialog);

POWER_UI_Dialog::POWER_UI_Dialog(wxWindow* parent, int id, 
				 long *num_turbs, 
				 double *gts_idx_in_pres_ratio,
				 double *gts_idx_in_heat_rate,
				 double *gts_idx_in_sox_so3,
				 double *gts_idx_in_nox_cons,
				 double *gts_idx_in_nox_no,
				 double *gts_idx_in_carbon_co)
  : UIDialog((wxWindow *) parent, id, "Power Block"),
    num_gasturb_(num_turbs),
    gts_idx_in_pres_ratio_(gts_idx_in_pres_ratio),
    gts_idx_in_heat_rate_(gts_idx_in_heat_rate),
    gts_idx_in_sox_so3_(gts_idx_in_sox_so3),
    gts_idx_in_nox_cons_(gts_idx_in_nox_cons),
    gts_idx_in_nox_no_(gts_idx_in_nox_no),
    gts_idx_in_carbon_co_(gts_idx_in_carbon_co)
{
  wxBoxSizer* toptop = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);
  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  wxStaticBox* gas_turb_box = new wxStaticBox(this, -1, "Gas Turbine");
  wxStaticBoxSizer* gas_turb_row = new wxStaticBoxSizer(gas_turb_box, wxVERTICAL);
  wxStaticBox* steam_cyc_box = new wxStaticBox(this, -1, "Steam Cycle");
  wxStaticBoxSizer* steam_cyc_row = new wxStaticBoxSizer(steam_cyc_box, wxVERTICAL);
  wxStaticBox* emis_box = new wxStaticBox(this, -1, "Emis. Factors");
  wxStaticBoxSizer* emis_row = new wxStaticBoxSizer(emis_box, wxVERTICAL);

  wxBoxSizer* gas_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* gas_second_row = new wxBoxSizer(wxHORIZONTAL);
  //wxBoxSizer* gas_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* gas_forth_row = new wxBoxSizer(wxHORIZONTAL);
  
  wxBoxSizer* steam_first_row = new wxBoxSizer(wxHORIZONTAL);

  wxBoxSizer* emis_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* emis_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* emis_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* emis_forth_row = new wxBoxSizer(wxHORIZONTAL);

  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  wxSize sz(200, 20); // the size for the lable
  top_sizer->Add(10, 10, 0);
  top_sizer->Add(gas_turb_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(steam_cyc_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(emis_row, 0);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);

  gas_turb_row->Add(10, 5, 0);
  gas_turb_row->Add(gas_first_row, 0);
  gas_turb_row->Add(10, 3, 0);
  gas_turb_row->Add(gas_second_row, 0);
  gas_turb_row->Add(10, 3, 0);
  //  gas_turb_row->Add(gas_third_row, 0);
  //gas_turb_row->Add(10, 3, 0);
  gas_turb_row->Add(gas_forth_row, 0);
  gas_turb_row->Add(10, 3, 0);

  steam_cyc_row->Add(10, 5, 0);
  steam_cyc_row->Add(steam_first_row, 0);
  steam_cyc_row->Add(10, 3, 0);

  emis_row->Add(10, 5, 0);
  emis_row->Add(emis_first_row, 0);
  emis_row->Add(10, 3, 0);
  emis_row->Add(emis_second_row, 0);
  emis_row->Add(10, 3, 0);
  emis_row->Add(emis_third_row, 0);
  emis_row->Add(10, 3, 0);
  emis_row->Add(emis_forth_row, 0);
  emis_row->Add(10, 3, 0);


  wxString gas_turb_val[] = { wxT("GE 7FA") };
  wxString num_gasturb_val[] = { wxT("1"), wxT("2"), wxT("3"), wxT("4"), wxT("5")};
 
  wxStaticText * label0 = new wxStaticText(this, -1, "    Gas Turbine Model", wxDefaultPosition, sz);
  gas_turb_mod = new wxComboBox(this, GAS_TURB_MOD, wxT("GE 7FA"), wxDefaultPosition, wxSize(80, 20), 1, gas_turb_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);

  gas_turb_mod->Enable(false);

  gas_first_row->Add(label0);
  gas_first_row->Add(gas_turb_mod);
  gas_first_row->Add(80,20);
  wxStaticText * label1 = new wxStaticText(this, -1, "    No. of Gas Turbines", wxDefaultPosition, sz);
  num_gasturb = new wxComboBox(this,  NUM_GASTURB, wxT("1"), wxDefaultPosition, wxSize(80, 20), 5, num_gasturb_val, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  
  gas_second_row->Add(label1);
  gas_second_row->Add(num_gasturb);
  gas_second_row->Add(80,20);
  wxStaticText * label3 = new wxStaticText(this, -1, "    Pressure Ratio (outlet/inlet)", wxDefaultPosition, sz);
  pres_ratio = new wxTextCtrl(this, PRES_RATIO, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  
  //  pres_ratio->Enable(false);

  gas_forth_row->Add(label3);
  gas_forth_row->Add(pres_ratio);
  gas_forth_row->Add(80,20);
  wxStaticText * label4 = new wxStaticText(this, -1, "    Steam Cycle Heat Rate, HHV", wxDefaultPosition, sz);
  steam_cyc_heatrate = new wxTextCtrl(this, STEAM_CYC_HEATRATE, wxT("9000"), wxDefaultPosition, wxSize(80, 20));
  wxStaticText * label5 = new wxStaticText(this, -1, " Btu/kWh", wxDefaultPosition, wxSize(80,20));

  //  steam_cyc_heatrate->Enable(false);

  steam_first_row->Add(label4);
  steam_first_row->Add(steam_cyc_heatrate);
  steam_first_row->Add(label5);

  wxStaticText * label6 = new wxStaticText(this, -1, "    Percent SOx as SO3", wxDefaultPosition, sz);
  sox_so3 = new wxTextCtrl(this, SOX_SO3, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  //sox_so3->Enable(false);
  wxStaticText * label7 = new wxStaticText(this, -1, " vol %", wxDefaultPosition, wxSize(80,20));
  emis_first_row->Add(label6);
  emis_first_row->Add(sox_so3);
  emis_first_row->Add(label7);

  wxStaticText * label8 = new wxStaticText(this, -1, "    NOx Emission Concentration", wxDefaultPosition, sz);
  nox_cons = new wxTextCtrl(this, NOX_CONS, wxT("9.000"), wxDefaultPosition, wxSize(80, 20));
  //nox_cons->Enable(false);
  wxStaticText * label9 = new wxStaticText(this, -1, " ppmv", wxDefaultPosition, wxSize(80,20));
  emis_second_row->Add(label8);
  emis_second_row->Add(nox_cons);
  emis_second_row->Add(label9);

  wxStaticText * label10 = new wxStaticText(this, -1, "    Percent NOx as NO", wxDefaultPosition, sz);
  nox_no = new wxTextCtrl(this, NOX_NO, wxT("95.00"), wxDefaultPosition, wxSize(80, 20));
  //nox_no->Enable(false);
  wxStaticText * label11 = new wxStaticText(this, -1, " vol %", wxDefaultPosition, wxSize(80,20));
  emis_third_row->Add(label10);
  emis_third_row->Add(nox_no);
  emis_third_row->Add(label11);

  wxStaticText * label12 = new wxStaticText(this, -1, "    Percent Total Carbon as CO", wxDefaultPosition, sz);
  carbon_co = new wxTextCtrl(this, CARBON_CO, wxT("0.0"), wxDefaultPosition, wxSize(80, 20));
  //carbon_co->Enable(false);
  wxStaticText * label13 = new wxStaticText(this, -1, " vol %", wxDefaultPosition, wxSize(80,20));
  
  emis_forth_row->Add(label12);
  emis_forth_row->Add(carbon_co);
  emis_forth_row->Add(label13);


  
  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);
  
}

POWER_UI_Dialog::~POWER_UI_Dialog()
{
}

bool POWER_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;
  
  txt = num_gasturb->GetValue();
  (*num_gasturb_)=atoi(txt.c_str());
  if ((*num_gasturb_)>5 || (*num_gasturb_)<1)
    {
      wxMessageBox("Number of operating gas turbines should be between 1 and 5", "Error!");
      return false;
    }

  txt = pres_ratio->GetValue();
  (*gts_idx_in_pres_ratio_)=atof(txt.c_str());
  if ((*gts_idx_in_pres_ratio_)>25.00 || (*gts_idx_in_pres_ratio_)<1.0)
    {
      wxMessageBox("Pressure ratio should be between 1.0 and 25.00", "Error!");
      return false;
    }

  txt = steam_cyc_heatrate->GetValue();
  (*gts_idx_in_heat_rate_)=atof(txt.c_str());
  if ((*gts_idx_in_heat_rate_)>11000.0 || (*gts_idx_in_heat_rate_)<6000.0)
    {
      wxMessageBox(" Steam Cycle Heat Rate should be between 6000.0 and 11000.0", "Error!");
      return false;
    }

  txt = sox_so3->GetValue();
  (*gts_idx_in_sox_so3_)=atof(txt.c_str());
  if ((*gts_idx_in_sox_so3_)>10.0 || (*gts_idx_in_sox_so3_)<0.0)
    {
      wxMessageBox("  Percent SOx as SO3 should be between 0.0 and 10.0", "Error!");
      return false;
    }

  txt = nox_cons->GetValue();
  (*gts_idx_in_nox_cons_)=atof(txt.c_str());
  if ((*gts_idx_in_nox_cons_)>100.0 || (*gts_idx_in_nox_cons_)<0.0)
    {
      wxMessageBox("  Percent NOx Emission Concentration should be between 0.0 and 100.0", "Error!");
      return false;
    }

  txt = nox_no->GetValue();
  (*gts_idx_in_nox_no_)=atof(txt.c_str());
  if ((*gts_idx_in_nox_no_)>100.0 || (*gts_idx_in_nox_no_)<90.0)
    {
      wxMessageBox("  Percent NOx as NO should be between 90.0 and 100.0", "Error!");
      return false;
    }

  txt = carbon_co->GetValue();
  (*gts_idx_in_carbon_co_)=atof(txt.c_str());
  if ((*gts_idx_in_carbon_co_)>100.0 || (*gts_idx_in_carbon_co_)<0.0)
    {
      wxMessageBox("  Percent Total Carbon as CO should be between 0.0 and 100.0", "Error!");
      return false;
    }

  return true;
}

bool POWER_UI_Dialog::TransferDataToWindow()
{
  wxString txt1, txt2, txt3, txt4, txt5, txt6, txt7, txt8;
   
  txt1<<(*num_gasturb_);
  num_gasturb->SetValue(txt1);

  txt3<<(*gts_idx_in_pres_ratio_);
  pres_ratio->SetValue(txt3);

  txt4<<(*gts_idx_in_heat_rate_);
  steam_cyc_heatrate->SetValue(txt4);

  txt5<<(*gts_idx_in_sox_so3_);
  sox_so3->SetValue(txt5);

  txt6<<(*gts_idx_in_nox_cons_);
  nox_cons->SetValue(txt6);

  txt7<<(*gts_idx_in_nox_no_);
  nox_no->SetValue(txt7);

  txt8<<(*gts_idx_in_carbon_co_);
  carbon_co->SetValue(txt8);

  return true;
}
