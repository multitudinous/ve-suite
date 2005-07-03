#ifndef RESULT_PANEL_H
#define RESULT_PANEL_H
#include <wx/wx.h>

enum 
{
  MW_GROSS,
  MW_NET,
  NET_EFF,
  COAL_IN,
  WATER_IN, 
  OXID_IN,
  NOX_CONS,
  CO2_IN,
  CO2_OUT,
  CO2_CAP,
  CAPITAL_CST,
  ELEC_CST,
};

class ResultPanel_Dialog : public wxDialog
{
  //DECLARE_DYNAMIC_CLASS(ResultPanel_Dialog);
 public:
  ResultPanel_Dialog() {};
  ResultPanel_Dialog(wxWindow* parent, int id);
  virtual ~ResultPanel_Dialog();
  virtual bool TransferDataToWindow();

  double mw_gross_;
  double mw_net_;
  double net_eff_;
  double coal_in_;
  double water_in_;
  double oxid_in_;
  double co2_in_;
  double co2_out_;
  double co2_cap_;
  double capital_cst_;
  double elec_cst_;
  
 protected:

  wxTextCtrl* mw_gross;
  wxTextCtrl* mw_net;
  wxTextCtrl* net_eff;
  wxTextCtrl* coal_in;
  wxTextCtrl* water_in;
  wxTextCtrl* oxid_in;
  wxTextCtrl* co2_in;
  wxTextCtrl* co2_out;
  wxTextCtrl* co2_cap;
  wxTextCtrl* capital_cst;
  wxTextCtrl* elec_cst;
};

#endif
