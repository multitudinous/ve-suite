#ifndef GLOBALPARAMDIALOG_H
#define GLOBALPARAMDIALOG_H

#include <wx/wx.h>
#include "interface.h"

enum {
  PLANT_CAP,
  YEAR_COSTS,
  CST_CUR_DOLLAR,
  FIXED_CHARGE,
  DISCNT_RATE,
  INFLATION_RATE,
  PLANT_LIFE,
  BOND_INTEREST,
  PREFERRED_STOCK_RETURN,
  COMMON_STOCK_RETURN,
  PERCENT_DEBT,
  PERCENT_P_EQUITY,
  PERCENT_C_EQUITY,
  FED_TAX,
  STATE_TAX,
  PROPERTY_TAX,
  INVEST_TAX_CREDIT,
  RADIO_A,
  RADIO_B
};

class GlobalParamDialog : public wxDialog
{
 public:
  GlobalParamDialog(wxWindow *parent, wxWindowID id );
  ~GlobalParamDialog();

 protected:
  wxTextCtrl* plant_capacity;
  
  wxComboBox* year_costs;
  wxComboBox* cst_cur_dollar;
  wxTextCtrl* fixed_charge;
  wxTextCtrl* discnt_rate;
  wxTextCtrl* inflation_rate;
  wxTextCtrl* plant_life;
  wxTextCtrl* bond_interest;
  wxTextCtrl* preferred_stock_return;
  wxTextCtrl* common_stock_return;
  wxTextCtrl* percent_debt;
  wxTextCtrl* percent_p_equity;
  wxTextCtrl* percent_c_equity;
  wxTextCtrl* fed_tax;
  wxTextCtrl* state_tax;
  wxTextCtrl* property_tax;
  wxTextCtrl* invest_tax_credit;
  
  wxRadioButton* specify_a;
  wxRadioButton* specify_b;
  wxButton * ok_b;
  wxButton * cancel_b;
  
  void double2entry(wxTextCtrl* entry, double * value);
  void entry2double(wxTextCtrl* entry, double * value);
  virtual bool TransferDataToWindow();
  virtual bool TransferDataFromWindow();
  Interface globalparam_intf;
 public:
  void UnPack(Interface* intf);
  Interface* Pack(); 
  
  double plant_capacity_d;
  
  void OnChange(wxCommandEvent &event);
  
  wxString year_costs_s;
  wxString cst_cur_dollar_s; 
  double fixed_charge_d;
  double discnt_rate_d;
  double inflation_rate_d;
  double plant_life_d;
  double bond_interest_d;
  double preferred_stock_return_d;
  double common_stock_return_d;
  double percent_debt_d;
  double percent_p_equity_d;
  double percent_c_equity_d;
  double fed_tax_d;
  double state_tax_d;
  double property_tax_d;
  double invest_tax_credit_d; 
  long use_l;

  DECLARE_EVENT_TABLE()
  
};

#endif
