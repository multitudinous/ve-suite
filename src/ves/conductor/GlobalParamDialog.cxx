/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/conductor/GlobalParamDialog.h>

#include <wx/textctrl.h>
#include <wx/combobox.h>
#include <wx/radiobut.h>
#include <wx/button.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/sizer.h>
using namespace ves::conductor;

BEGIN_EVENT_TABLE(GlobalParamDialog, wxDialog)
  EVT_RADIOBUTTON(RADIO_A, GlobalParamDialog::OnChange)
  EVT_RADIOBUTTON(RADIO_B, GlobalParamDialog::OnChange)
END_EVENT_TABLE()

GlobalParamDialog::GlobalParamDialog(wxWindow *parent, wxWindowID id)
  : wxDialog((wxWindow *)parent, id, wxT("Global Parameters"), wxDefaultPosition,wxDefaultSize)
{
  wxSize entry_size(100, 17);
  wxSize tag_size(250, 15);
  int i;
  plant_capacity_d = 75.0;
  
  year_costs_s=_T("2000");
  cst_cur_dollar_s=_T("Constant"); 
  fixed_charge_d = 0.1480;
  discnt_rate_d = 0.1030;
  inflation_rate_d = 0.0;
  plant_life_d = 30.00;
  bond_interest_d = 9.000;
  preferred_stock_return_d = 8.500;
  common_stock_return_d = 12.00;
  percent_debt_d = 45.00;
  percent_p_equity_d = 10.00;
  percent_c_equity_d = 45.00;
  fed_tax_d = 35.00;
  state_tax_d = 4.000;
  property_tax_d = 2.000;
  invest_tax_credit_d = 0.0; 
  use_l = 1;

  wxBoxSizer* toptop = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);
  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  wxStaticBox *glb_lbl = new wxStaticBox(this, -1, _("Global Parameters") );
  wxStaticBoxSizer *glb_sizer = new wxStaticBoxSizer(glb_lbl, wxHORIZONTAL);

  wxStaticBox *finance_lbl = new wxStaticBox(this, -1, _("Financial Parameters") );
  wxStaticBoxSizer *finance_sizer = new wxStaticBoxSizer(finance_lbl, wxVERTICAL);

  wxBoxSizer *ok_row=new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(5, 10);
  top_sizer->Add(glb_sizer, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 10);
  top_sizer->Add(finance_sizer, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 10);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(5, 10);

  specify_a = new wxRadioButton(this, RADIO_A, _T(" Specify : "), wxDefaultPosition, wxDefaultSize, wxRB_GROUP);

  specify_a->SetValue(true);

  specify_b = new wxRadioButton(this, RADIO_B, _T(" Specify : "));

  specify_a->SetValue(false);

  wxStaticText* label0 = new wxStaticText(this, -1, _T("Plant Capacity Factor "));
  plant_capacity = new wxTextCtrl(this, PLANT_CAP, wxT(" "),
				  wxDefaultPosition, entry_size);
  glb_sizer->Add(label0);
  glb_sizer->Add(plant_capacity);

  wxBoxSizer * fin_sizer[16];
  wxStaticText * fin_lbl[16];
  
  finance_sizer->Add(5, 10);
  for (i=0; i<16; i++)
    {
      if (i==2)
	{
	  finance_sizer->Add(specify_a);
	}
      fin_sizer[i]=new wxBoxSizer(wxHORIZONTAL);
      if (i==4)
	{
	  finance_sizer->Add(specify_b);
	}
      finance_sizer->Add(fin_sizer[i], 0, wxALIGN_CENTER_HORIZONTAL);
      //finance_sizer->Add(5, 5);
    }
  finance_sizer->Add(5, 10);

  fin_lbl[0] = new wxStaticText(this, -1, _T("Year Costs Reported"), wxDefaultPosition, tag_size);
  
  wxString years[]={_("1977"), _("1978"), _("1979"), _("1980"), _("1981"), _("1982"), _("1983"), _("1984"), _("1985"), _("1986"), 
		    _("1987"), _("1988"), _("1989"), _("1990"), _("1991"), _("1992"), _("1993"), _("1994"), _("1995"), _("1996"), 
		    _("1997"), _("1998"), _("1999"), _("2000"), _("2001"), _("2002"), _("2003")};
  
  year_costs = new wxComboBox(this, YEAR_COSTS, _T("2002"), wxDefaultPosition, entry_size, 27, years, wxCB_DROPDOWN|wxCB_READONLY);

  fin_sizer[0]->Add(fin_lbl[0],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[0]->Add(year_costs,0,wxALIGN_CENTER_HORIZONTAL);

  wxString dollars[]={_("Constant"), _("Current")};

  fin_lbl[1] = new wxStaticText(this, -1, _T("Constant or Current Dollars?"), wxDefaultPosition, tag_size);
  cst_cur_dollar = new wxComboBox(this, CST_CUR_DOLLAR, _T("Constant"), wxDefaultPosition, entry_size, 2, dollars, wxCB_DROPDOWN|wxCB_READONLY);

  fin_sizer[1]->Add(fin_lbl[1],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[1]->Add(cst_cur_dollar,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[2]= new wxStaticText(this, -1, _T("Fixed Charge Factor"), wxDefaultPosition, tag_size);
  fixed_charge = new wxTextCtrl(this, FIXED_CHARGE, _T("0.1480"), wxDefaultPosition, entry_size);
  fin_sizer[2]->Add(fin_lbl[2],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[2]->Add(fixed_charge,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[3]= new wxStaticText(this, -1, _T("Discount Rate (Before Taxes)"), wxDefaultPosition, tag_size);
  discnt_rate = new wxTextCtrl(this, DISCNT_RATE, _T("0.1030"), wxDefaultPosition, entry_size);
  fin_sizer[3]->Add(fin_lbl[3],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[3]->Add(discnt_rate,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[4]= new wxStaticText(this, -1, _T("Inflation Rate (%/yr)"), wxDefaultPosition, tag_size);
  inflation_rate = new wxTextCtrl(this, INFLATION_RATE, _T("0.0"), wxDefaultPosition, entry_size);
  fin_sizer[4]->Add(fin_lbl[4],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[4]->Add(inflation_rate,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[5]= new wxStaticText(this, -1, _T("Plant or Project Book Life (years)"), wxDefaultPosition, tag_size);
  plant_life = new wxTextCtrl(this, PLANT_LIFE, _T("30.00"), wxDefaultPosition, entry_size);
  fin_sizer[5]->Add(fin_lbl[5],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[5]->Add(plant_life,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[6]= new wxStaticText(this, -1, _T("Real Bond Interest Rate (%)"), wxDefaultPosition, tag_size);
  bond_interest = new wxTextCtrl(this, BOND_INTEREST, _T("9.000"), wxDefaultPosition, entry_size);
  fin_sizer[6]->Add(fin_lbl[6],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[6]->Add(bond_interest,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[7]= new wxStaticText(this, -1, _T("Real Preferred Stock Return (%)"), wxDefaultPosition, tag_size);
  preferred_stock_return = new wxTextCtrl(this, PREFERRED_STOCK_RETURN, _T("8.5000"), wxDefaultPosition, entry_size);
  fin_sizer[7]->Add(fin_lbl[7],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[7]->Add(preferred_stock_return,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[8]= new wxStaticText(this, -1, _T("Real Common Stock Return (%)"), wxDefaultPosition, tag_size);
  common_stock_return = new wxTextCtrl(this, COMMON_STOCK_RETURN, _T("12.00"), wxDefaultPosition, entry_size);
  fin_sizer[8]->Add(fin_lbl[8],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[8]->Add(common_stock_return, 0, wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[9]= new wxStaticText(this, -1, _T("Percent Debt (%)"), wxDefaultPosition, tag_size);
  percent_debt = new wxTextCtrl(this, PERCENT_DEBT, _T("45"), wxDefaultPosition, entry_size);
  fin_sizer[9]->Add(fin_lbl[9],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[9]->Add(percent_debt,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[10]= new wxStaticText(this, -1, _T("Percent Equity (Preferred Stock) (%)"), wxDefaultPosition, tag_size);
  percent_p_equity = new wxTextCtrl(this, PERCENT_P_EQUITY, _T("10.00"), wxDefaultPosition, entry_size);
  fin_sizer[10]->Add(fin_lbl[10],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[10]->Add(percent_p_equity,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[11]= new wxStaticText(this, -1, _T("Percent Equity (Common Stock) (%)"), wxDefaultPosition, tag_size);
  percent_c_equity = new wxTextCtrl(this, PERCENT_C_EQUITY, _T("45.00"), wxDefaultPosition, entry_size);
  fin_sizer[11]->Add(fin_lbl[11],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[11]->Add(percent_c_equity,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[12]= new wxStaticText(this, -1, _T("Federal Tax Rate (%)"), wxDefaultPosition, tag_size);
  fed_tax = new wxTextCtrl(this, FED_TAX, _T("35"), wxDefaultPosition, entry_size);
  fin_sizer[12]->Add(fin_lbl[12],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[12]->Add(fed_tax,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[13]= new wxStaticText(this, -1, _T("State Tax Rate (%)"), wxDefaultPosition, tag_size);
  state_tax = new wxTextCtrl(this, STATE_TAX, _T("4"), wxDefaultPosition, entry_size);
  fin_sizer[13]->Add(fin_lbl[13],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[13]->Add(state_tax,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[14]= new wxStaticText(this, -1, _T("Property Tax Rate (%)"), wxDefaultPosition, tag_size);
  property_tax = new wxTextCtrl(this, PROPERTY_TAX, _T("2.000"), wxDefaultPosition, entry_size);
  fin_sizer[14]->Add(fin_lbl[14],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[14]->Add(property_tax,0,wxALIGN_CENTER_HORIZONTAL);

  fin_lbl[15]= new wxStaticText(this, -1, _T("Investment Tax Credit"), wxDefaultPosition, tag_size);
  invest_tax_credit = new wxTextCtrl(this, INVEST_TAX_CREDIT, _T("0.0"), wxDefaultPosition, entry_size);
  fin_sizer[15]->Add(fin_lbl[15],0,wxALIGN_CENTER_HORIZONTAL);
  fin_sizer[15]->Add(invest_tax_credit,0,wxALIGN_CENTER_HORIZONTAL);

 //The ok row
  ok_b = new wxButton(this, wxID_OK, _("OK"));
  cancel_b = new wxButton(this, wxID_CANCEL, _("Cancel"));
  ok_row->Add(ok_b, 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(cancel_b, 0, wxALIGN_CENTER_HORIZONTAL);

  this->SetAutoLayout( true );
  this->SetSizer(toptop); 
 
  toptop->Fit(this);
}

bool GlobalParamDialog::TransferDataToWindow()
{
  double2entry(plant_capacity, &plant_capacity_d);
  
  year_costs->SetValue(year_costs_s);
  cst_cur_dollar->SetValue(cst_cur_dollar_s);

  double2entry(fixed_charge, &fixed_charge_d);
  double2entry(discnt_rate,  &discnt_rate_d);
  double2entry(inflation_rate, &inflation_rate_d);
  double2entry(plant_life, &plant_life_d);
  double2entry(bond_interest, &bond_interest_d);
  double2entry(preferred_stock_return, &preferred_stock_return_d);
  double2entry(common_stock_return, &common_stock_return_d);
  double2entry(percent_debt, &percent_debt_d);
  double2entry(percent_p_equity, &percent_p_equity_d);
  double2entry(percent_c_equity, &percent_c_equity_d);
  double2entry(fed_tax, &fed_tax_d);
  double2entry(state_tax, &state_tax_d);
  double2entry(property_tax, &property_tax_d);
  double2entry(invest_tax_credit, &invest_tax_credit_d); 
  wxCommandEvent event;
  if (use_l)
    {
      specify_a->SetValue(true);
      specify_b->SetValue(false);
    }
  else
    {
      specify_a->SetValue(false);
      specify_b->SetValue(true);
    }

  OnChange(event);

  return true;

}

GlobalParamDialog::~GlobalParamDialog()
{
   ;
}

void GlobalParamDialog::OnChange(wxCommandEvent& WXUNUSED(event) )
{
  if (specify_a->GetValue())
    {
      fixed_charge->Enable(true);
      discnt_rate->Enable(true);
      inflation_rate->Enable(false);
      plant_life->Enable(false);
      bond_interest->Enable(false);
      preferred_stock_return->Enable(false);
      common_stock_return->Enable(false);
      percent_debt->Enable(false);
      percent_p_equity->Enable(false);
      percent_c_equity->Enable(false);
      fed_tax->Enable(false);
      state_tax->Enable(false);
      property_tax->Enable(false);
      invest_tax_credit->Enable(false);
    }
  else
    {
      fixed_charge->Enable(false);
      discnt_rate->Enable(false);
      inflation_rate->Enable(true);
      plant_life->Enable(true);
      bond_interest->Enable(true);
      preferred_stock_return->Enable(true);
      common_stock_return->Enable(true);
      percent_debt->Enable(true);
      percent_p_equity->Enable(true);
      percent_c_equity->Enable(true);
      fed_tax->Enable(true);
      state_tax->Enable(true);
      property_tax->Enable(true);
      invest_tax_credit->Enable(true);
    }
}

bool GlobalParamDialog::TransferDataFromWindow()
{
/*  entry2double(plant_capacity, &plant_capacity_d);

  year_costs_s=year_costs->GetValue();
  cst_cur_dollar_s=cst_cur_dollar->GetValue();

  entry2double(fixed_charge, &fixed_charge_d);
  if ((fixed_charge_d)>1.0 || (fixed_charge_d)<0.0)
    {
      wxMessageBox("Fixed Charge Factor should be between 0.0 to 1.0!", "Error!");
      return false;
    } 
  entry2double(discnt_rate,  &discnt_rate_d);
  if ((discnt_rate_d)>2.0 || (discnt_rate_d)<0.0)
    {
      wxMessageBox("Discount Rate should be between 0.0 to 2.0!", "Error!");
      return false;
    }
  entry2double(inflation_rate, &inflation_rate_d);
  if ((inflation_rate_d)>20.0 || (inflation_rate_d)<0.0)
    {
      wxMessageBox("Inflation rate should be between 0.0 to 20.0!", "Error!");
      return false;
    } 
  entry2double(plant_life, &plant_life_d);
  if ((plant_life_d)>60.0 || (plant_life_d)<5.0)
    {
      wxMessageBox("Plant life should be between 5.0 to 60.0!", "Error!");
      return false;
    } 
  entry2double(bond_interest, &bond_interest_d);
  if ((bond_interest_d)>15.0 || (bond_interest_d)<0.0)
    {
      wxMessageBox("Bond interest rate should be between 0.0 to 15.0!", "Error!");
      return false;
    } 
  entry2double(preferred_stock_return, &preferred_stock_return_d);
  if ((preferred_stock_return_d)>20.0 || (preferred_stock_return_d)<0.0)
    {
      wxMessageBox("Preferred Stock Return should be between 0.0 to 20.0!", "Error!");
      return false;
    } 
  entry2double(common_stock_return, &common_stock_return_d);
  if ((common_stock_return_d)>25.0 || (common_stock_return_d)<0.0)
    {
      wxMessageBox("Common Stock Return should be between 0.0 to 25.0!", "Error!");
      return false;
    } 
  entry2double(percent_debt, &percent_debt_d);
  if ((percent_debt_d)>100.0 || (percent_debt_d)<0.0)
    {
      wxMessageBox("Percent Debt should be between 0.0 to 100.0!", "Error!");
      return false;
    } 
  entry2double(percent_p_equity, &percent_p_equity_d);
  if ((percent_p_equity_d)>100.0 || (percent_p_equity_d)<0.0)
    {
      wxMessageBox("Percent Equity(Preferred Stock) should be between 0.0 to 100.0!", "Error!");
      return false;
    } 
  entry2double(percent_c_equity, &percent_c_equity_d);
  if ((percent_c_equity_d)>100.0 || (percent_c_equity_d)<0.0)
    {
      wxMessageBox("Percent Equity(Common Stock) should be between 0.0 to 100.0!", "Error!");
      return false;
    } 
  entry2double(fed_tax, &fed_tax_d);
  if ((fed_tax_d)>50.0 || (fed_tax_d)<15.0)
    {
      wxMessageBox("Federal Tax Rate should be between 15.0 to 50.0!", "Error!");
      return false;
    } 
  entry2double(state_tax, &state_tax_d);
  if ((state_tax_d)>10.0 || (state_tax_d)<0.0)
    {
      wxMessageBox("State Tax Rate should be between 0.0 to 10.0!", "Error!");
      return false;
    } 
  entry2double(property_tax, &property_tax_d);
  if ((property_tax_d)>5.0 || (property_tax_d)<0.0)
    {
      wxMessageBox("Property Tax Rate should be between 0.0 to 5.0!", "Error!");
      return false;
    } 
  entry2double(invest_tax_credit, &invest_tax_credit_d); 
  if ((invest_tax_credit_d)>20.0 || (invest_tax_credit_d)<0.0)
    {
      wxMessageBox("Investment Tax Credit should be between 0.0 to 20.0!", "Error!");
      return false;
    } 
  if (specify_a->GetValue())
    use_l = 1;
  else
    use_l = 0;*/
  return true;
}

void GlobalParamDialog::double2entry(wxTextCtrl* entry, double * value)
{
  wxString txt;
  txt<<(*value);
  entry->SetValue(txt);
}

void GlobalParamDialog::entry2double(wxTextCtrl* entry, double * value)
{
  wxString txt;
  txt=entry->GetValue();
  (*value) = atof( ConvertUnicode( txt.c_str() ).c_str());
}
/*
void GlobalParamDialog::UnPack(Interface *intf)
{
  
  std::string temp;

  globalparam_intf=*intf;
  globalparam_intf.getVal("plant_capacity", plant_capacity_d);
  globalparam_intf.getVal("year_costs", temp);
  year_costs_s = wxString( temp.c_str(), wxConvUTF8);
  globalparam_intf.getVal("cst_cur_dollar", temp);
  cst_cur_dollar_s = wxString(temp.c_str(), wxConvUTF8);
  globalparam_intf.getVal("fixed_charge", fixed_charge_d);
  globalparam_intf.getVal("discnt_rate", discnt_rate_d);
  globalparam_intf.getVal("inflation_rate", inflation_rate_d);
  globalparam_intf.getVal("plant_life", plant_life_d);
  globalparam_intf.getVal("bond_interest", bond_interest_d);
  globalparam_intf.getVal("preferred_stock_return", preferred_stock_return_d);
  globalparam_intf.getVal("common_stock_return", common_stock_return_d);
  globalparam_intf.getVal("percent_debt", percent_debt_d);
  globalparam_intf.getVal("percent_p_equity", percent_p_equity_d);
  globalparam_intf.getVal("percent_c_equity", percent_c_equity_d);
  globalparam_intf.getVal("fed_tax", fed_tax_d);
  globalparam_intf.getVal("state_tax", state_tax_d);
  globalparam_intf.getVal("property_tax", property_tax_d);
  globalparam_intf.getVal("invest_tax_credit", invest_tax_credit_d);
  globalparam_intf.getVal("which_to_use", use_l);
  TransferDataToWindow();
}

Interface *GlobalParamDialog::Pack()
{
  std::string temp;

  globalparam_intf._id= 100000 ; //The id for the globalparam

  globalparam_intf.setVal("plant_capacity", plant_capacity_d);
  temp = ConvertUnicode( year_costs_s.c_str());
  globalparam_intf.setVal("year_costs", temp);
  temp = ConvertUnicode(cst_cur_dollar_s.c_str());  
  globalparam_intf.setVal("cst_cur_dollar", temp);
  globalparam_intf.setVal("fixed_charge", fixed_charge_d);
  globalparam_intf.setVal("discnt_rate", discnt_rate_d);
  globalparam_intf.setVal("inflation_rate", inflation_rate_d);
  globalparam_intf.setVal("plant_life", plant_life_d);
  globalparam_intf.setVal("bond_interest", bond_interest_d);
  globalparam_intf.setVal("preferred_stock_return", preferred_stock_return_d);
  globalparam_intf.setVal("common_stock_return", common_stock_return_d);
  globalparam_intf.setVal("percent_debt", percent_debt_d);
  globalparam_intf.setVal("percent_p_equity", percent_p_equity_d);
  globalparam_intf.setVal("percent_c_equity", percent_c_equity_d);
  globalparam_intf.setVal("fed_tax", fed_tax_d);
  globalparam_intf.setVal("state_tax", state_tax_d);
  globalparam_intf.setVal("property_tax", property_tax_d);
  globalparam_intf.setVal("invest_tax_credit", invest_tax_credit_d);
  globalparam_intf.setVal("which_to_use", use_l);
  
  return &globalparam_intf;
}
*/