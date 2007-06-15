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
#ifndef GLOBALPARAMDIALOG_H
#define GLOBALPARAMDIALOG_H
/*!\file GlobalParamDialog.h
GlobalParamDialog API
*/
/*!\class GlobalParamDialog
* 
*/

#include <wx/dialog.h>
#include <wx/string.h>
#include <wx/msgdlg.h>

#include <string>

class wxTextCtrl;
class wxComboBox;
class wxRadioButton;
class wxButton;
class wxStaticText;
class wxBoxSizer;
class wxStaticBoxSizer;

#include "VE_Installer/include/VEConfig.h"

class VE_GUIPLUGINS_EXPORTS GlobalParamDialog : public wxDialog
{
 public:
  GlobalParamDialog(wxWindow *parent, wxWindowID id );
  virtual ~GlobalParamDialog();

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
 public:
  //void UnPack(Interface* intf);
  //Interface* Pack(); 
  
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

  std::string ConvertUnicode( const wxChar* data )
  {
     std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
     return tempStr;
  }

  DECLARE_EVENT_TABLE()  
};

#endif
