/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#ifndef RESULT_PANEL_H
#define RESULT_PANEL_H
/*!\file ResultPanel.h
ResultPanel API
*/
/*!\class ResultPanel
* 
*/
#include <wx/dialog.h>

#include "VE_Installer/include/VEConfig.h"

class wxTextCtrl;
class wxStaticText;
class wxBoxSizer;
class wxButton;

class VE_GUIPLUGINS_EXPORTS ResultPanel_Dialog : public wxDialog
{
public:
   ResultPanel_Dialog(){;}
   ResultPanel_Dialog(wxWindow* parent, int id);
   virtual ~ResultPanel_Dialog();

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
