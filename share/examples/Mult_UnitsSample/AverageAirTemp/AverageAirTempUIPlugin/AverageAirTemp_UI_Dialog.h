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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef AverageAirTemp_UI_DIALOG_H
#define AverageAirTemp_UI_DIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include <vector>
#include <string>
#include <wx/wx.h>

using namespace std;

enum {
	CLOSE_EXCEL
};

class AverageAirTemp_UI_Dialog : public UIDialog
{
   DECLARE_DYNAMIC_CLASS(AverageAirTemp_UI_Dialog);
   public:
  AverageAirTemp_UI_Dialog(wxWindow* parent, int id,
          double* intakediam,
          double* airvel,
          double* intaketemp,
          double* airinlettemp,
          double* intakelength,
          long* closesheets);
      AverageAirTemp_UI_Dialog() {;}
  
      virtual ~AverageAirTemp_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l);

      void _onCloseExcel(wxCommandEvent& event);
   protected:
      //UI widgets variables
  
   public:
  double* p_intakediam;
  double* p_airvel;
  double* p_intaketemp;
  double* p_airinlettemp;
  double* p_intakelength;
  long* p_closesheets;
  wxTextCtrl* _intakediamentry;
  wxTextCtrl* _airvelentry;
  wxTextCtrl* _intaketempentry;
  wxTextCtrl* _airinlettempentry;
  wxTextCtrl* _intakelengthentry;
  wxButton*   _closeExcelButton;
  wxButton*   _updateButton;

  int closeSheets;
      //GUI Variables

  DECLARE_EVENT_TABLE();
};

#endif

