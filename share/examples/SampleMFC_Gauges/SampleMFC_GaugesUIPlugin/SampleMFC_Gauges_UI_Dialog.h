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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef SampleMFC_Gauges_UI_DIALOG_H
#define SampleMFC_Gauges_UI_DIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include <vector>
#include <string>
#include <wx/wx.h>

using namespace std;

enum {
  CALC_METHOD_RADIOBOX,
	LIST1_COMBOBOX,
	LIST2_COMBOBOX,
	LIST3_COMBOBOX,
	ADD_SLIDER,
	CLOSE_EXCEL
};

class SampleMFC_Gauges_UI_Dialog : public UIDialog
{
	DECLARE_DYNAMIC_CLASS(SampleMFC_Gauges_UI_Dialog);
   public:
  SampleMFC_Gauges_UI_Dialog(wxWindow* parent, int id,
          double* dbl1,
          double* dbl2,
          long* int1,
					long* int2,
          vector<double>* dbllist);
      SampleMFC_Gauges_UI_Dialog() {;}
  
      virtual ~SampleMFC_Gauges_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l); 

			void _onCalcMethod(wxCommandEvent& event);
			void _onCloseExcel(wxCommandEvent& event);
   protected:
      //UI widgets variables
  
   public:
  double* p_dbl1;
  double* p_dbl2;
  long* p_int1;
  long* p_int2;
  vector<double>* p_dbllist;
      //GUI Variables
	wxRadioBox* _selcalcRBox;
	wxComboBox* _list1sel;
	wxComboBox* _list2sel;
	wxComboBox* _list3sel;
	wxTextCtrl* _scalefactorentry;
	wxSlider*   _addfactorSlider;
	wxButton*   _closeExcelButton;
	wxButton*   _updateButton;

	int closeSheets;

	
	DECLARE_EVENT_TABLE();
};

#endif

