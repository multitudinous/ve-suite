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

