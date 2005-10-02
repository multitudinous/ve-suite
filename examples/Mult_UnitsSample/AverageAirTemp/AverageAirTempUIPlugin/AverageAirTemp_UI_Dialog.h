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

