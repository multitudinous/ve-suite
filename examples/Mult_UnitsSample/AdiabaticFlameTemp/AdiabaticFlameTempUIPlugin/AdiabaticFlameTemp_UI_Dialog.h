#ifndef AdiabaticFlameTemp_UI_DIALOG_H
#define AdiabaticFlameTemp_UI_DIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include <vector>
#include <string>
#include <wx/wx.h>

using namespace std;

enum {
	CLOSE_EXCEL
};

class AdiabaticFlameTemp_UI_Dialog : public UIDialog
{
   DECLARE_DYNAMIC_CLASS(AdiabaticFlameTemp_UI_Dialog);
   public:
  AdiabaticFlameTemp_UI_Dialog(wxWindow* parent, int id,
          double* perc_theor_error,
          long* closesheets);
      AdiabaticFlameTemp_UI_Dialog() {;}
  
      virtual ~AdiabaticFlameTemp_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l); 

      void _onCloseExcel(wxCommandEvent& event);
   protected:
      //UI widgets variables
  
   public:
  double* p_perc_theor_error;
  long* p_closesheets;
  wxTextCtrl* _errorentry;
  wxButton*   _closeExcelButton;
  wxButton*   _updateButton;

  int closeSheets;
      //GUI Variables

  DECLARE_EVENT_TABLE();
};

#endif

