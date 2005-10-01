#ifndef AdiabaticFlameTemp_UI_DIALOG_H
#define AdiabaticFlameTemp_UI_DIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include <vector>
#include <string>

class AdiabaticFlameTemp_UI_Dialog : public UIDialog
{
   public:
  AdiabaticFlameTemp_UI_Dialog(wxWindow* parent, int id,
          double* perc_theor_error);
      AdiabaticFlameTemp_UI_Dialog() {;}
  
      virtual ~AdiabaticFlameTemp_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l); 
   protected:
      //UI widgets variables
  
   public:
  double* p_perc_theor_error;
      //GUI Variables
};

#endif

