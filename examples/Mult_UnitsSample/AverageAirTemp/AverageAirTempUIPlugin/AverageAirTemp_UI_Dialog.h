#ifndef AverageAirTemp_UI_DIALOG_H
#define AverageAirTemp_UI_DIALOG_H
#include "VE_Conductor/Framework/UIDialog.h"
#include <vector>
#include <string>

class AverageAirTemp_UI_Dialog : public UIDialog
{
   public:
  AverageAirTemp_UI_Dialog(wxWindow* parent, int id,
          double* intakediam,
          double* airvel,
          double* intaketemp,
          double* airinlettemp,
          double* intakelength);
      AverageAirTemp_UI_Dialog() {;}
  
      virtual ~AverageAirTemp_UI_Dialog();
  
      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();
      virtual void Lock(bool l); 
   protected:
      //UI widgets variables
  
   public:
  double* p_intakediam;
  double* p_airvel;
  double* p_intaketemp;
  double* p_airinlettemp;
  double* p_intakelength;
      //GUI Variables
};

#endif

