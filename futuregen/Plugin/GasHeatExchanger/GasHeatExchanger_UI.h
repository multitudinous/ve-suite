#ifndef GasHeatExchanger_UI_H
#define GasHeatExchanger_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class GasHeatExchanger_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(GasHeatExchanger_UI_Dialog);
 public:
  GasHeatExchanger_UI_Dialog(wxWindow* parent, int id,
          double* desired_temp);
  GasHeatExchanger_UI_Dialog() {};
  
  virtual ~GasHeatExchanger_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables

  wxTextCtrl* t_desired_temp;
 public:
  double* p_desired_temp;
  //GUI Variables
};

#endif

