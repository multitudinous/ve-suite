#ifndef HeatExchange0D_UI_H
#define HeatExchange0D_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class HeatExchange0D_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(HeatExchange0D_UI_Dialog);
 public:
  HeatExchange0D_UI_Dialog(wxWindow* parent, int id,
          double* desired_temp);
  HeatExchange0D_UI_Dialog() {};
  
  virtual ~HeatExchange0D_UI_Dialog();
  
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

