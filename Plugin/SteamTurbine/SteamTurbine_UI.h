#ifndef SteamTurbine_UI_H
#define SteamTurbine_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class SteamTurbine_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(SteamTurbine_UI_Dialog);
 public:
  SteamTurbine_UI_Dialog(wxWindow* parent, int id,
          double* ad_eff,
          double* pressure_drop);
  SteamTurbine_UI_Dialog() {};
  
  virtual ~SteamTurbine_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_ad_eff;
  wxTextCtrl* t_pressure_drop;

 public:
  double* p_ad_eff;
  double* p_pressure_drop;
  //GUI Variables
};

#endif

