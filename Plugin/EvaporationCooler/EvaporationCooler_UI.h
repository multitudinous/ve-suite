#ifndef EvaporationCooler_UI_H
#define EvaporationCooler_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class EvaporationCooler_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(EvaporationCooler_UI_Dialog);
 public:
  EvaporationCooler_UI_Dialog(wxWindow* parent, int id,
          double* desired_temp,
          double* air_temp,
          double* air_humidity,
          double* ambient_pres);
  EvaporationCooler_UI_Dialog() {};
  
  virtual ~EvaporationCooler_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables

  wxTextCtrl* t_desired_temp;
  wxTextCtrl* t_air_temp;
  wxTextCtrl* t_air_humidity;
  wxTextCtrl* t_ambient_pres;

 public:
  double* p_desired_temp;
  double* p_air_temp;
  double* p_air_humidity;
  double* p_ambient_pres;
  //GUI Variables
};

#endif

