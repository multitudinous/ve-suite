#ifndef CarbonBed_UI_H
#define CarbonBed_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class CarbonBed_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(CarbonBed_UI_Dialog);
 public:
  CarbonBed_UI_Dialog(wxWindow* parent, int id,
          double* press_drop,
          double* part_diam,
          double* bulk_density,
          double* temp,
          double* press,
          double* cr_time,
          double* porosity,
          double* res_time,
          long* carbon_type);
  CarbonBed_UI_Dialog() {};
  
  virtual ~CarbonBed_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_press_drop;
  wxTextCtrl* t_part_diam;
  wxTextCtrl* t_bulk_density;
  wxTextCtrl* t_temp;
  wxTextCtrl* t_press;
  wxTextCtrl* t_cr_time;
  wxTextCtrl* t_porosity;
  wxTextCtrl* t_res_time;
  wxComboBox* cb_carbon_type;

 public:
  double* p_press_drop;
  double* p_part_diam;
  double* p_bulk_density;
  double* p_temp;
  double* p_press;
  double* p_cr_time;
  double* p_porosity;
  double* p_res_time;
  long* p_carbon_type;
  //GUI Variables
};

#endif

