#ifndef SOFC_UI_H
#define SOFC_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class SOFC_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(SOFC_UI_Dialog);
 public:
  SOFC_UI_Dialog(wxWindow* parent, int id,
          double* a_thickness,
          double* c_thickness,
          double* e_thickness,
          double* a_A,
          double* a_E,
          double* c_A,
          double* c_E,
          double* e_A,
          double* e_E,
          double* cell_area,
          double* num_cell,
          double* fuel_util,
          double* press_drop);
  SOFC_UI_Dialog() {};
  
  virtual ~SOFC_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_a_thickness;
  wxTextCtrl* t_c_thickness;
  wxTextCtrl* t_e_thickness;
  wxTextCtrl* t_a_A;
  wxTextCtrl* t_a_E;
  wxTextCtrl* t_c_A;
  wxTextCtrl* t_c_E;
  wxTextCtrl* t_e_A;
  wxTextCtrl* t_e_E;
  wxTextCtrl* t_cell_area;
  wxTextCtrl* t_num_cell;
  wxTextCtrl* t_fuel_util;
  wxTextCtrl* t_press_drop;

 public:
  double* p_a_thickness;
  double* p_c_thickness;
  double* p_e_thickness;
  double* p_a_A;
  double* p_a_E;
  double* p_c_A;
  double* p_c_E;
  double* p_e_A;
  double* p_e_E;
  double* p_cell_area;
  double* p_num_cell;
  double* p_fuel_util;
  double* p_press_drop;
  //GUI Variables
};

#endif

