#ifndef Condenser_UI_H
#define Condenser_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class Condenser_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(Condenser_UI_Dialog);
 public:
  Condenser_UI_Dialog(wxWindow* parent, int id,
          double* tube_id,
          double* tube_od,
          double* tube_length,
          double* int_press_drop,
          double* ext_press_drop,
          long* num_tubeH,
          long* num_tubeV);
  Condenser_UI_Dialog() {};
  
  virtual ~Condenser_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables

  wxTextCtrl* t_num_tubeH;
  wxTextCtrl* t_num_tubeV;
  wxTextCtrl* t_tube_id;
  wxTextCtrl* t_tube_od;
  wxTextCtrl* t_tube_length;
  wxTextCtrl* t_int_press_drop;
  wxTextCtrl* t_ext_press_drop;

 public:
  double* p_tube_id;
  double* p_tube_od;
  double* p_tube_length;
  double* p_int_press_drop;
  double* p_ext_press_drop;
  long* p_num_tubeH;
  long* p_num_tubeV;
  //GUI Variables
};

#endif

