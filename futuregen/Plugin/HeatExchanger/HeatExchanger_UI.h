#ifndef HeatExchanger_UI_H
#define HeatExchanger_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>
#include "wx/combobox.h"

using namespace std;

enum {
  FINS
};

class HeatExchanger_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(HeatExchanger_UI_Dialog);
 public:
  HeatExchanger_UI_Dialog(wxWindow* parent, int id,
          double* Sl,
          double* St,
          double* tube_id,
          double* tube_od,
          double* tube_length,
          double* int_press_drop,
          double* ext_press_drop,
          double* fin_effect,
          string* arrangement,
          string* tube_config,
          long* num_tubeL,
          long* num_tubeX,
          long* use_fins);
  HeatExchanger_UI_Dialog() {};
  
  virtual ~HeatExchanger_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables

  wxTextCtrl* t_num_tubeL;
  wxTextCtrl* t_num_tubeX;
  wxTextCtrl* t_Sl;
  wxTextCtrl* t_St;
  wxTextCtrl* t_tube_id;
  wxTextCtrl* t_tube_od;
  wxTextCtrl* t_tube_length;
  wxTextCtrl* t_int_press_drop;
  wxTextCtrl* t_ext_press_drop;
  wxTextCtrl* t_fin_effect;
  wxComboBox* cb_arrangement;
  wxComboBox* cb_tube_config;
  wxCheckBox* c_use_fins;

 public:
  double* p_Sl;
  double* p_St;
  double* p_tube_id;
  double* p_tube_od;
  double* p_tube_length;
  double* p_int_press_drop;
  double* p_ext_press_drop;
  double* p_fin_effect;
  string* p_arrangement;
  string* p_tube_config;
  long* p_num_tubeL;
  long* p_num_tubeX;
  long* p_use_fins;
  //GUI Variables

  void OnFinsChange(wxCommandEvent &event);

  DECLARE_EVENT_TABLE()
};

#endif

