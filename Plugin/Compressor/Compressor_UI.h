#ifndef Compressor_UI_H
#define Compressor_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum {
  R_CASETYPE_EVA,
  R_CASETYPE_DES
};

class Compressor_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(Compressor_UI_Dialog);
 public:
  Compressor_UI_Dialog(wxWindow* parent, int id,
          double* eff,
          double* pressure_out,
          double* pressure_change,
          long* case_type);
  Compressor_UI_Dialog() {};
  
  virtual ~Compressor_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_eff;
  wxTextCtrl* t_pressure_out;
  wxTextCtrl* t_pressure_change;

  wxRadioButton* r_case_type_eva;
  wxRadioButton* r_case_type_des;
 public:
  double* p_eff;
  double* p_pressure_out;
  double* p_pressure_change;
  long* p_case_type;
  //GUI Variables
  void OnCaseTypeChange(wxCommandEvent &event);

  DECLARE_EVENT_TABLE()
};

#endif

