#ifndef DumpCombustor_UI_H
#define DumpCombustor_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum {
  R_CASETYPE_EVA,
  R_CASETYPE_DES
};

class DumpCombustor_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(DumpCombustor_UI_Dialog);
 public:
  DumpCombustor_UI_Dialog(wxWindow* parent, int id,
          double* conversion,
          double* volume,
          double* fracQloss,
          double* press_drop,
          long* case_type);
  DumpCombustor_UI_Dialog() {};
  
  virtual ~DumpCombustor_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_conversion;
  wxTextCtrl* t_volume;
  wxTextCtrl* t_fracQloss;
  wxTextCtrl* t_press_drop;

  wxRadioButton* r_case_type_eva;
  wxRadioButton* r_case_type_des;

 public:
  double* p_conversion;
  double* p_volume;
  double* p_fracQloss;
  double* p_press_drop;
  long* p_case_type;
  //GUI Variables
  void OnCaseTypeChange(wxCommandEvent &event);

  DECLARE_EVENT_TABLE()
};

#endif

