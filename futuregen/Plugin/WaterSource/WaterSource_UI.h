#ifndef WaterSource_UI_H
#define WaterSource_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum {
  R_CASETYPE_EVA,
  R_CASETYPE_DES
};

class WaterSource_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(WaterSource_UI_Dialog);
 public:
  WaterSource_UI_Dialog(wxWindow* parent, int id,
          double* temp,
          double* pres,
          double* enth,
          double* flow,
          long* case_type);
  WaterSource_UI_Dialog() {};
  
  virtual ~WaterSource_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_temp;
  wxTextCtrl* t_pres;
  wxTextCtrl* t_enth;
  wxTextCtrl* t_flow;

  wxRadioButton* r_case_type_eva;
  wxRadioButton* r_case_type_des;

 public:
  double* p_temp;
  double* p_pres;
  double* p_enth;
  double* p_flow;
  long* p_case_type;
  //GUI Variables

  void OnCaseTypeChange(wxCommandEvent &event);

  DECLARE_EVENT_TABLE()
};

#endif

