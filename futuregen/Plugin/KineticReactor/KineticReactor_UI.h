#ifndef KineticReactor_UI_H
#define KineticReactor_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

enum {
  R_CASETYPE
};

class KineticReactor_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(KineticReactor_UI_Dialog);
 public:
  KineticReactor_UI_Dialog(wxWindow* parent, int id,
          double* res_time,
          double* qloss,
          double* quench_rate,
          string* work_dir,
          long* case_type);
  KineticReactor_UI_Dialog() {};
  
  virtual ~KineticReactor_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl *t_res_time;
  wxTextCtrl *t_quench_rate;
  wxTextCtrl *t_qloss;
  wxRadioBox *r_case_type;
  
 public:
  double* p_res_time;
  double* p_qloss;
  double* p_quench_rate;
  string* p_work_dir;
  long* p_case_type;
  //GUI Variables
  
  void OnCaseTypeChange(wxCommandEvent &event);
  
  DECLARE_EVENT_TABLE()
};

#endif

