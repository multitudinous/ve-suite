#ifndef V21ASU_UI_H
#define V21ASU_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class V21ASU_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(V21ASU_UI_Dialog);
 public:
  V21ASU_UI_Dialog(wxWindow* parent, int id,
          double* o2_temp,
          double* o2_pres,
          double* o2_purity,
          double* n2_temp,
          double* n2_pres);
  V21ASU_UI_Dialog() {};
  
  virtual ~V21ASU_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables

  wxTextCtrl* t_o2_temp;
  wxTextCtrl* t_o2_pres;
  wxTextCtrl* t_o2_purity;
  wxTextCtrl* t_n2_temp;
  wxTextCtrl* t_n2_pres;

 public:
  double* p_o2_temp;
  double* p_o2_pres;
  double* p_o2_purity;
  double* p_n2_temp;
  double* p_n2_pres;
  //GUI Variables
};

#endif

