#ifndef AGR_UI_H
#define AGR_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class AGR_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(AGR_UI_Dialog);
 public:
  AGR_UI_Dialog(wxWindow* parent, int id,
          double* solv_mw,
          double* solv_den,
          long* solv_type,
          long* tray_type);
  AGR_UI_Dialog() {};
  
  virtual ~AGR_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_solv_mw;
  wxTextCtrl* t_solv_den;
  wxComboBox* cb_solv_type;
  wxComboBox* cb_tray_type;
 public:
  double* p_solv_mw;
  double* p_solv_den;
  long* p_solv_type;
  long* p_tray_type;
  //GUI Variables
};

#endif

