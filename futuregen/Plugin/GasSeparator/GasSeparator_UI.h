#ifndef GasSeparator_UI_H
#define GasSeparator_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class GasSeparator_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(GasSeparator_UI_Dialog);
 public:
  GasSeparator_UI_Dialog(wxWindow* parent, int id,
          double* purity,
          double* remain,
          string* specie);
  GasSeparator_UI_Dialog() {};
  
  virtual ~GasSeparator_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxComboBox* cb_specie;
  wxTextCtrl* t_purity;
  wxTextCtrl* t_remain;
  
 public:
  double* p_purity;
  double* p_remain;
  string* p_specie;
  //GUI Variables
};

#endif

