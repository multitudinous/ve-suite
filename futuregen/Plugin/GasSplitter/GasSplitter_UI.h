#ifndef GasSplitter_UI_H
#define GasSplitter_UI_H
#include "UIDialog.h"
#include <vector>
#include <string>

using namespace std;

class GasSplitter_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(GasSplitter_UI_Dialog);
 public:
  GasSplitter_UI_Dialog(wxWindow* parent, int id,
          double* percent_port1,
          double* percent_port2,
          double* percent_port3,
          double* percent_port4);
  GasSplitter_UI_Dialog() {};
  
  virtual ~GasSplitter_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  wxTextCtrl* t_percent_port1;
  wxTextCtrl* t_percent_port2;
  wxTextCtrl* t_percent_port3;
  wxTextCtrl* t_percent_port4; 
  
 public:
  double* p_percent_port1;
  double* p_percent_port2;
  double* p_percent_port3;
  double* p_percent_port4;
  //GUI Variables
};

#endif

