#ifndef WGSR_UI_H
#define WGSR_UI_H
#include "UIDialog.h"

enum {
  CO_CONV_EFF,
  STEAM_ADDED
};

class WGSR_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(WGSR_UI_Dialog);
 public:
  WGSR_UI_Dialog() {};
  WGSR_UI_Dialog(wxWindow* parent, int id, double *con_conv_eff_d, double *steam_added_d);
  virtual ~WGSR_UI_Dialog();
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();

  wxTextCtrl* co_conv_eff;
  wxTextCtrl* steam_added;

  double *co_conv_eff_d_;
  double *steam_added_d_;
};

#endif
