#ifndef SELX_UI_H
#define SELX_UI_H
#include "UIDialog.h"

enum {
  NUM_SPARES,
  CO2_REMOVAL
};

class SELX_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(SELX_UI_Dialog);
 public:
  SELX_UI_Dialog() {};
  SELX_UI_Dialog(wxWindow* parent, int id, long *num_idx_in_nspares, double *co2_removal_d);
  virtual ~SELX_UI_Dialog();

  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();

 protected:
  wxComboBox* num_spares;
  wxTextCtrl* co2_removal;

 public:
  long* num_spares_;
  double *co2_removal_d_;
};

#endif
