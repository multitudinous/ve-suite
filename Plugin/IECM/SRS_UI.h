#ifndef SRS_UI_H
#define SRS_UI_H
#include "UIDialog.h"

enum {
  H2S_RMV_EFF,
  SULF_RCV_EFF_CP,
  TAIL_GAS_SRE,
};

class SRS_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(SRS_UI_Dialog);
 public:
  SRS_UI_Dialog() {};
  SRS_UI_Dialog(wxWindow* parent, int id, double* tail_gas_sre_d, double* h2s_rmv_eff_d, double* sulf_rcv_eff_d);
  virtual ~SRS_UI_Dialog();
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();

  wxTextCtrl* h2s_rmv_eff;
  wxTextCtrl* sulf_rcv_eff_cp;
  wxTextCtrl* tail_gas_sre;

  double *tail_gas_sre_d_;
  double *h2s_rmv_eff_d_;
  double *sulf_rcv_eff_d_;
};

#endif
