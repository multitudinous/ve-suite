#ifndef GASI_UI_H
#define GASI_UI_H
#include "UIDialog.h"

enum {
  GASI_TEMP,
  PRESSURE,
  STEAM_INPUT,
  TEX_NTRAINS,
  TEX_NSPARES
};

class GASI_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(GASI_UI_Dialog);
 public:
  GASI_UI_Dialog() {};
  GASI_UI_Dialog(wxWindow* parent, int id, long *tex_idx_in_ntrains, long* tex_idx_in_nspares, double* tex_idx_in_temp, double *wc_ratio);
  virtual ~GASI_UI_Dialog();
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();

  wxComboBox* gasi_temp;
  wxComboBox* tex_ntrains;
  wxComboBox* tex_nspares;
  wxComboBox* steam_input;
  wxTextCtrl* pressure;

  long *tex_ntrains_;
  long *tex_nspares_;
  double *tex_temp_;
  double *wc_ratio_;

  DECLARE_EVENT_TABLE()
};

#endif
