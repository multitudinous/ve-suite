#ifndef POWER_UI_H
#define POWER_UI_H
#include "UIDialog.h"

enum {
  GAS_TURB_MOD,
  NUM_GASTURB,
  PRES_RATIO,
  STEAM_CYC_HEATRATE, 
  SOX_SO3,
  NOX_CONS,
  NOX_NO,
  CARBON_CO
};

class POWER_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(POWER_UI_Dialog);
 public:
  POWER_UI_Dialog() {};
  POWER_UI_Dialog(wxWindow* parent, int id, 
		  long *num_turbs, 
		  double *gts_idx_in_pres_ratio,
		  double *gts_idx_in_heat_rate,
		  double *gts_idx_in_sox_so3,
		  double *gts_idx_in_nox_cons,
		  double *gts_idx_in_nox_no,
		  double *gts_idx_in_carbon_co);
  virtual ~POWER_UI_Dialog();
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();

  wxComboBox* gas_turb_mod;
  wxComboBox* num_gasturb;
  wxTextCtrl* pres_ratio;
  wxTextCtrl* steam_cyc_heatrate;
  wxTextCtrl* sox_so3;
  wxTextCtrl* nox_cons;
  wxTextCtrl* nox_no;
  wxTextCtrl* carbon_co;

  long *num_gasturb_;
  double *gts_idx_in_pres_ratio_;
  double *gts_idx_in_heat_rate_;
  double *gts_idx_in_sox_so3_;
  double *gts_idx_in_nox_cons_;
  double *gts_idx_in_nox_no_;
  double *gts_idx_in_carbon_co_;

};

#endif
