#ifndef REI_GASI_UI_H
#define REI_GASI_UI_H

#include "UIDialog.h"
#include "wx/image.h"

enum {
  STEAM_TEMP,
  STEAM_FLRT,
  //  SLURRY_TEMP,
  //  SLURRY_FLRT,
  //  SLURRY_COAL,
  //  SLURRY_CHAR,
  //  SLURRY_WATER,
  INJECT_TEMP,
  INJECT_ANGLE,
  WC_RATIO,
  OC_RATIO
};

class REI_Gasi_UI : public UIDialog
{
   DECLARE_DYNAMIC_CLASS(REI_Gasi_UI)
 public:
   REI_Gasi_UI() {}; 
  REI_Gasi_UI(wxWindow* parent, int id);
  virtual ~REI_Gasi_UI();
  virtual bool TransferDataToWindow();
  virtual bool TransferDataFromWindow();
  void OnChange(wxCommandEvent& event);
  wxTextCtrl *steam_temp;
  wxTextCtrl *steam_flrt;
  // wxTextCtrl *slurry_temp;
  //  wxTextCtrl *slurry_flrt;
  //  wxTextCtrl *slurry_coal;
  //  wxTextCtrl *slurry_char;
  //  wxTextCtrl *slurry_water;
  wxTextCtrl *inject_temp;
  wxTextCtrl *inject_angle;
  wxComboBox *wc_ratio;
  wxComboBox *oc_ratio;

  wxButton *ok_b;
  wxButton *cancel_b;
 protected:
  double s_temp;
  double s_flrt;
  //  double sl_temp;
  //  double sl_flrt;
  //  double sl_coal;
  //  double sl_char;
  //  double sl_water;
  double i_temp;
  double i_angle;
  double d_wc;
  double d_oc;

  void double2entry(wxTextCtrl* entry, double * value);
  void entry2double(wxTextCtrl* entry, double * value);
  // void Calc(wxCommandEvent& event);

  DECLARE_EVENT_TABLE()
};


#endif
