#ifndef Power_H
#define Power_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

class Power : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(Power)

 public:
  Power();
  
  long gts_idx_in_nturb;
  double gts_idx_in_pres_ratio;
  double gts_idx_in_heat_rate;
  double gts_idx_in_sox_so3;
  double gts_idx_in_nox_cons;
  double gts_idx_in_nox_no;
  double gts_idx_in_carbon_co;

  wxBitmap *my_icon;
  int icon_w, icon_h;

  virtual wxString GetName();
  virtual wxString GetDesc();
  virtual UIDialog* UI(wxWindow* parent);
  virtual void DrawIcon(wxDC *dc);
  virtual void GetIPorts(POLY &iports);
  virtual void GetOPorts(POLY& ports);

  //virtual UIDialog* Result(wxWindow* parent);
};


#endif
