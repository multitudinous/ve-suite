#ifndef SRS_H
#define SRS_H

#include "Plugin_base.h"
#include "wx/image.h"
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

class SRS : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(SRS)

 public:
  SRS();
  
  double tail_gas_sre_d;
  double h2s_rmv_eff_d;
  double sulf_rcv_eff_d;

  wxBitmap *my_icon;
  int icon_w, icon_h;

  virtual wxString GetName();
  virtual wxString GetDesc();
  virtual UIDialog* UI(wxWindow* parent);
  virtual void DrawIcon(wxDC *dc);
  //virtual UIDialog* Result(wxWindow* parent);
  virtual void GetIPorts(POLY &iports);
  virtual void GetOPorts(POLY& ports);
};


#endif
