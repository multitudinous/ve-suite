#ifndef GASI_H
#define GASI_H

#include "wx/image.h"
#include "Plugin_base.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

class GASI : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(GASI)

 public:
  GASI();
  
  long tex_idx_in_ntrains;
  long tex_idx_in_nspares;
  double tex_idx_in_temp;
  double wc_ratio;

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
