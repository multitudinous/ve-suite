#ifndef WGSR_H
#define WGSR_H
#include "wx/image.h"
#include "Plugin_base.h"


#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

class WGSR : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(WGSR)

 public:
  WGSR();
  
  double co_conv_eff;
  double steam_added;

  
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
