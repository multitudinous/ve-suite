#ifndef REI_GASI_H
#define REI_GASI_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

class REI_Gasi : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(REI_Gasi)

 public:
  REI_Gasi();
  long x;
  double y;
  string z;

  wxBitmap *my_icon;
  int icon_w, icon_h;
  
  virtual wxString GetName();
  virtual wxString GetDesc();
  //  virtual int GetNumPoly();
  //virtual void GetPoly(POLY &polygon);
  virtual void DrawIcon(wxDC* dc);
  virtual UIDialog* UI(wxWindow* parent);
  virtual void GetIPorts(POLY &iports);
  virtual void GetOPorts(POLY& ports);
  //  virtual UIDialog* Result(wxWindow* parent);
};

#endif
