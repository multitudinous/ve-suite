#ifndef STACK_H
#define STACK_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

class STACK : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(STACK)

 public:
  STACK();
  
  long x;
  long y;
  long z;
  
  wxBitmap *my_icon;
  int icon_w, icon_h;

  virtual wxString GetName();
  virtual wxString GetDesc();
  virtual UIDialog* UI(wxWindow* parent);
  virtual void DrawIcon(wxDC *dc);
  virtual void GetIPorts(POLY &iports);
  virtual void GetOPorts(POLY& ports);
  //  virtual UIDialog* Result(wxWindow* parent);
};


#endif
