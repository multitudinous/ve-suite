#ifndef Gasifier_H
#define Gasifier_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)


class Gasifier : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(Gasifier)

 public:

  Gasifier();
  ~Gasifier();

  virtual double GetVersion();
  //Return the version number of the module

  virtual void DrawIcon(wxDC* dc);
  //This call return a window to be displayed on the framework
  
  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumPoly();
  
  //  virtual void GetPoly(POLY &polygon); 
  //Return the outline polygon

  virtual UIDialog* UI(wxWindow* parent);
  //This returns the UI dialog of the module

  virtual wxString GetName();
  //This returns the name of the module

  virtual wxString GetDesc();
  //This returns the description of the module, This should be a short description


  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumIports();
  virtual void GetIPorts(POLY& ports);

  virtual int GetNumOports();
  virtual void GetOPorts(POLY& ports);
  virtual bool Has3Ddata();

 public:
  double steam_temp1;
  double steam_flrt1;
  double slurry_temp1;
  double slurry_flrt1;
  double coal_percent1;
  double char_percent1;
  double steam_temp2;
  double steam_flrt2;
  double slurry_temp2;
  double slurry_flrt2;
  double coal_percent2;
  double char_percent2;
  double steam_temp3;
  double steam_flrt3;
  double slurry_temp3;
  double slurry_flrt3;
  double coal_percent3;
  double char_percent3;
  double size_50;
  double size_200;
  double pres_drop;
  string coal_type;
  long stage;

  //HERE is the GUI variable passed to the Dialog and Packed
 protected:
  wxBitmap *my_icon;
  int icon_w, icon_h;  
};

#endif

