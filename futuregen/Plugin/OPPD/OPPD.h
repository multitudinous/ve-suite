#ifndef OPPD_H
#define OPPD_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)


class OPPD : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(OPPD)

 public:

  OPPD();
  ~OPPD();

  virtual double GetVersion();
  //Return the version number of the module

  virtual void DrawIcon(wxDC* dc);
  //This call return a window to be displayed on the framework
  
  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumPoly();
  
  //virtual void GetPoly(POLY &polygon); 
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

 public:
  double intlinthickdbl;
  double massfuelburndbl;
  double solidfuelareadbl;
  double evalabvfiredbl;
  double cblburnareadbl;
  long tempmethod;
  long tempcalcmethod;
  long detectortype;
  long flametype;
  long detacttemp;
  long matselindex;
  long fuelselindex;
  long vismatselindex;
  long durmatselindex;
  long vispropselindex;
  long viscombselindex;
  long detrtiselindex;
  long dettempratselindex;
  long detspaceselindex;
  long cableselindex;
  long killexcel;
  vector<double> fuelpardbls;
  vector<double> compardbls;
  vector<double> ambpardbls;
  vector<double> ventpardbls;
  vector<double> detectpardbls;
  //HERE is the GUI variable passed to the Dialog and Packed
  protected:
  wxBitmap *my_icon;
  int icon_w, icon_h;
};

#endif

