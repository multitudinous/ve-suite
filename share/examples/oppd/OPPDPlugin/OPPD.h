#ifndef OPPD_H
#define OPPD_H

#include "VE_Conductor/Framework/Plugin_base.h"
#include "wx/image.h"
#include <vector>

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

using namespace std;

class OPPD : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(OPPD)

 public:

  OPPD();
  virtual ~OPPD();

  virtual double GetVersion();
  //Return the version number of the module

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

  virtual UIDialog* Result(wxWindow* parent);

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
  vector<double> fv1thicktime;
  vector<double> fv1thicktemp;
  vector<double> fv2thicktime;
  vector<double> fv2thicktemp;
  vector<double> nvthicktime;
  vector<double> nvthicktemp;
  double tsec;
  double tmin;
  double hrrkw;
  double hrrbtu;
  double detsprinktime;
  double detsmtime;
  double detfthtime;
  double flwallinehgt;
  double flcornerhgt;
  double flwallhgt;
  double hrrhrr;
  double hrrburndur;
  double hrrhgthesk;
  double hrrhgtthom;
  double pltemp;
  double tcltemp;
  double visdist;
  double fv1thintemp;
  double fv2thintemp;
  double nvthintemp;
};

#endif

