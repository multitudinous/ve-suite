#ifndef Gasifier0D_H
#define Gasifier0D_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

#include <vector>
#include <string>
using namespace std;

class Gasifier0D : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(Gasifier0D)

 public:

  Gasifier0D();
  ~Gasifier0D();

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
  virtual wxString GetHelp();
 public:
  double steam_temp1;
  double steam_flrt1;
  double slurry_temp1;
  double slurry_flrt1;
  double coal_percent1;
  double steam_temp2;
  double steam_flrt2;
  double slurry_temp2;
  double slurry_flrt2;
  double coal_percent2;
  double steam_temp3;
  double steam_flrt3;
  double slurry_temp3;
  double slurry_flrt3;
  double coal_percent3;
  double geo_diam;
  double geo_stage1_len;
  double geo_stage2_len;
  double geo_stage1_wall;
  double geo_stage2_wall;
  double burn_out;
  double stage1_heatloss;
  double stage2_heatloss;
  double LD_ratio;
  double stage1_emis;
  double stage2_emis;
  double backside_temp;
  double slag_eff;
  double pres_drop;
  long stage;
  long spec_geometry;
  long des_mode;
  std::string coal_type;
  double size_50;
  double size_200;
  //HERE is the GUI variable passed to the Dialog and Packed
 protected:
  wxBitmap *my_icon;
  int icon_w, icon_h;     
};

#endif

