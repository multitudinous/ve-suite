#ifndef ChlorineBed_H
#define ChlorineBed_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)


class ChlorineBed : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(ChlorineBed)

 public:

  ChlorineBed();
  ~ChlorineBed();

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
  double Temp_change;
  double HCL_eff;
  double HCL_ppm;
  double Pres_drop;
  double Bed_diameter;
  double Bed_depth;
  double Bed_void_frac;
  double Particle_size;
  double Particle_sphericity;
  long rHCL_eff_ppm;
  long rPresDrop_spec_calc;
  //HERE is the GUI variable passed to the Dialog and Packed
  
};

#endif

