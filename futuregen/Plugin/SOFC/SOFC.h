#ifndef SOFC_H
#define SOFC_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)


class SOFC : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(SOFC)

 public:

  SOFC();
  ~SOFC();

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

 public:
  double a_thickness;
  double c_thickness;
  double e_thickness;
  double a_A;
  double a_E;
  double c_A;
  double c_E;
  double e_A;
  double e_E;
  double cell_area;
  double num_cell;
  double fuel_util;
  double press_drop;
  //HERE is the GUI variable passed to the Dialog and Packed
  
};

#endif

