#ifndef SOFC1D_H
#define SOFC1D_H

#include "Plugin_base.h"
#include "wx/image.h"

#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)

#include <vector>
#include <string>
using namespace std;
class SOFC1D : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(SOFC1D)

 public:

  SOFC1D();
  ~SOFC1D();

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
  virtual wxString GetHelp();
 public:
  double a_height;
  double a_width;
  double a_space;
  double a_ecd;
  double a_tcoeff;
  double a_thick;
  double a_presdrop;
  double c_height;
  double c_width;
  double c_space;
  double c_ecdb;
  double c_ecdm;
  double c_tcoeffb;
  double c_tcoeffm;
  double c_thick;
  double c_presdrop;
  double s_thick;
  double s_heatcap;
  double s_density;
  double e_thick;
  double e_preexp;
  double e_exp;
  double f_preexp;
  double f_exp;
  double a_preexp;
  double a_exp;
  double i_preexp;
  double i_exp;
  double l_heatcap;
  double l_density;
  double l_length;
  double l_width;
  double stop_time;
  double loadres;
  string work_dir;
  long l_numCells;
  long ax_nodes;
  //HERE is the GUI variable passed to the Dialog and Packed
 protected:
  wxBitmap *my_icon;
  int icon_w, icon_h;       
};

#endif

