#ifndef SampleMFC_Gauges_H
#define SampleMFC_Gauges_H

#include "VE_Conductor/Framework/Plugin_base.h"
#include <wx/image.h>

#ifdef WIN32
   #pragma warning(disable : 4786)
   #pragma warning(disable : 4101)
   #pragma warning(disable : 4503)
#endif

class SampleMFC_Gauges : public REI_Plugin
{
  DECLARE_DYNAMIC_CLASS(SampleMFC_Gauges)

 public:

  SampleMFC_Gauges();
  ~SampleMFC_Gauges();

  virtual double GetVersion();
  //Return the version number of the module

  virtual void DrawIcon(wxDC* dc);
  //This call return a window to be displayed on the framework
  
  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumPoly();
  
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
  double dbl1;
  double dbl2;
  long int1;
	long int2;
	std::vector<double> dbllist;
  //HERE is the GUI variable passed to the Dialog and Packed
	protected:
  wxBitmap *my_icon;
  int icon_w, icon_h;
  
};

#endif

