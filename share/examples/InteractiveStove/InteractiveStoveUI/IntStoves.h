#ifndef IntStoves_H
#define IntStoves_H

#include <ves/conductor/UIPluginBase.h>

#include <vector>

#include <wx/image.h>

using namespace std;

class IntStoves : public ves::conductor::UIPluginBase
{
  DECLARE_DYNAMIC_CLASS(IntStoves)

 public:

  IntStoves();
  virtual ~IntStoves();

  virtual double GetVersion();
  //Return the version number of the module

  virtual void DrawIcon(wxDC* dc);
  //This call return a window to be displayed on the framework
  
  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumPoly();
  
  //virtual void GetPoly(POLY &polygon); 
  //Return the outline polygon

  virtual ves::conductor::UIDialog* UI(wxWindow* parent);
  //This returns the UI dialog of the module

  virtual wxString GetName();
  //This returns the name of the module

  virtual wxString GetDesc();
  //This returns the description of the module, This should be a short description

  wxString GetConductorName();

  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumIports();
  virtual void GetIPorts(POLY& ports);

  virtual int GetNumOports();
  virtual void GetOPorts(POLY& ports);

 public:
  long numbaffles;
  vector<double> baffle1;
  vector<double> baffle2;
  vector<double> baffle3;
  vector<double> baffle4;
  vector<double> baffle5;
  vector<double> baffle6;
  vector<double> baffle7;
  //HERE is the GUI variable passed to the Dialog and Packed
  protected:
  wxBitmap *my_icon;
  int icon_w, icon_h;
  
};

#endif

