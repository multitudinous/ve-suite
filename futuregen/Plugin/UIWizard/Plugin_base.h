#ifndef PLUGIN_BASE_H
#define PLUGIN_BASE_H

#include <wx/wx.h>
#include <wx/dc.h>
#include <wx/gdicmn.h>

#include "interface.h"

#include "UIDialog.h"

/*
 * If we're using wx in Dynamic Library format do we 
 * want FL to be in DLL form as well?
 */
#if defined(WXUSINGDLL) && \
    (defined(WXMAKING_PLUGIN_DLL) || defined(WXUSING_PLUGIN_DLL))

#if defined(WXMAKING_PLUGIN_DLL)
    // When building the DLL WXPLUGINDECLSPEC exports classes
#   define WXPLUGIN_DECLSPEC            WXEXPORT
#elif defined(WXUSING_PLUGIN_DLL)
    // When building the DLL WXPLUGINDECLSPEC imports classes
#   define WXPLUGIN_DECLSPEC            WXIMPORT
#endif // defined(WXBUILD_PLUGIN_DLL)

#else
// When building the static library nullify the effect of WXPLUGIN_DECLSPEC
#define WXPLUGIN_DECLSPEC
#endif // WXUSINGDLL && (WXMAKING_PLUGIN_DLL || WXUSING_PLUGIN_DLL)


#define ICON 20000

#define edge_size 10

typedef std::vector<wxPoint> POLY;

class WXPLUGIN_DECLSPEC REI_Plugin : public wxObject
{
  DECLARE_DYNAMIC_CLASS(REI_Plugin)

 public:

  REI_Plugin();
  ~REI_Plugin();

  virtual double GetVersion();
  //Return the version number of the module

  virtual void DrawIcon(wxDC* dc);
  //This call return a window to be displayed on the framework

  virtual void DrawID(wxDC* dc);

  virtual void SetPos(wxPoint pt);
  //Set the start drawing location

  virtual wxRect GetBBox();
  //Return the bounding box;
  
  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumPoly();
  
  virtual void GetPoly(POLY &polygon); 
  //Return the outline polygon

  virtual UIDialog* UI(wxWindow* parent);
  //This returns the UI dialog of the module

  virtual UIDialog* Result(wxWindow* parent);
  //This returns the Result dialog of the module

  virtual unsigned int GetID();
  //This is the ID to identify the module

  virtual wxString GetName();
  //This returns the name of the module

  virtual wxString GetDesc();
  //This returns the description of the module, This should be a short description

  virtual wxString GetHelp();
  //Return the URL of the online help

  virtual void UnPack(Interface* intf);
  //This is the load function of the module, unpack the input string and fill up the UI according to this
  virtual Interface* Pack();

  //This is to unpack the result from the 
  virtual void UnPackResult(Interface * intf) ;
  //This is the save function of the module. 

  //To Get around the Memory allocation problem of windows dll
  //Add the calls for the size. So the main program can preallocate memory for it

  virtual int GetNumIports();
  virtual void GetIPorts(POLY& ports);

  virtual int GetNumOports();
  virtual void GetOPorts(POLY& ports);

  virtual void Lock(bool lock);
  virtual void SetID(int id);

 protected:

  void RegistVar(string vname, long *var);
  void RegistVar(string vname, double *var);
  void RegistVar(string vname, std::string *var);
  void RegistVar(string vname, std::vector<long> *var);
  void RegistVar(string vname, std::vector<double> *var);
  void RegistVar(string vname, std::vector<std::string> *var);

  UIDialog* dlg;
  UIDialog* result_dlg;
  Interface mod_pack;
  wxPoint pos; //The Position to draw Icon;
  
  //That's the for default implementation of the DrawIcon. Not part of the general interface
  wxPoint* poly; //The outline polygon points list;
  int n_pts; //Number of points

  std::map<std::string, long *>                      _int;
  std::map<std::string, double *>                    _double;
  std::map<std::string, std::string *>               _string;
  std::map<std::string, std::vector<long> * >        _int1D;
  std::map<std::string, std::vector<double> * >      _double1D;
  std::map<std::string, std::vector<std::string> * > _string1D;
};

#endif
