#ifndef PLUGIN_BASE_H
#define PLUGIN_BASE_H
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>
#include "VE_Conductor/Framework/interface.h"
#include "VE_Conductor/Framework/UIDialog.h"
#include "VE_Conductor/Framework/TextResultDialog.h"
// EPRI TAG
#include "VE_Conductor/Framework/FinancialDialog.h"
#include "VE_Conductor/Framework/GeometryDataManager.h"
#include "VE_Conductor/Framework/GeometryDialog.h"



#define ICON 20000

#define edge_size 10

typedef std::vector<wxPoint> POLY;

class VE_GUIPLUGINS_EXPORTS REI_Plugin : public wxObject
{
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
      //Add the calls for the size. So the main program can preallocate 
      // memory for it

      virtual int GetNumPoly();
  
      virtual void GetPoly(POLY &polygon); 
      //Return the outline polygon

      virtual UIDialog* UI(wxWindow* parent);
      //This returns the UI dialog of the module

      virtual UIDialog* Result(wxWindow* parent);
      //This returns the Result dialog of the module

      virtual UIDialog* PortData(wxWindow* parent,  Interface *intf);
      //This returns the PortData dialog of the module

      virtual void FinancialData();
      //This returns the FinancialData dialog of the module

      virtual unsigned int GetID();
      //This is the ID to identify the module

      virtual wxString GetName();
      //This returns the name of the module

      virtual wxString GetDesc();
      //This returns the description of the module, This should be a short description

      virtual wxString GetHelp();
      //Return the URL of the online help

      // This launches the geometry gui for respective module
      // this is not virtual because it should always be the
      // same for each module
      /*void GeometryDialog( void );
      Geometry* GetGeometryDialog( void );*/


      void GeometryData();
      GeometryDialog* GetGeometryDialog();
      
      virtual void UnPack(Interface* intf);
      //This is the load function of the module, unpack the input string and fill up the UI according to this
      virtual Interface* Pack();

      //This is to unpack the result from the 
      virtual void UnPackResult(Interface * intf);
  
      //To Get around the Memory allocation problem of windows dll
      //Add the calls for the size. So the main program can preallocate memory for it

      virtual int GetNumIports();
      virtual void GetIPorts(POLY& ports);

      virtual int GetNumOports();
      virtual void GetOPorts(POLY& ports);

      virtual void Lock(bool lock);
      virtual void SetID(int id);
      virtual bool Has3Ddata();
      

      bool HasGeomInfoPackage();
      //void GeometryInfoPackage();
      //Geometry* GetGeometryInfoPackage();

      // EPRI TAG
      FinancialDialog* financial_dlg;
            

   protected:

      void RegistVar(std::string vname, long *var);
      void RegistVar(std::string vname, double *var);
      void RegistVar(std::string vname, std::string *var);
      void RegistVar(std::string vname, std::vector<long> *var);
      void RegistVar(std::string vname, std::vector<double> *var);
      void RegistVar(std::string vname, std::vector<std::string> *var);

      UIDialog* dlg;
      TextResultDialog* result_dlg;
      TextResultDialog* port_dlg;
      GeometryDialog* geom_dlg;
      //Geometry  _geometrypermodule;

      Interface mod_pack;
      wxPoint pos; //The Position to draw Icon;
      std::vector<wxString> v_desc;
      std::vector<wxString> v_value;

      //That's the for default implementation of the DrawIcon. Not part of the general interface
      wxPoint* poly; //The outline polygon points list;
      int n_pts; //Number of points

      std::map<std::string, long *>                      _int;
      std::map<std::string, double *>                    _double;
      std::map<std::string, std::string *>               _string;
      std::map<std::string, std::vector<long> * >        _int1D;
      std::map<std::string, std::vector<double> * >      _double1D;
      std::map<std::string, std::vector<std::string> * > _string1D;
   
   DECLARE_DYNAMIC_CLASS( REI_Plugin )
};

#endif
