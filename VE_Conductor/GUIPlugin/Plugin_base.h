/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PLUGIN_BASE_H
#define PLUGIN_BASE_H
#include "VE_Open/skel/VjObsC.h"
//#include <xercesc/dom/DOM.hpp>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/XML/Model/Port.h"

#include "VE_Conductor/Network/interface.h"
#include <vector>
#include <map>

#define ICON 20000

#include <wx/object.h>
#include <wx/icon.h>



class wxPoint;
class wxDC;
class wxRect;
class wxWindow;
class wxDialog;

#define edge_size 10
class UIDialog;
class TextResultDialog;
class TextResultDialog;
class GeometryDialog;
class FinancialDialog;
class GeometryDataBuffer;
class SummaryResultDialog;

namespace VE_Model
{
   class Model;
   class Port;
}

namespace VE_XML
{
   class Command;
}

class Interface;

typedef std::vector< wxPoint > POLY;
typedef std::vector< VE_Model::Port > PORT;

class VE_GUIPLUGINS_EXPORTS REI_Plugin : public wxObject
{
public:
   REI_Plugin();
   virtual ~REI_Plugin();

   virtual double GetVersion();
   //Return the version number of the module

   virtual void DrawIcon(wxDC* dc);
   //This call return a window to be displayed on the framework

   void DrawID(wxDC* dc);

   void SetPos(wxPoint pt);
   //Set the start drawing location

   wxRect GetBBox();
   //Return the bounding box;

   //To Get around the Memory allocation problem of windows dll
   //Add the calls for the size. So the main program can preallocate 
   // memory for it

   int GetNumPoly( void );

   void GetPoly(POLY &polygon); 
   //Return the outline polygon

   virtual UIDialog* UI(wxWindow* parent);
   //This returns the UI dialog of the module

   virtual UIDialog* Result(wxWindow* parent);
   //This returns the Result dialog of the module

   virtual UIDialog* PortData(wxWindow* parent,  Interface *intf);
   //This returns the PortData dialog of the module

   virtual void FinancialData();
   //This returns the FinancialData dialog of the module

   unsigned int GetID();
   //This is the ID to identify the module

   virtual wxString GetName();
   //This returns the name of the module

   virtual void SetName( wxString pluginName );
   //This sets the name of the module

   virtual wxString GetDesc();
   //This returns the description of the module, This should be a short description

   virtual wxString GetHelp();
   //Return the URL of the online help

   void GeometryData();
   GeometryDataBuffer* GetGeometryDataBuffer( void );

   virtual void UnPack(Interface* intf);
   //This is the load function of the module, unpack the input string and fill up the UI according to this
   virtual Interface* Pack();
   //This is the load function of the module, unpack the input string and fill up the UI according to this
   VE_Model::Model* GetVEModel( void );
   void SetVEModel( VE_Model::Model* tempModel );
   VE_Model::Model* GetModel( void );

   //This is to unpack the result from the 
   virtual void UnPackResult(Interface * intf);

   //To Get around the Memory allocation problem of windows dll
   //Add the calls for the size. So the main program can preallocate memory for it

   int GetNumIports();
   void SetNumIports( int numPorts );
   virtual void GetIPorts(PORT& ports);

   int GetNumOports();
   void SetNumOports( int numPorts );
   virtual void GetOPorts(PORT& ports);

   virtual void Lock(bool lock);
   void SetID(int id);
   virtual bool Has3Ddata();
   // EPRI TAG
   FinancialDialog* financial_dlg;

   // virtual functions to launch custom input dialogs
   virtual void ViewInputVariables( void );
   virtual void ViewResultsVariables( void );
   
   ///Launches the geometry dialogs
   //void ViewCADInfo( VjObs_ptr vjObjs );

protected:
   void GetDataTables( VE_XML::Command* inputCommand, 
                        std::vector< wxString >& tagNames, 
                        std::vector< wxString >& values );

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
   GeometryDataBuffer* geometryDataBuffer;

   Interface mod_pack;
   wxPoint pos; //The Position to draw Icon;

   VE_Model::Model* veModel;
   wxString name;

   //That's the for default implementation of the DrawIcon. Not part of the general interface
   wxPoint* poly; //The outline polygon points list;
   int n_pts; //Number of points

   ///Port data info
   int numberOfInputPorts;
   int numberOfOutputPorts;
   std::vector< VE_Model::Port* > inputPort;
   std::vector< VE_Model::Port* > outputPort;
   
   //data storage types
   std::vector< wxString > v_desc;
   std::vector< wxString > v_value;

   std::map<std::string, long *>                      _int;
   std::map<std::string, double *>                    _double;
   std::map<std::string, std::string *>               _string;
   std::map<std::string, std::vector<long> * >        _int1D;
   std::map<std::string, std::vector<double> * >      _double1D;
   std::map<std::string, std::vector<std::string> * > _string1D;

   // Dynamic input and results dialogs as well as port dialogs
   //wxDialog* inputsDialog;
   SummaryResultDialog* resultsDialog;
   wxDialog* portsDialog;
   SummaryResultDialog* inputsDialog;

   VjObs_var vjObsPtr;///<The VjObj ptr.

   DECLARE_DYNAMIC_CLASS( REI_Plugin )
};

#endif
