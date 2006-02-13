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
 * File:          $RCSfile: Plugin_base.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PLUGIN_BASE_H
#define PLUGIN_BASE_H

#include "VE_Conductor/Framework/interface.h"
#include <xercesc/dom/DOM.hpp>

#define ICON 20000

#include <wx/object.h>
#include <wx/icon.h>

class wxPoint;
class wxDC;
class wxRect;
class wxWindow;

#define edge_size 10
class UIDialog;
class TextResultDialog;
class TextResultDialog;
class GeometryDialog;
class FinancialDialog;
class GeometryDataBuffer;

namespace VE_Model
{
   class Model;
}

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

   virtual int GetNumIports();
   virtual void GetIPorts(POLY& ports);

   virtual int GetNumOports();
   virtual void GetOPorts(POLY& ports);

   virtual void Lock(bool lock);
   virtual void SetID(int id);
   virtual bool Has3Ddata();

   void SetIDtoGeometryDataBuffer();

   bool HasGeomInfoPackage();

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
   GeometryDataBuffer* geometryDataBuffer;

   Interface mod_pack;
   wxPoint pos; //The Position to draw Icon;

   VE_Model::Model* veModel;
   wxString name;

   //That's the for default implementation of the DrawIcon. Not part of the general interface
   wxPoint* poly; //The outline polygon points list;
   int n_pts; //Number of points

   std::vector<wxString> v_desc;
   std::vector<wxString> v_value;

   std::map<std::string, long *>                      _int;
   std::map<std::string, double *>                    _double;
   std::map<std::string, std::string *>               _string;
   std::map<std::string, std::vector<long> * >        _int1D;
   std::map<std::string, std::vector<double> * >      _double1D;
   std::map<std::string, std::vector<std::string> * > _string1D;

   DECLARE_DYNAMIC_CLASS( REI_Plugin )
};

#endif
