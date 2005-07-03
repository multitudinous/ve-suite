/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdVEBaseClass.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VE_BASECLASS_H
#define CFD_VE_BASECLASS_H

#include <wx/wx.h>
#include <string>
#include <map>

//class cfdModuleGeometry;
namespace VE_SceneGraph
{
   class cfdGroup;
   class cfdDCS;
}

namespace VE_Xplorer
{
   class cfdModel;
   class cfdReadParam;
   class cfdCursor;
   class cfdNavigate;
   class cfdObjects;
}
#include "VE_Conductor/Framework/interface.h"
//#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class VE_GRAPHICALPLUGINS_EXPORTS cfdVEBaseClass: public wxObject // Inherit from wxBase class to enable string instantiation
   {
      public:
         cfdVEBaseClass( void );
         //cfdVEBaseClass( cfdDCS* );
         virtual ~cfdVEBaseClass( void );

         virtual void InitializeNode( VE_SceneGraph::cfdDCS* );
         // Methods to do scene graph manipulations
         // New methods may have to be added later
         virtual void AddSelfToSG( void );
         virtual void RemoveSelfFromSG( void );

         // Change state information for geometric representation
         void MakeTransparent( void );
         void SetColor( double* );
      
         // transform object based 
         void SetTransforms( float*, float*, float* );

         // Implement Gengxun's work by using socket
         // stuff from vtk. This will be used in parallel
         // with implementation of a unit connected to the 
         // computational engine.
         virtual void GetDataFromUnit( void );
         // Basically uses vtkActorToPF to create a geode and 
         // add it to the scene graph. Probably use cfdObject.
         virtual void MakeGeodeByUserRequest( int );

         wxString GetName();
         //This returns the name of the module

         wxString GetDesc();
         //This returns the description of the module, This should be a short description

         virtual void UnPack(Interface* intf);
         //This is the load function of the module, unpack the input string and fill up the UI according to this
         virtual Interface* Pack();

         //This is to unpack the result from the 
         void UnPackResult(Interface * intf);
         //This is the save function of the module. 

         void SetID(int id);
   
         cfdModel* GetCFDModel( void );
   
         void LoadSurfaceFiles( char* );

         bool OnSceneGraph( void ){return _onSceneGraph;}
      
         void SetCursor( cfdCursor* );

         void SetNavigate( cfdNavigate* );

         void SetInterface( Interface& );

         void SetModuleResults( const char* );

         virtual void CreateCustomVizFeature( int );

         virtual void PreFrameUpdate( void ){;}  // allows graphical plugins access to scenegraph

      private:
         // This needs to be vector of geometry nodes
         //cfdModuleGeometry*  geometryNode;
         VE_SceneGraph::cfdGroup* groupNode;

         VE_SceneGraph::cfdDCS*   worldDCS;

         wxString _objectDescription;

         char* _network;

      protected:
         void CreateObjects( void );
         long pos_x;
         long pos_y;
         // Stuff taken from Plugin_base.h
         // All of Yang's work (REI)
         void RegistVar(std::string vname, long *var);
         void RegistVar(std::string vname, double *var);
         void RegistVar(std::string vname, std::string *var);
         void RegistVar(std::string vname, std::vector<long> *var);
         void RegistVar(std::string vname, std::vector<double> *var);
         void RegistVar(std::string vname, std::vector<std::string> *var);
         VE_SceneGraph::cfdDCS* GetWorldDCS();

         Interface mod_pack;

         std::map<std::string, long *>                      _int;
         std::map<std::string, double *>                    _double;
         std::map<std::string, std::string *>               _string;
         std::map<std::string, std::vector<long> * >        _int1D;
         std::map<std::string, std::vector<double> * >      _double1D;
         std::map<std::string, std::vector<std::string> * > _string1D;

         cfdObjects* dataRepresentation;

         cfdModel* _model;
         cfdReadParam* _readParam;

         char* _param;

         bool _onSceneGraph;

         int _modID;
         wxString _objectName;
         VE_SceneGraph::cfdDCS* _dcs;
         cfdCursor* _cursor;
         cfdNavigate* _navigate;
         Interface myInterface;

         std::vector<wxString> v_desc;
         std::vector<wxString> v_value;

      DECLARE_DYNAMIC_CLASS( cfdVEBaseClass )
   };
}
#endif
