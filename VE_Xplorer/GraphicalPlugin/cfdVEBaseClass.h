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
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VE_BASECLASS_H
#define CFD_VE_BASECLASS_H

#include <string>
#include <vector>
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
   class cfdSoundHandler;
}

namespace VE_Model
{
   class Model;
}

#include "VE_Installer/include/VEConfig.h"
namespace VE_Xplorer
{
class VE_GRAPHICALPLUGINS_EXPORTS cfdVEBaseClass
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

   std::string GetName();
   //This returns the name of the module

   std::string GetDesc();
   //This returns the description of the module, This should be a short description

   void SetID(int id);

   cfdModel* GetCFDModel( void );

   void LoadSurfaceFiles( std::string );

   bool OnSceneGraph( void ){return _onSceneGraph;}

   void SetCursor( cfdCursor* );

   void SetNavigate( cfdNavigate* );

   void SetSoundHandler( cfdSoundHandler* );

   void SetModuleResults( const std::string );

   void SetObjectName( std::string );
   virtual void CreateCustomVizFeature( int );

   ///This function gets called if the model is selected
   virtual void SelectedPreFrameUpdate( void ){;}  // allows graphical plugins access to scenegraph

   ///This gets called every frame no matter what
   virtual void PreFrameUpdate( void ){;}  // allows graphical plugins access to scenegraph

   ///Set the VE_Model to be used by this plugin
   ///\param tempModel Pointer to VE_Model
   void SetXMLModel( VE_Model::Model* tempModel );
   
private:
   // This needs to be vector of geometry nodes
   //cfdModuleGeometry*  geometryNode;
   VE_SceneGraph::cfdGroup* groupNode;

   VE_SceneGraph::cfdDCS*   worldDCS;

   std::string _objectDescription;

   std::string _network;

protected:
   void CreateObjects( void );
   long pos_x;
   long pos_y;

   VE_SceneGraph::cfdDCS* GetWorldDCS();

   std::map<std::string, long *>                      _int;
   std::map<std::string, double *>                    _double;
   std::map<std::string, std::string *>               _string;
   std::map<std::string, std::vector<long> * >        _int1D;
   std::map<std::string, std::vector<double> * >      _double1D;
   std::map<std::string, std::vector<std::string> * > _string1D;

   cfdObjects* dataRepresentation;

   cfdModel* _model;
   cfdReadParam* _readParam;

   std::string _param;

   bool _onSceneGraph;

   int _modID;
   std::string _objectName;
   VE_SceneGraph::cfdDCS* _dcs;
   cfdCursor* _cursor;
   cfdNavigate* _navigate;
   cfdSoundHandler* soundHandler;
   
   VE_Model::Model* xmlModel;
   std::vector< std::string > v_desc;
   std::vector< std::string > v_value;
};
}

#define VE_GRAPHICALPLUGIN_CLASS(name)  \
   extern "C" \
   { \
      VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin() \
      { \
         return new name(); \
      } \
   }

#endif
