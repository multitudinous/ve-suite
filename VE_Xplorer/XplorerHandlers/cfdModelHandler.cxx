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
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

//#include <vtkPolyDataWriter.h>
#include <vtkPolyDataNormals.h>
#include <vtkPolyData.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>

#include "VE_Xplorer/Utilities/fileIO.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"
#include "VE_Xplorer/SceneGraph/CADEntity.h"

#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdVectorBase.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdReadParam.h"
#include "VE_Xplorer/XplorerHandlers/cfdScalarBarActor.h"

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
#include "VE_Xplorer/XplorerHandlers/CADTransformEH.h"
#include "VE_Xplorer/XplorerHandlers/CADAddNodeEH.h"
#include "VE_Xplorer/XplorerHandlers/CADDeleteNodeEH.h"
#include "VE_Xplorer/XplorerHandlers/CADAddAttributeEH.h"
#include "VE_Xplorer/XplorerHandlers/CADSetActiveAttributeEH.h"
#include "VE_Xplorer/XplorerHandlers/CADSetNameEH.h"
#include "VE_Xplorer/XplorerHandlers/CADSetRootNodeEH.h"
#include "VE_Xplorer/XplorerHandlers/CADToggleEH.h"
#include "VE_Xplorer/XplorerHandlers/CADRemoveAttributeEH.h"

#include "VE_Xplorer/XplorerHandlers/ActiveModelEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/MaterialUpdateEH.h"
#include "VE_Xplorer/XplorerHandlers/MaterialModeUpdateEH.h"
#include "VE_Xplorer/XplorerHandlers/AddVTKDataSetEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/BBoxEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/WireframeEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/AxesEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/AxesLabelsEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/ScalarBarEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/DataTransformEH.h"
#include "VE_Xplorer/XplorerHandlers/SoundActivateEH.h"
#include "VE_Xplorer/XplorerHandlers/SoundAddNewEH.h"

#include "VE_Open/XML/Command.h"

#ifdef _OSG
#ifdef VE_PATENTED
#include "VE_Xplorer/TextureBased/cfdTextureDataSet.h"
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"
using namespace VE_TextureBased;
#endif
#endif
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#ifndef WIN32
// Needed for irix
#include <unistd.h>
#include <sys/types.h>
#include <sys/dir.h>
#endif 

#ifdef WIN32
#include <direct.h>
#endif 

vprSingletonImp( VE_Xplorer::cfdModelHandler );
using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;

cfdModelHandler::cfdModelHandler( void )
{
   vprDEBUG(vesDBG,2) << "cfdModelHandler constructor"
                          << std::endl << vprDEBUG_FLUSH;
   _param.erase();//_param = 0;
   
   activeDataset  = 0;
   _scalarBar     = 0;
   arrow          = 0;
   _readParam     = 0;
   commandArray   = 0;
   _activeModel   = 0;
   activeCommand  = 0;

   //create null command to be returned when a command is not active
   nullCommand = new VE_XML::Command();
   nullCommand->SetCommandName( "NULL" );
   
   tbased = false;
   _eventHandlers[ std::string("SET_ROOT_CAD_NODE")] = new VE_EVENTS::CADSetRootNodeEventHandler();   
   _eventHandlers[ std::string("CAD_TRANSFORM_UPDATE")] = new VE_EVENTS::CADTransformEventHandler();   
   _eventHandlers[ std::string("CAD_ADD_NODE") ] = new VE_EVENTS::CADAddNodeEventHandler();
   _eventHandlers[ std::string("CAD_DELETE_NODE") ] = new VE_EVENTS::CADDeleteNodeEventHandler();
   _eventHandlers[ std::string("CAD_ADD_ATTRIBUTE_TO_NODE") ] = new VE_EVENTS::CADAddAttributeEventHandler();
   _eventHandlers[ std::string("CAD_SET_ACTIVE_ATTRIBUTE_ON_NODE") ] = new VE_EVENTS::CADSetActiveAttributeEventHandler();
   _eventHandlers[ std::string("CAD_SET_NODE_NAME") ] = new VE_EVENTS::CADSetNameEventHandler();
   _eventHandlers[ std::string("CAD_TOGGLE_NODE") ] = new VE_EVENTS::CADToggleEventHandler();
   _eventHandlers[ std::string("CHANGE_ACTIVE_MODEL") ] = new VE_EVENTS::ActiveModelEventHandler();
   _eventHandlers[ std::string("CAD_ATTRIBUTE_MATERIAL_UPDATE") ] = new VE_EVENTS::MaterialUpdateEventHandler();
   _eventHandlers[ std::string("CAD_ATTRIBUTE_MATERIAL_MODE") ] = new VE_EVENTS::MaterialModeUpdateEventHandler();
   _eventHandlers[ std::string("CAD_REMOVE_ATTRIBUTE") ] = new VE_EVENTS::CADRemoveAttributeEventHandler();
   _eventHandlers[ std::string("UPDATE_MODEL_DATASETS") ] = new VE_EVENTS::AddVTKDataSetEventHandler();
   _eventHandlers[ std::string("Change Bounding Box State") ] = new VE_EVENTS::BBoxEventHandler();
   _eventHandlers[ std::string("Change Wire Frame State") ] = new VE_EVENTS::WireframeEventHandler();
   _eventHandlers[ std::string("Change Axes State") ] = new VE_EVENTS::AxesEventHandler();
   _eventHandlers[ std::string("Change Axes Labels") ] = new VE_EVENTS::AxesLabelsEventHandler();
   _eventHandlers[ std::string("Change Scalar Bar State") ] = new VE_EVENTS::ScalarBarEventHandler();
   _eventHandlers[ std::string("DATA_TRANSFORM_UPDATE") ] = new VE_EVENTS::DataTransformEventHandler();
   _eventHandlers[ std::string("Enable/Disable Sound") ] = new VE_EVENTS::SoundActivateEventHandler();
   _eventHandlers[ std::string("Add New Sound") ] = new VE_EVENTS::SoundAddNewEventHandler();
#ifdef _OSG
#ifdef VE_PATENTED
   _activeTDSet = 0;
#endif
#endif
}

void cfdModelHandler::Initialize( std::string param )
{
	;
}

void cfdModelHandler::CleanUp( void )
{
   vprDEBUG(vesDBG,2) << "cfdModelHandler destructor"
                          << std::endl << vprDEBUG_FLUSH;
   delete nullCommand;
   nullCommand = 0;

   m_filenameToCADMap.clear();
   
   for( size_t i = 0; i < _modelList.size(); ++i )
   {
      delete _modelList.at( i );
   }
   _modelList.clear();

   if ( _scalarBar )
   {
      delete _scalarBar;
      vprDEBUG(vesDBG,2) << "delete _scalarBar"
         << std::endl << vprDEBUG_FLUSH;
   }

   if ( this->arrow ) 
   {
      this->arrow->Delete();
      arrow = 0;
      vprDEBUG(vesDBG,2) << "this->arrow->Delete()"
         << std::endl << vprDEBUG_FLUSH;
   }
   
   for ( std::map<std::string ,VE_EVENTS::EventHandler*>::iterator itr = _eventHandlers.begin();
                                       itr != _eventHandlers.end(); itr++ )
   {
      delete itr->second;
      itr->second = 0;
   }
   _eventHandlers.clear();
   vprDEBUG(vesDBG,2) << "cfdModelHandler end destructor"
      << std::endl << vprDEBUG_FLUSH;
}

///////////////////////
// Helper functions
///////////////////////
/////////////////////////////////////////////////////////////
void cfdModelHandler::SetCommandArray( cfdCommandArray* input )
{
   // Must be set before PreFrameUpdate is called
   commandArray = input;
}
/////////////////////////////////////////////////////////////
void cfdModelHandler::SetXMLCommand( VE_XML::Command* inputCommand )
{
   //if ( inputCommand )
   {
      activeCommand = inputCommand;
   }
   /*else
   {
      activeCommand = nullCommand;
   }*/
}
/////////////////////////////////////////////////////////////
VE_XML::Command* cfdModelHandler::GetXMLCommand( void )
{
   return activeCommand;
}
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
#ifdef _OSG
#ifdef VE_PATENTED
cfdTextureDataSet* cfdModelHandler::GetActiveTextureDataSet()
{
   return _activeTDSet;
}
#endif
#endif
/////////////////////////////////////////////////////
cfdScalarBarActor* cfdModelHandler::GetScalarBar(void)
{
   return _scalarBar;
}
/////////////////////////////////////////////////////
void cfdModelHandler::SetActiveModel( int modelNumber )
{
      for ( size_t i = 0; i < _modelList.size(); i++ )
      {
         if ( modelNumber == _modelList.at( i )->GetID() )
         {
            vprDEBUG(vesDBG,1) << "|\tcfdModelHandler::SetActiveModel : " 
                  << modelNumber 
                  << " is set." << std::endl << vprDEBUG_FLUSH;
            _activeModel = _modelList.at( i );
            break;
         }
      }
	 
	 if ( _modelList.size() == 0 )
         _activeModel = 0;
}
/////////////////////////////////////////////
cfdModel* cfdModelHandler::GetModel( int i )
{
   if ( _modelList.empty() )
      return NULL;
   else
      return _modelList.at( i );
}
/////////////////////////////////////////////////
void cfdModelHandler::AddModel( cfdModel* input )
{
   _modelList.push_back( input );
}
///////////////////////////////////////////////////////////////
void cfdModelHandler::RemoveModel( cfdModel* modelToBeRemoved )
{
   std::vector< cfdModel* >::iterator iter;
   for ( iter=_modelList.begin(); iter!=_modelList.end(); )
   {  
      if ( (*iter) == modelToBeRemoved )
      {
         //delete (*iter);
         //cfdModel* tempModel = (*iter);
         _modelList.erase( iter++ ); 
         delete modelToBeRemoved;
         //tempModel = 0;
         break;     
      }
      else
      {
         ++iter;
      }
      // The above code is from : The C++ Standard Library by:Josuttis
   }
}
/////////////////////////////////////////////////
cfdModel* cfdModelHandler::GetActiveModel( void )
{
   return _activeModel;
}
//////////////////////////////////////////////
int cfdModelHandler::GetNumberOfModels( void )
{
   return static_cast< int >( _modelList.size() ); 
}
///////////////////////////////////////////////////
vtkPolyData* cfdModelHandler::_GetArrowPolyData(  )
{    
   //ripped from cfdArrow in the Utilities/arrowCreator directory
   float shaftAngleIncrement = (3.14159265/1.5);
   float tipAngleIncrement = (3.14159265/1.5);
   int tipResolution = 3;
   int shaftResolution = 3;
   float shaftRadius = 0.03;
   float tipRadius = 0.10;
   float tipLength = 0.35;
   int numCells = 6;

   float vertex[3];
   int vertexList[4];     // make large enough for rectangles
   vtkPolyData * arrow = vtkPolyData::New();

   int memSize = numCells;
   arrow->Allocate( numCells, memSize );


   // Work on the arrow head
   int i;
   vtkPoints * points = vtkPoints::New();
   for (i=0; i<tipResolution; i++)
   {
      float angle = tipAngleIncrement*float(i);

      //generate and store points for each triangle of the arrow head
      vertex[0] = -tipLength;
      vertex[1] = tipRadius*sin( angle );
      vertex[2] = tipRadius*cos( angle );
      vertexList[0] = points->InsertNextPoint( vertex );

      vertex[0] = -tipLength;
      vertex[1] = tipRadius*sin( angle + tipAngleIncrement );
      vertex[2] = tipRadius*cos( angle + tipAngleIncrement );
      vertexList[1] = points->InsertNextPoint( vertex );

      vertex[0] = 0.0;
      vertex[1] = 0.0;
      vertex[2] = 0.0;
      vertexList[2] = points->InsertNextPoint( vertex );

      arrow->InsertNextCell( VTK_POLYGON, 3, vertexList );

      if ( tipResolution==2 )     // generate crossed polygon and exit...
      {
         vertex[0] = -tipLength;
         vertex[1] = tipRadius;
         vertex[2] = 0.0;
         vertexList[0] = points->InsertNextPoint( vertex );

         vertex[0] = -tipLength;
         vertex[1] = -tipRadius;
         vertex[2] = 0.0;
         vertexList[1] = points->InsertNextPoint( vertex );

         vertex[0] = 0.0;
         vertex[1] = 0.0;
         vertex[2] = 0.0;
         vertexList[2] = points->InsertNextPoint( vertex );

         arrow->InsertNextCell( VTK_POLYGON, 3, vertexList );
         break;
      }
   }

   // Work on the shaft
   for (i=0; i<shaftResolution; i++)
   {
      float angle = shaftAngleIncrement*float(i);

      // generate and store points for each rectangle of the shaft
      vertex[0] = -1.0;
      vertex[1] = shaftRadius*sin( angle );
      vertex[2] = shaftRadius*cos( angle );
      vertexList[0] = points->InsertNextPoint( vertex );

      vertex[0] = -tipLength;
      vertex[1] = shaftRadius*sin( angle );
      vertex[2] = shaftRadius*cos( angle );
      vertexList[1] = points->InsertNextPoint( vertex );

      vertex[0] = -tipLength;
      vertex[1] = shaftRadius*sin( angle + shaftAngleIncrement );
      vertex[2] = shaftRadius*cos( angle + shaftAngleIncrement );
      vertexList[2] = points->InsertNextPoint( vertex );

      vertex[0] = -1.0;
      vertex[1] = shaftRadius*sin( angle + shaftAngleIncrement );
      vertex[2] = shaftRadius*cos( angle + shaftAngleIncrement );
      vertexList[3] = points->InsertNextPoint( vertex );

      arrow->InsertNextCell( VTK_POLYGON, 4, vertexList );

      if ( shaftResolution==2 )   // generate crossed polygon and exit...
      {
         vertex[0] = -1.0;
         vertex[1] = shaftRadius;
         vertex[2] = 0.0;
         vertexList[0] = points->InsertNextPoint( vertex );

         vertex[0] = -tipLength;
         vertex[1] = shaftRadius;
         vertex[2] = 0.0;
         vertexList[1] = points->InsertNextPoint( vertex );

         vertex[0] = -tipLength;
         vertex[1] = -shaftRadius;
         vertex[2] = 0.0;
         vertexList[2] = points->InsertNextPoint( vertex );

         vertex[0] = -1.0;
         vertex[1] = -shaftRadius;
         vertex[2] = 0.0;
         vertexList[3] = points->InsertNextPoint( vertex );

         arrow->InsertNextCell( VTK_POLYGON, 4, vertexList );
         break;
      }
   }

   arrow->SetPoints( points );

   //the normals
   vtkPolyDataNormals * arrowPolysWithNormals = vtkPolyDataNormals::New();
   arrowPolysWithNormals->SetInput( arrow );
   //Specify the angle that defines a sharp edge. If the difference in angle across neighboring
   //polygons is greater than this value, the shared edge is considered "sharp".    
   arrowPolysWithNormals->SetFeatureAngle( 60 );
   arrowPolysWithNormals->Update();
   
   vtkPolyData* arrowPolys = vtkPolyData::New();
   arrowPolys->DeepCopy( arrowPolysWithNormals->GetOutput() );

   arrowPolysWithNormals->Delete();
   points->Delete();
   arrow->Delete();

   return arrowPolys;
}
///////////////////////////////////////
void cfdModelHandler::InitScene( void )
{

   this->arrow = _GetArrowPolyData();

   if(!arrow)
   {
      std::cerr<<"Error: cfdModelHandler::InitScene()"<<std::endl;
      std::cerr<<"Couldn't create arrow polydata!!"<<std::endl;
      exit(1);
   }
   //this->arrow->ShallowCopy( tempArrow->GetPolyData());

   for ( unsigned int j = 0; j < _modelList.size(); j++ )
      for ( unsigned int i = 0; i < _modelList.at( j )->GetNumberOfCfdDataSets(); i++)
      {
         std::cout << "|   Loading data for file " 
                << _modelList.at( j )->GetCfdDataSet( i )->GetFileName()
                << std::endl;
         _modelList.at( j )->GetCfdDataSet( i )->LoadData();
         _modelList.at( j )->GetCfdDataSet( i )->SetArrow( this->arrow );
         if ( _modelList.at( j )->GetCfdDataSet( i )->GetParent() == _modelList.at( j )->GetCfdDataSet( i ) )
            VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->
               AddChild( _modelList.at( j )->GetCfdDataSet( i )->GetDCS() );
      }

   // set default active dataset to be the meshed volume
   if ( !_modelList.empty() )
   {
      _activeModel = _modelList.at(0);
      if ( _modelList.at( 0 )->GetNumberOfCfdDataSets() > 0 )
      {
         activeDataset = _modelList.at( 0 )->GetCfdDataSet( 0 );
         _activeModel->SetActiveDataSet( activeDataset );
      }
#ifdef _OSG
#ifdef VE_PATENTED
      if(_modelList.at(0)->GetNumberOfTextureDataSets()>0)
      {
         _activeTDSet = _modelList.at(0)->GetTextureDataSet(0);
         _activeModel->SetActiveTextureDataSet(_activeTDSet);
      }
#endif
#endif
   }

   if ( activeDataset != NULL )
   {
      // Fix this later - we need to check and see if this is already 
      // done in cfdDataSet upon initialization
      // set first scalar active
      activeDataset->SetActiveScalar( 0 );
   
      oldDatasetName.assign( activeDataset->GetFileName() );//strcpy( oldDatasetName, activeDataset->GetFileName() );
      vprDEBUG(vesDBG,1) << "cfdModelHandler: Setting active dataset to " 
               << activeDataset->GetFileName() << " , " 
               << oldDatasetName << std::endl << vprDEBUG_FLUSH;
   }

   std::cout << "|  57. Initializing................................. Create Scalar Bar |" << std::endl;
   // Create Scalar bar
	//This code is broken due to the get parent call
	_scalarBar = new cfdScalarBarActor( _param, //dynamic_cast< VE_SceneGraph::Group* >
												 VE_SceneGraph::SceneManager::instance()->GetRootNode() );
   // Assumes active dataset isn't null
   _scalarBar->RefreshScalarBar();
}

/////////////////////////////////
// PreFrameUpdate - Be sure to set the commandArray before calling this
// function.
/////////////////////////////////
void cfdModelHandler::PreFrameUpdate( void )
{
   bool updateScalarRange = false;
   std::map<std::string,VE_EVENTS::EventHandler*>::iterator currentEventHandler;
   if( activeCommand )
   {
      vprDEBUG(vesDBG,3) << "Command Name : "<< activeCommand->GetCommandName() <<std::endl<< vprDEBUG_FLUSH;;
      currentEventHandler = _eventHandlers.find( activeCommand->GetCommandName() );
      if(currentEventHandler != _eventHandlers.end())
      {
         vprDEBUG(vesDBG,1) << "Executing: "<< activeCommand->GetCommandName() <<std::endl<< vprDEBUG_FLUSH;;
         currentEventHandler->second->SetGlobalBaseObject();
         currentEventHandler->second->Execute( activeCommand );
      }
   }
   
   // Check and see if we need to refresh the scalar bar
   _scalarBar->CheckCommandId( commandArray );
   // May use in the future
   //_scalarBar->UpdateCommand();
}

///////////////////////////////////////////////
// Used to initialize data for the simulation
///////////////////////////////////////////////
void cfdModelHandler::LoadSurfaceFiles( std::string precomputedSurfaceDir )
{
   if ( precomputedSurfaceDir.empty() )// == NULL )
   {
      vprDEBUG(vesDBG,1) << "precomputedSurfaceDir == NULL" 
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   vprDEBUG(vesDBG,1) << "Loading surface files from " 
      << precomputedSurfaceDir << std::endl << vprDEBUG_FLUSH;


   boost::filesystem::path dir_path( precomputedSurfaceDir );

   if ( boost::filesystem::is_directory( dir_path ) )
   {
      std::cout << "\nIn directory: "
              << dir_path.native_directory_string() << "\n\n";
      boost::filesystem::directory_iterator end_iter;
      for ( boost::filesystem::directory_iterator dir_itr( dir_path );
            dir_itr != end_iter; ++dir_itr )    
      {
         try
         {
            if ( boost::filesystem::is_directory( *dir_itr ) )
            {
               std::cout << dir_itr->leaf()<< " [directory]\n";
            }
            else
            {
               std::cout << dir_itr->leaf() << "\n";
               if ( strstr( dir_itr->leaf().c_str(), ".vtk") )
               {
                  std::string pathAndFileName;// = new char[strlen(dir_path.leaf().c_str() )+
                  //                        strlen(dir_itr->leaf().c_str())+2];
                  pathAndFileName.assign( dir_path.leaf().c_str() );//strcpy(pathAndFileName,dir_path.leaf().c_str());
                  pathAndFileName.append( "/" );//strcat(pathAndFileName,"/");
                  pathAndFileName.append( dir_itr->leaf().c_str() );//strcat(pathAndFileName,dir_itr->leaf().c_str());

                  vprDEBUG(vesDBG,0) << "\tsurface file = " << pathAndFileName
                                         << std::endl << vprDEBUG_FLUSH;

                  _modelList.at( 0 )->CreateCfdDataSet();
                  unsigned int numDataSets = _modelList.at( 0 )->GetNumberOfCfdDataSets();
                  // subtract 1 because this number was 1 base not 0 base
                  numDataSets -= 1;
                  _modelList.at( 0 )->GetCfdDataSet( -1 )->SetFileName( pathAndFileName );

                  // set the dcs matrix the same as the last file
                  _modelList.at( 0 )->GetCfdDataSet( -1 )->SetDCS( 
                                    _modelList.at( 0 )->GetCfdDataSet( (int)(numDataSets-1) )->GetDCS() ); 

                  // precomputed data that descends from a flowdata.vtk should
                  // automatically have the same color mapping as the "parent" 
                  _modelList.at( 0 )->GetCfdDataSet( -1 )->SetParent( 
                                    _modelList.at( 0 )->GetCfdDataSet( (int)(numDataSets-1) )->GetParent() );
               }
            }
         }
         catch ( const std::exception & ex )
         {
            std::cout << dir_itr->leaf() << " " << ex.what() << std::endl;
         }
      }
   }
   else // must be a file
   {
      std::cout << "\nFound: " << dir_path.native_file_string() << "\n";    
   }
}
////////////////////////////////////////////////////////////////////////////////
bool cfdModelHandler::GetVisOption()
{
   return tbased;
}
////////////////////////////////////////////////////////////////////////////////
vtkPolyData* cfdModelHandler::GetArrow( void )
{
   return this->arrow;
}
////////////////////////////////////////////////////////////////////////////////
void cfdModelHandler::RegisterCADFile( VE_SceneGraph::CADEntity* tempEntity )
{
    m_filenameToCADMap.insert( 
        std::pair< std::string, VE_SceneGraph::CADEntity* >( 
        tempEntity->GetFilename(), tempEntity ) );
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::CADEntity* cfdModelHandler::IsCADFileLoaded( std::string filename )
{
    std::multimap< std::string, VE_SceneGraph::CADEntity* >::iterator iter;
    iter = m_filenameToCADMap.find( filename );
    if ( iter != m_filenameToCADMap.end() )
    {
        return iter->second;
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void cfdModelHandler::UnregisterCADFile( VE_SceneGraph::CADEntity* tempEntity )
{
    std::multimap< std::string, VE_SceneGraph::CADEntity* >::iterator iter;
    for( iter = m_filenameToCADMap.begin(); iter != m_filenameToCADMap.end(); ++iter )
    {
        if( iter->second == tempEntity )
        {
            m_filenameToCADMap.erase( iter );
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
