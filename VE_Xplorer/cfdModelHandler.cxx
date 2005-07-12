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
 * File:          $RCSfile: cfdModelHandler.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/cfdModelHandler.h"

#include <vrj/Util/Debug.h>

#include <vtkPolyDataReader.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>

#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdVectorBase.h"
#include "VE_Xplorer/cfdCommandArray.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdReadParam.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_Xplorer/cfdScalarBarActor.h"
#include "VE_SceneGraph/cfdTempAnimation.h"
#include "VE_SceneGraph/cfdSwitch.h"
#include "VE_SceneGraph/cfdPfSceneManagement.h"
#include "VE_SceneGraph/cfdGroup.h"

#ifdef _OSG
#ifdef VE_PATENTED
#include "VE_TextureBased/cfdTextureDataSet.h"
#include "VE_TextureBased/cfdTextureManager.h"
using namespace VE_TextureBased;
#endif
#endif
#include <fstream>
#include <string>
#include <sstream>

#ifdef WIN32   // windows
#include <direct.h>
#else          // not windows
// Needed for irix
#include <unistd.h>
#include <sys/types.h>
#include <sys/dir.h>
#endif // WIN32

vprSingletonImp( VE_Xplorer::cfdModelHandler );
using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;

cfdModelHandler::cfdModelHandler( void )
{
   vprDEBUG(vprDBG_ALL,2) << "cfdModelHandler constructor"
                          << std::endl << vprDEBUG_FLUSH;
   _param = 0;
   
   this->activeDataset  = 0;
   this->_scalarBar     = 0;
   this->arrow          = 0;
   this->_readParam     = 0;
   this->commandArray   = 0;
   this->_activeModel   = 0;
   tbased = false;
#ifdef _OSG
#ifdef VE_PATENTED
   _activeTDSet = 0;
#endif
#endif
}

void cfdModelHandler::Initialize( char* param )
{
   _param = param;
   _readParam = new cfdReadParam();
   CreateObjects();
}

void cfdModelHandler::CleanUp( void )
{
   vprDEBUG(vprDBG_ALL,2) << "cfdModelHandler destructor"
                          << std::endl << vprDEBUG_FLUSH;

   for( std::vector< cfdModel* >::iterator itr = _modelList.begin();
                                           itr != _modelList.end(); itr++)
   {
      delete *itr;
      // This code probably doesn't work
      // Please see code in cfdExecutive dealing with maps
   }
   _modelList.clear();

   if ( _scalarBar ) 
      delete _scalarBar;

   if ( this->arrow ) 
      this->arrow->Delete();
}

///////////////////////
// Helper functions
///////////////////////
void cfdModelHandler::SetCommandArray( cfdCommandArray* input )
{
   // Must be set before PreFrameUpdate is called
   commandArray = input;
}
#ifdef _OSG
#ifdef VE_PATENTED
/////////////////////////////////////////////////////////////
cfdTextureDataSet* cfdModelHandler::GetActiveTextureDataSet()
{
   return _activeTDSet;
}
#endif
#endif
/////////////////////////////////////////////////////
cfdDataSet* cfdModelHandler::GetActiveDataSet( void )
{
   return activeDataset;
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
         delete (*iter);
         _modelList.erase( iter++ ); 
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
   return (int)_modelList.size(); 
}
///////////////////////////////////////
void cfdModelHandler::InitScene( void )
{
   // Locate and load the arrow file...
   char * arrowFile = fileIO::GetFile( "arrow", "/VE_Xplorer/data/arrow.vtk" );
   if ( arrowFile == NULL )
   {
      std::cerr << "ERROR: The vtkPolyData arrow file could not be found\n"
                << "       Please create one and put it in the data dir"
                << std::endl;
      exit( 1 );
   }

   vtkPolyDataReader * arrowReader = vtkPolyDataReader::New();
   arrowReader->SetFileName( arrowFile );
   arrowReader->Update();

   this->arrow = vtkPolyData::New();
   this->arrow->ShallowCopy( arrowReader->GetOutput() );
   arrowReader->Delete();
   delete [] arrowFile;

   for ( unsigned int j = 0; j < _modelList.size(); j++ )
      for ( unsigned int i = 0; i < _modelList.at( j )->GetNumberOfCfdDataSets(); i++)
      {
         std::cout << "|   Loading data for file " 
                << _modelList.at( j )->GetCfdDataSet( i )->GetFileName()
                << std::endl;
         _modelList.at( j )->GetCfdDataSet( i )->LoadData();
         _modelList.at( j )->GetCfdDataSet( i )->SetArrow( this->arrow );
         if ( _modelList.at( j )->GetCfdDataSet( i )->GetParent() == _modelList.at( j )->GetCfdDataSet( i ) )
            VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->
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
   
      strcpy( oldDatasetName, activeDataset->GetFileName() );
      vprDEBUG(vprDBG_ALL,1) << "cfdModelHandler: Setting active dataset to " 
               << activeDataset->GetFileName() << " , " 
               << oldDatasetName << std::endl << vprDEBUG_FLUSH;
   }

   std::cout << "|  57. Initializing................................. Create Scalar Bar |" << std::endl;
   // Create Scalar bar
   _scalarBar = new cfdScalarBarActor( _param, (VE_SceneGraph::cfdGroup*)VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->GetParent( 0 ) );
   // Assumes active dataset isn't null
   _scalarBar->SetActiveDataSet( activeDataset );
   _scalarBar->RefreshScalarBar();
}

/////////////////////////////////
// PreFrameUpdate - Be sure to set the commandArray before calling this
// function.
/////////////////////////////////
void cfdModelHandler::PreFrameUpdate( void )
{
   bool updateScalarRange = false;
   
   if ( commandArray == NULL )
   {
      std::cerr << "ERROR: commandArray not set for cfdModelHandler"
                << std::endl;
      exit( 1 );
   }

   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
            == CHANGE_STEADYSTATE_DATASET )
   {
      unsigned int i = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      vprDEBUG(vprDBG_ALL,1) 
         << "CHANGE_STEADYSTATE_DATASET " << i 
         //<< ", scalarIndex = " << this->cfdSc
         //<< ", min = " << this->cfdMin 
         //<< ", max = " << this->cfdMax
         << std::endl << vprDEBUG_FLUSH;

      //update active texture dataset if it exists
#ifdef _OSG
#ifdef VE_PATENTED
      unsigned int nTextureDataSets = _activeModel->GetNumberOfTextureDataSets();
      if( (nTextureDataSets) && ( i < nTextureDataSets ) )
      {
         _activeTDSet = _activeModel->GetTextureDataSet(i);
         _activeModel->SetActiveTextureDataSet(_activeTDSet);
      }else{
         _activeTDSet = 0;
      }
#endif
#endif
      if ( ( i < _activeModel->GetNumberOfCfdDataSets() ) )
      {
         vprDEBUG(vprDBG_ALL,0) << "\tcfdModelHandler::PreFrameUpdate dataset = "
                  << _activeModel->GetCfdDataSet( i )->GetFileName()
                  << ", dcs = " << _activeModel->GetCfdDataSet( i )->GetDCS()
                  << std::endl << vprDEBUG_FLUSH;

         int cfdType = _activeModel->GetCfdDataSet( i )->GetType();
         vprDEBUG(vprDBG_ALL,1) << "\tcfdModelHandler::PreFrameUpdate cfdType: " << cfdType
                             << std::endl << vprDEBUG_FLUSH;

         // set the dataset as the appropriate dastaset type
         // (and the active dataset as well)
         activeDataset = _activeModel->GetCfdDataSet( i );         
         _activeModel->SetActiveDataSet( activeDataset );         
      
         vprDEBUG(vprDBG_ALL,1) << "\tcfdModelHandler::PreFrameUpdate last active dataset name = " 
                             << oldDatasetName
                             << std::endl << vprDEBUG_FLUSH;

         vprDEBUG(vprDBG_ALL,1) << "\tcfdModelHandler::PreFrameUpdate Activating steady state file " 
                << activeDataset->GetFileName()
                << std::endl << vprDEBUG_FLUSH;

         // make sure that the user did not just hit same dataset button
         // (or change scalar since that is routed through here too)
         if ( strcmp( oldDatasetName, activeDataset->GetFileName() ) )
         {
            vprDEBUG(vprDBG_ALL,1) << "\tcfdModelHandler::PreFrameUpdate  setting dataset as newly activated" 
                                << std::endl << vprDEBUG_FLUSH;
            activeDataset->SetNewlyActivated();
            strcpy( oldDatasetName, activeDataset->GetFileName() );
         }

         // update scalar bar for possible new scalar name
         updateScalarRange = true;
         // Set the current active dataset for the scalar bar
         // so that it knows how to update itself
         _scalarBar->SetActiveDataSet( activeDataset );
      }
      else
      {
         std::cerr << "ERROR: cfdModelHandler::PreFrameUpdate  requested steady state dataset " 
                  << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) << " must be less than " 
                  << _activeModel->GetNumberOfCfdDataSets()
                  << std::endl;
      }
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
               == CHANGE_VECTOR )
   { 
      int vectorIndex = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_VECTOR, vectorIndex = " << vectorIndex
                             << std::endl << vprDEBUG_FLUSH;

      activeDataset->SetActiveVector( vectorIndex );
      //activeDataset->GetParent()->SetActiveVector( vectorIndex );
#ifdef _OSG
#ifdef VE_PATENTED
      if(_activeModel !=0)
      {
         if(_activeTDSet)
         {
            _activeTDSet->SetActiveVector(activeDataset->GetVectorName(vectorIndex));
         }
      }
#endif
#endif
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
               == UPDATE_GEOMETRY )
   {
      if ( commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE ) )
      {
         vprDEBUG(vprDBG_ALL,1) << " CommandArray Geostate Value = " 
            << commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE )
            << std::endl << vprDEBUG_FLUSH;

         long int test = this->_readParam->convertDecimalToBinary( (long)
                        commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE ) );

         vprDEBUG(vprDBG_ALL,1) << " Return from conversion : " << test
            << " : Active Model Ptr = " << _activeModel
            << std::endl << vprDEBUG_FLUSH;

         this->_readParam->convertBinaryToArray( test, _activeModel->GetNumberOfGeomDataSets() );

         // Update Scene Graph with selected or deselected geometry
         vprDEBUG(vprDBG_ALL,0) << " Change Geometry in Scene Graph."
                          << std::endl << vprDEBUG_FLUSH;

         VE_SceneGraph::cfdDCS* parent = NULL;
         for( unsigned int i = 0; i < _activeModel->GetNumberOfGeomDataSets(); i++ )
         {
            int temp = 0;
            // If it is the dynamic stuff
            // Remember for the dynamic stuff there is multple layers of nodes
            // therefore we must find the parent besides worldDCS
            // because for search child we only search the immediate children
            // maybe come up with better functionality later
            if (  !strcmp( _activeModel->GetCfdDCS()->GetName(), "cfdVEBaseClass" ) )
            {
               parent = _activeModel->GetCfdDCS();
            }
            else
            {
               parent = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS();
            }

            vprDEBUG(vprDBG_ALL,2)
               << "guiVal[ " << i << " ] = " << this->_readParam->guiVal[ i ] << std::endl
               << " : Active Model index : " << _activeModel << std::endl 
               << " : SearchChild Result : " << parent->SearchChild( _activeModel->GetGeomDataSet( i )->getpfDCS() ) << std::endl 
               << " : Filename[ " << i << " ] : " << _activeModel->GetGeomDataSet( i )->GetFilename() << std::endl
               << vprDEBUG_FLUSH;

            if ( ( this->_readParam->guiVal[ i ] == 1 ) && 
               ( parent->SearchChild( _activeModel->GetGeomDataSet( i )->getpfDCS() ) == -1 ) )
            {
               temp = parent->AddChild( _activeModel->GetGeomDataSet( i )->getpfDCS() );
            }
            else if ( ( this->_readParam->guiVal[ i ] == 0 ) &&
                     ( parent->SearchChild( _activeModel->GetGeomDataSet( i )->getpfDCS() ) != -1 ) )
            {
               temp = parent->RemoveChild( _activeModel->GetGeomDataSet( i )->getpfDCS() );
            }
            vprDEBUG(vprDBG_ALL,1) << "|   Add Child Output ( -1 is BAD ) :  " << temp 
                           << std::endl << vprDEBUG_FLUSH;
         }
      }
      else
      {
         // Change the opacity of a specific piece of geometry
         int geomFile = (int)this->commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
         float opacity = (float)this->commandArray->GetCommandValue( cfdCommandArray::CFD_MIN )*.01f;
         this->_activeModel->GetGeomDataSet( geomFile )->setOpac( opacity );
      }
   }
   // This represents all viz options. If more viz options are added these enums probably need to be changed. 
   // We should probably come up with a better way to do this.
   else if ( ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) >= CONTOUR ) && 
             ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) <= PARTICLE_TRANSIENT ) )
   {
      // for the active model, change opaque geometries to transparent
      if ( _activeModel != NULL )
      {
         for ( unsigned int i = 0; i < _activeModel->GetNumberOfGeomDataSets(); i++ )
         {
            if ( _activeModel->GetGeomDataSet( i )->GetTransparentFlag() == 1 )
            {
               vprDEBUG(vprDBG_ALL,2) << "Changing Transparency for geom : "
                                   << i << std::endl << vprDEBUG_FLUSH;
               _activeModel->GetGeomDataSet( i )->setOpac( 0.2 );
            }
         }
      }
   }  //change the model's property
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_ALL )
   { 
      // change all transparent geometries back to opaque
      for ( unsigned int j = 0; j < _modelList.size(); j++ )
      {
         for( unsigned int i = 0; i < _modelList.at( j )->GetNumberOfGeomDataSets(); i++ )
         {
            if( _modelList.at( j )->GetGeomDataSet( i )->GetTransparentFlag() == 1 )
               _modelList.at( j )->GetGeomDataSet( i )->setOpac( 1.0 );
         }
         
         for( unsigned int i = 0; i < _modelList.at( j )->GetNumberOfCfdDataSets(); i++ )
         {
            if ( _modelList.at( j )->GetCfdDataSet( i )->IsPartOfTransientSeries() )
            {
               _modelList.at( j )->GetCfdDataSet( i )->GetAnimation()->ClearSequence();
            }
         }
         //may need to add the handling of texture data stuff here
      } 
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_ACTIVE_MODEL )
   {
      vprDEBUG(vprDBG_ALL,1) << " cfdModelHandler :: Change Active Model " 
            << commandArray->GetCommandValue( cfdCommandArray::CFD_SC )
            << std::endl << vprDEBUG_FLUSH;
      _activeModel = _modelList.at( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC ) );
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
               == CHANGE_CONTOUR_SETTINGS )
   {  
      // opacity settings
      int opacity = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      double realOpacity = (double)opacity/100.0f;
      if ( this->activeDataset != NULL )
         this->activeDataset->GetLookupTable()->SetAlphaRange(realOpacity, realOpacity);
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID )== VIS_OPTION )
   {
      if ( _activeModel &&  _activeModel->GetActiveDataSet())
      {
         //cfd visualization options
         int visOpt = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );

         if ( visOpt == TEXTURE_BASED_VISUALIZATION )
         {
			   _activeModel->GetActiveDataSet()->GetSwitchNode()->SetVal(1);
            tbased = true;
         }
         else if ( visOpt == CLASSIC_VISUALIZATION )
         {
            _activeModel->GetActiveDataSet()->GetSwitchNode()->SetVal(0);
            tbased = false;
         }
      }       
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID )== MIRROR_VIS_DATA )
   {
std::cout << " help 1 "<< std::endl;
      if ( _activeModel )
      {
         bool tempFlag = commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
std::cout << " help 1 "<< tempFlag << std::endl;
			_activeModel->SetMirrorDataFlag( tempFlag );
      }
   }

   // Can't be an else if because may have to update if dataset has changed beforehand
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR || 
        commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR_RANGE ||
        updateScalarRange
        )
   { 
      int scalarIndex = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
      vprDEBUG(vprDBG_ALL,1) << "CHANGE_SCALAR || CHANGE_SCALAR_RANGE"
         << ", scalarIndex = " << scalarIndex
         << ", min = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MIN )
         << ", max = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX )
         << std::endl << vprDEBUG_FLUSH;
      //update active scalar texture if it exists
#ifdef _OSG
#ifdef VE_PATENTED
      if(_activeModel !=0)
      {
         if(_activeTDSet)
         {
            _activeTDSet->SetActiveScalar(activeDataset->GetScalarName(scalarIndex));
         }
      }
#endif
#endif

      activeDataset->SetActiveScalar( scalarIndex );
      activeDataset->GetParent()->SetActiveScalar( scalarIndex );

      activeDataset->ResetScalarBarRange( 
                           (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ), 
                           (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
      activeDataset->GetParent()->ResetScalarBarRange( 
                           (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ), 
                           (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
   }
#ifdef _OSG 
#ifdef VE_PATENTED
   if(commandArray->GetCommandValue(cfdCommandArray::CFD_ID) == X_VECTOR||
      commandArray->GetCommandValue(cfdCommandArray::CFD_ID) == Y_VECTOR||
      commandArray->GetCommandValue(cfdCommandArray::CFD_ID) == Z_VECTOR)
   {
      if(_activeModel !=0)
      {
         if(_activeTDSet)
         {
            _activeTDSet->SetActiveVector(activeDataset->GetVectorName(activeDataset->GetActiveVector()));
         }
      }
   }else if(commandArray->GetCommandValue(cfdCommandArray::CFD_ID) == X_CONTOUR||
      commandArray->GetCommandValue(cfdCommandArray::CFD_ID) == Y_CONTOUR||
      commandArray->GetCommandValue(cfdCommandArray::CFD_ID) == Z_CONTOUR)
   {
      if(_activeModel !=0)
      {
         if(_activeTDSet)
         {
            _activeTDSet->SetActiveScalar(activeDataset->GetScalarName(activeDataset->GetActiveScalar()));
         }
      }
   }
#endif
#endif
   // Check and see if we need to refresh the scalar bar
   _scalarBar->CheckCommandId( commandArray );
   // May use in the future
   //_scalarBar->UpdateCommand();
}

///////////////////////////////////////////////
// Used to initialize data for the simulation
///////////////////////////////////////////////
void cfdModelHandler::CreateObjects( void )
{  
   int numObjects;
   char text[ 256 ];
   char textLine[ 256 ];
#ifdef _OSG
#ifdef VE_PATENTED
   unsigned int nTextureDataSets = 0;
#endif
#endif
   std::ifstream input;
   input.open( this->_param );
   input >> numObjects; 
   input.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vprDBG_ALL,1) << " cfdModelHandler::Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vprDBG_ALL,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 8 )
      {
         if ( _modelList.empty() )
            _modelList.push_back( new cfdModel( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS() ) );
         // Assume only one model for now
         // Flexibilty to have multiply models
         _modelList.at( 0 )->CreateCfdDataSet();

         vprDEBUG(vprDBG_ALL,0) << " ************************************* "
                          << std::endl << vprDEBUG_FLUSH;

         vprDEBUG(vprDBG_ALL,0) << " vtk DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

         float scale[3], trans[3], rotate[3];   // pfDCS stuff
         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

         // Pass in -1 to GetCfdDataSet to get the last dataset added
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetScaleArray( scale );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetTranslationArray( trans );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetRotationArray( rotate );

         // get vtk data set name...
         char vtk_filein[ 256 ];
         input >> vtk_filein;
         input.getline( textLine, 256 );   //skip past remainder of line
            
         // Set the name of the dcs so that we can track it later on
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetName( vtk_filein );

         if ( fileIO::isFileReadable( vtk_filein ) ) 
         {
            vprDEBUG(vprDBG_ALL,0) << " vtk file = " << vtk_filein 
                             << ", dcs = "  << _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()
                             << std::endl << vprDEBUG_FLUSH;
            _modelList.at( 0 )->GetCfdDataSet( -1 )->SetFileName( vtk_filein );
         }
         else
         {
            std::cerr << "ERROR: unreadable vtk file = " << vtk_filein 
                      << ".  You may need to correct your param file."
                      << std::endl;
            exit( 1 );
         }

         char * precomputedDataSliceDir = _readParam->readDirName( input, "precomputedDataSliceDir" );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->SetPrecomputedDataSliceDir( precomputedDataSliceDir );
         delete [] precomputedDataSliceDir;
         precomputedDataSliceDir = NULL;

         char * precomputedSurfaceDir = _readParam->readDirName( input, "precomputedSurfaceDir" );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->SetPrecomputedSurfaceDir( precomputedSurfaceDir );
         delete [] precomputedSurfaceDir;
         precomputedSurfaceDir = NULL;

         this->LoadSurfaceFiles( _modelList.at( 0 )->GetCfdDataSet( -1 )->GetPrecomputedSurfaceDir() );
      }
      else if ( id == 9 ) // if it is an geom file
      {
         if ( _modelList.empty() )
            _modelList.push_back( new cfdModel( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS() ) );

         char fileName[100];
         float stlColor[3];
         int color;
         int transFlag;

         input >> transFlag;
         input.getline( textLine, 256 );   //skip past remainder of line
         vprDEBUG(vprDBG_ALL,0) << " geometry transparency flag = "
                                 << transFlag
                                 << std::endl << vprDEBUG_FLUSH;

         // read color flag
         input >> color;
         vprDEBUG(vprDBG_ALL,0) << " stl color flag = " << color
                          << std::endl << vprDEBUG_FLUSH;

         // read color if color flag = 1
         if( color == 1)
         {
            for(int i=0;i<3;i++)
            {
               input >> stlColor[ i ];
            }
            vprDEBUG(vprDBG_ALL,0) << "\tcolor: " << stlColor[ 0 ] << " : " << stlColor[ 1 ] << " : "
                                    << stlColor[ 2 ]
                                    << std::endl << vprDEBUG_FLUSH;
         }
         input.getline( textLine, 256 );   //skip past remainder of line

         vprDEBUG(vprDBG_ALL,0) << " geometry DCS parameters:" 
                          << std::endl << vprDEBUG_FLUSH;
         float scale[3], trans[3], rotate[3];   // pfDCS stuff
         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

         input >> fileName;
         input.getline( textLine, 256 );   //skip past remainder of line

         int test1 = fileIO::isFileReadable( fileName );
         if ( test1 == 1 )
         { 
            vprDEBUG(vprDBG_ALL,0) << " geometry fileName = "
                                    << fileName
                                    << std::endl << vprDEBUG_FLUSH;
         }
         else
         {
            std::cerr << "ERROR: unreadable geometry file = " << fileName 
                      << ".  You may need to correct your param file."
                      << std::endl;
            exit( 1 );
         }

         //std::cout << scale[0] << " : " << scale[1] << " : " << scale[2] << " : " << std::endl;
         _modelList.at( 0 )->CreateGeomDataSet( fileName );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetScaleArray( scale );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetTranslationArray( trans );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetRotationArray( rotate );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetName( fileName );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->SetFILEProperties( color, transFlag, stlColor );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->setOpac( 1.0f );
      }
      else if ( id == 10 )
      {
         if ( _modelList.empty() )
            _modelList.push_back( new cfdModel( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS() ) );

         float stlColor[3];
         int color;
         int transFlag;

         float duration = 0.0f;
         input >> duration;
         input.getline( textLine, 256 );   //skip past remainder of line
         VE_SceneGraph::cfdTempAnimation* animation = _modelList.at( 0 )->GetAnimation();
         animation->SetDuration( duration );

         // For the data we need to loop over all the datasets and set the appropriate data dir
         _modelList.at( 0 )->CreateCfdDataSet();

         vprDEBUG(vprDBG_ALL,0) << " ************************************* "
                          << std::endl << vprDEBUG_FLUSH;

         vprDEBUG(vprDBG_ALL,0) << " vtk DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

         float scale[3], trans[3], rotate[3];   // pfDCS stuff
         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

         // Pass in -1 to GetCfdDataSet to get the last dataset added
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetScaleArray( scale );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetTranslationArray( trans );
         _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetRotationArray( rotate );

         // get vtk data dir name...
         char vtkDataDir[ 256 ];
         input >> vtkDataDir;
         input.getline( textLine, 256 );   //skip past remainder of line
         
         // TODO: what is purpose of this line?
         fileIO::isDirWritable( vtkDataDir );
         
         // get vtk data dir name...
         char vtkPreComputeDir[ 256 ];
         input >> vtkPreComputeDir;
         input.getline( textLine, 256 );   //skip past remainder of line
         
         // Now we need to loop over all the files in the vtk dataset dir
         // and create n number of cfdDataSets
         ReadNNumberOfDataSets( vtkDataDir, vtkPreComputeDir );

         // For the geometry we need to loop over all the files and set the dcs appropriately
         char geomDirName[ 100 ];
         input >> geomDirName;
         input.getline( textLine, 256 );   //skip past remainder of line

         /*int test1 = fileIO::isFileReadable( geomDirName );
         if ( test1 == 1 )
         { 
            vprDEBUG(vprDBG_ALL,0) << " geometry fileName = "
                                    << geomDirName
                                    << std::endl << vprDEBUG_FLUSH;
         }
         else
         {
            std::cerr << "ERROR: unreadable geometry file = " << geomDirName 
                      << ".  You may need to correct your param file."
                      << std::endl;
            exit( 1 );
         }*/

         vprDEBUG(vprDBG_ALL,0) << " geometry DCS parameters:" 
                          << std::endl << vprDEBUG_FLUSH;
         this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

         input >> transFlag;
         vprDEBUG(vprDBG_ALL,0) << " geometry transparency flag = "
                                 << transFlag
                                 << std::endl << vprDEBUG_FLUSH;

         // read color flag
         input >> color;
         vprDEBUG(vprDBG_ALL,0) << " stl color flag = " << color
                          << std::endl << vprDEBUG_FLUSH;

         // read color if color flag = 1
         if( color == 1)
         {
            for(int i=0;i<3;i++)
            {
               input >> stlColor[ i ];
            }
            vprDEBUG(vprDBG_ALL,0) << "\tcolor: " << stlColor[ 0 ] << " : " << stlColor[ 1 ] << " : "
                                    << stlColor[ 2 ]
                                    << std::endl << vprDEBUG_FLUSH;
         }
         input.getline( textLine, 256 );   //skip past remainder of line

         //std::cout << scale[0] << " : " << scale[1] << " : " << scale[2] << " : " << std::endl;
         /*_modelList.at( 0 )->CreateGeomDataSet( geomDirName );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetScaleArray( scale );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetTranslationArray( trans );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->getpfDCS()->SetRotationArray( rotate );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->SetFILEProperties( color, transFlag, stlColor );
         _modelList.at( 0 )->GetGeomDataSet( -1 )->setOpac( 1.0f );*/

         // For the geometry we need to loop over all the files and set the dcs appropriately
      }
      else if ( id == 15 )
      {
#ifdef _OSG
#ifdef VE_PATENTED
         vprDEBUG(vprDBG_ALL,0) << "Creating texture dataset." << std::endl << vprDEBUG_FLUSH;

         if ( _modelList.empty() )
         {
            _modelList.push_back( new cfdModel( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS() ) );
         }
   
         //read the number of files that describe the texture
         int numTextureDescriptionFiles = 0;
         input >> numTextureDescriptionFiles;
         input.getline(textLine,256);
         char textureDescriptionFile[256];

         _modelList.at(0)->CreateTextureDataSet();

         for ( int i = 0; i < numTextureDescriptionFiles; ++i ) 
         {
             input>>textureDescriptionFile;
             input.getline(textLine,256);
             //this isn't right--we should have a pointer to the
             //active/current model in the list. . .leaving for now
             //since we only have one model
             _modelList.at(0)->AddDataSetToTextureDataSet(nTextureDataSets,
                                                textureDescriptionFile);
         }
         nTextureDataSets++;
#endif
#endif
      }
      else
      {
         // Skip past block
         _readParam->ContinueRead( input, id );
      }
   }
   
}  

void cfdModelHandler::LoadSurfaceFiles( char * precomputedSurfaceDir )
{
   if ( precomputedSurfaceDir == NULL )
   {
      vprDEBUG(vprDBG_ALL,1) << "precomputedSurfaceDir == NULL" 
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   vprDEBUG(vprDBG_ALL,1) << "Loading surface files from " 
      << precomputedSurfaceDir << std::endl << vprDEBUG_FLUSH;

   //store the current directory so we can change back to it
   char *cwd;
#ifndef WIN32
   if ((cwd = getcwd(NULL, 100)) == NULL)
   {
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      exit( 1 );
   }

   //open the directory (we already know that it is valid)

   DIR* dir = opendir( precomputedSurfaceDir );
   //change into this directory so that vtk can find the files
   chdir( precomputedSurfaceDir );
   
   //get the name of each file
   direct* file = 0;
   while( (file = readdir(dir)) != NULL )
   {
      //assume all vtk files in this directory are to be loaded
      if(strstr(file->d_name, ".vtk"))
      {
         char* pathAndFileName = new char[strlen(precomputedSurfaceDir)+
                                          strlen(file->d_name)+2];
         strcpy(pathAndFileName,precomputedSurfaceDir);
         strcat(pathAndFileName,"/");
         strcat(pathAndFileName,file->d_name);

         if ( fileIO::isFileReadable( file->d_name ) ) 
         {
            vprDEBUG(vprDBG_ALL,0) << "\tsurface file = " << pathAndFileName
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
         else
         {
            std::cerr << "ERROR: unreadable file = " << pathAndFileName
                      << ".  You may need to correct your param file."
                      << std::endl;
            exit( 1 );
         }
      }
   };
   closedir( dir );  // close the directory
   dir = 0;
   file = 0;

   chdir( cwd );     // return to original directory
#else
   char buffer[_MAX_PATH];
   HANDLE hList;
   TCHAR* directory;//[MAX_PATH+1];
   WIN32_FIND_DATA fileData;

   //get the current working directory
   if ((cwd = _getcwd(buffer, _MAX_PATH)) == NULL)
   {
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      return ;
   }

   // Get the proper directory path for transient files
   //sprintf(directory, "%s\\*", precomputedSurfaceDir);
   std::ostringstream dirStringStream;
   dirStringStream << precomputedSurfaceDir << "\\*";
   std::string dirString = dirStringStream.str();
   directory = (char*)dirString.c_str();

   //get the first file
   hList = FindFirstFile(directory, &fileData);
  
   //check to see if directory is valid
   if ( hList == INVALID_HANDLE_VALUE )
   { 
      std::cerr << "No precomputed surface files found in: "
                << precomputedSurfaceDir << std::endl;
      return ;
   }
   else
   {
      // Traverse through the directory structure
      bool finished = FALSE;
      while ( !finished )
      {
         //add the file name to our data list
         //assume all vtk files in this directory are part of the sequence
         //assume all vtk files in this directory are to be loaded
         if(strstr(fileData.cFileName, ".vtk"))
         {
            char* pathAndFileName = new char[strlen(precomputedSurfaceDir)+
                                          strlen(fileData.cFileName)+2];
            strcpy(pathAndFileName,precomputedSurfaceDir);
            strcat(pathAndFileName,"/");
            strcat(pathAndFileName,fileData.cFileName);

            if ( fileIO::isFileReadable( pathAndFileName ) ) 
            {
               vprDEBUG(vprDBG_ALL,0) << "\tsurface file = " << pathAndFileName
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
            else
            {
               std::cerr << "ERROR: unreadable file = " << pathAndFileName
                         << ".  You may need to correct your param file."
                         << std::endl;
               exit( 1 );
            }
            delete [] pathAndFileName;
         }
         //check to see if this is the last file
         if(!FindNextFile(hList, &fileData))
         {
            if(GetLastError() == ERROR_NO_MORE_FILES)
            {
               finished = TRUE;
            }
         }
      }
   }
   FindClose( hList );  // close the handle
   chdir( cwd );        // return to original directory
#endif
}
////////////////////////////////////
bool cfdModelHandler::GetVisOption()
{
   return tbased;
}
vtkPolyData* cfdModelHandler::GetArrow( void )
{
   return this->arrow;
}

void cfdModelHandler::ReadNNumberOfDataSets(  char* directory, char* preComputedDir )
{
   std::vector< char* > frameFileNames;
   std::vector< char* > frameDirNames;
   int numFiles = 0;
   // count the files and record the name of each file
   int numDir = 0;
   char *cwd;
#ifndef WIN32
   if ((cwd = getcwd(NULL, 100)) == NULL)
   {
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      exit( 1 );
   }

   // open the directory
   DIR* dir = opendir( directory );
   direct* file = 0;

   // change into this directory so that vtk can find the files
   //chdir( directory );
   
   // count the files and record the name of each file
   while( (file = readdir(dir)) != NULL )
   {
      //std::cout << file->d_name << " : " << /*file->ino_t << " : " << file->off_t << */std::endl;
      //assume all vtk files in this directory are part of the sequence
      if(strstr(file->d_name, ".vtk"))
      {
         char * pathAndFileName = new char[
                     strlen(directory) + strlen(file->d_name) + 2 ];
         strcpy( pathAndFileName, directory);
         strcat(pathAndFileName, "/");
         strcat(pathAndFileName, file->d_name);
 
         frameFileNames.push_back( pathAndFileName );
         vprDEBUG(vprDBG_ALL, 1) << " pathAndFileName : " 
            << pathAndFileName << std::endl << vprDEBUG_FLUSH;
         //increment the number of frames found
         numFiles++;
      }
   };
   //chdir( cwd );
   closedir( dir ); //close the directory
   dir = opendir( cwd );
   
   while( (file = readdir(dir)) != NULL )
   {
      //std::cout << file->d_name << " : " << preComputedDir<< " : " << /*file->ino_t << " : " << file->off_t << */std::endl;
      //assume all vtk files in this directory are part of the sequence
      if(strstr(file->d_name, preComputedDir))
      {
         char * pathAndFileName = new char[ strlen(file->d_name) + 1 ];
         strcpy( pathAndFileName, file->d_name);
 
         frameDirNames.push_back( pathAndFileName );
         vprDEBUG(vprDBG_ALL, 1) << " pathAndFileName : " 
            << pathAndFileName << std::endl << vprDEBUG_FLUSH;
         //increment the number of frames found
         numDir++;
      }
   };
#else
   char buffer[_MAX_PATH];
   HANDLE hList;
   HANDLE hList2;
   TCHAR directoryPath[MAX_PATH+1];
   WIN32_FIND_DATA fileData;

   //get the current working directory
   if ((cwd = _getcwd(buffer, _MAX_PATH)) == NULL){
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      return;
   }
   
   // Get the proper directory path for transient files
   //sprintf(directoryPath, "%s\\*", directory);
   strcpy(directoryPath,directory);
   
   //change to directory
   _chdir(directoryPath);
   //get the first file
   hList = FindFirstFile("*.vtk", &fileData);
  
   //check to see if directory is valid
   if ( hList == INVALID_HANDLE_VALUE )
   { 
      std::cerr << "No transient files found in: " << directory << std::endl;
      return;
   }
   else
   {
      // Traverse through the directory structure
      bool finished = FALSE;
      while ( !finished )
      {
         //add the file name to our data list
         //assume all vtk files in this directory are part of the sequence
         if(strstr(fileData.cFileName, ".vtk"))
         {
            char* pathAndFileName = new char[
                  strlen(directoryPath) + strlen(fileData.cFileName) + 2 ];
            strcpy(pathAndFileName,directoryPath);
            strcat(pathAndFileName,"/");
            strcat(pathAndFileName,fileData.cFileName);

            frameFileNames.push_back( pathAndFileName );
            
            vprDEBUG(vprDBG_ALL,1) << " pathAndFileName : " << pathAndFileName
                                   << std::endl << vprDEBUG_FLUSH;
            //increment the number of frames found
            numFiles++;
         }
         //check to see if this is the last file
         if ( !FindNextFile(hList, &fileData) )
         {
            if ( GetLastError() == ERROR_NO_MORE_FILES )
            {
               finished = TRUE;
            }
         }
      }
   }
   FindClose( hList );
   _chdir( cwd );
   char tempDir[1024];
   strcpy(tempDir,cwd);
   strcat(tempDir,"/");
   strcat(tempDir,preComputedDir);
   char tempName[1024];
   strcpy(tempName,preComputedDir);
   strcat(tempName,"*");
   //get the first file
   hList2 = FindFirstFile(tempName, &fileData);
  
   //check to see if directory is valid
   if ( hList2 == INVALID_HANDLE_VALUE )
   { 
      std::cerr << "No transient files found in: " << directory << std::endl;
      return;
   }
   else
   {
      // Traverse through the directory structure
      bool finished = FALSE;
      while ( !finished )
      {
         //add the file name to our data list
         //assume all vtk files in this directory are part of the sequence
         if(strstr(fileData.cFileName,preComputedDir ))
         {
            char* pathAndFileName = new char[
                  strlen(cwd) + strlen(fileData.cFileName) + 2 ];
            strcpy(pathAndFileName,cwd);
            strcat(pathAndFileName,"/");
            strcat(pathAndFileName,fileData.cFileName);

            frameDirNames.push_back( pathAndFileName );
            
            vprDEBUG(vprDBG_ALL,1) << " pathAndFileName : " << pathAndFileName
                                   << std::endl << vprDEBUG_FLUSH;
            //increment the number of frames found
            numDir++;
         }
         //check to see if this is the last file
         if ( !FindNextFile(hList, &fileData) )
         {
            if ( GetLastError() == ERROR_NO_MORE_FILES )
            {
               finished = TRUE;
            }
         }
      }
   }
   FindClose( hList2 );
   // close the handle
   //change to the precomputed directory
   //chdir(preComputedDir);

           // return to original directory

#endif

   vprDEBUG(vprDBG_ALL,0) << " Number of files in directory \"" 
      << directory << "\" = " << numFiles
      << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vprDBG_ALL,0) << " Number of precomputes directorys \"" 
      << preComputedDir << "\" = " << numDir
      << std::endl << vprDEBUG_FLUSH;
   // The directory must contain only transient files of a particular type
   // (ie, y-plane slices)
   // Filenames should be something like:
   //   grid_0.vtk ==> time step 0
   //      . . .
   //   grid_21.vtk==> time step 21
   // The important components of the filename are the underscore before
   // the integer, the integer, the period, and the extension. 
   // The extension must be "vtk" for data files
   // or "iv", "flt", or "stl" for geometry files.  
   // The integer may be zero-padded (grid_0021.vtk is OK). 

   //Now numerically order the list of names because readdir doesn't
   int* order = NULL;
   order = new int [ numFiles ];
   for (int j = 0; j < numFiles; j++)
   {
      int number = fileIO::extractIntegerBeforeExtension( frameFileNames[ j ] );
      order[ number ] = j;
      vprDEBUG(vprDBG_ALL,2) << "\t" << j << "\t" << number << "\t" 
         << frameFileNames[ j ] << std::endl << vprDEBUG_FLUSH;
   }

   // Numerically order the dir name list 
   int* dirOrder = NULL;
   dirOrder = new int [ numDir ];
   for (int j = 0; j < numDir; j++)
   {
      int number = fileIO::ExtractIntegerFromString( frameDirNames[ j ] );
      dirOrder[ number ] = j;
      vprDEBUG(vprDBG_ALL,2) << "\t" << j << "\t" << number << "\t" 
         << frameDirNames[ j ] << std::endl << vprDEBUG_FLUSH;
   }

   // Set initial data file name 
   if (fileIO::isFileReadable( frameFileNames[ order[ 0 ] ] ) ) 
   {
      vprDEBUG(vprDBG_ALL,0) << " vtk file = " << frameFileNames[ order[ 0 ]  ]
                       << ", dcs = "  << _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()
                       << std::endl << vprDEBUG_FLUSH;
      _modelList.at( 0 )->GetCfdDataSet( -1 )->SetFileName( frameFileNames[ order[ 0 ]  ] );
   }
   else
   {
      std::cerr << "ERROR: unreadable vtk file = " << frameFileNames[ order[ 0 ]  ]
                << ".  You may need to correct your param file."
                << std::endl;
      exit( 1 );
   }

   // Set initial precomputed data dir file name 
   if ( fileIO::isDirWritable( frameDirNames[ dirOrder[ 0 ] ] ) )
   {
      vprDEBUG(vprDBG_ALL,0) << " vtk data dir = " << frameDirNames[ dirOrder[ 0 ] ]
                      << std::endl << vprDEBUG_FLUSH;
      _modelList.at( 0 )->GetCfdDataSet( -1 )->SetPrecomputedDataSliceDir( frameDirNames[ dirOrder[ 0 ] ] );
   }
   else
   {
      std::cerr << "ERROR: unreadable vtk file = " << frameDirNames[ dirOrder[ 0 ] ]
                << ".  You may need to correct your param file."
                << std::endl;
      exit( 1 );
   }

   VE_SceneGraph::cfdDCS* baseTransientDCS = _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS();
   VE_SceneGraph::cfdTempAnimation* animation = _modelList.at( 0 )->GetAnimation();
   animation->SetNumberOfFrames( numFiles );
   animation->SetGroups();
   _modelList.at( 0 )->GetCfdDataSet( -1 )->SetAnimation( animation );
   _modelList.at( 0 )->transientDataSets[ _modelList.at( 0 )->GetKeyForCfdDataSet( 
                        _modelList.at( 0 )->GetCfdDataSet( -1 ) ) ] = _modelList.at( 0 )->GetCfdDataSet( -1 );
   _modelList.at( 0 )->GetCfdDataSet( -1 )->SetAsPartOfTransientSeries();
   animation->GetGroup( 0 )->AddChild( _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS() );
   for ( int j = 1; j < numFiles; j++ )
   {
      vprDEBUG(vprDBG_ALL,0) << " For \"" << frameFileNames[ order[ j ]  ]
                             << "\"..." << std::endl << vprDEBUG_FLUSH;

      _modelList.at( 0 )->CreateCfdDataSet();

      // Pass in -1 to GetCfdDataSet to get the last dataset added
      _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetScaleArray( baseTransientDCS->GetScaleArray() );
      _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetTranslationArray( baseTransientDCS->GetVETranslationArray() );
      _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()->SetRotationArray( baseTransientDCS->GetRotationArray() );
      _modelList.at( 0 )->GetCfdDataSet( -1 )->SetAsPartOfTransientSeries();
      
      int key = _modelList.at( 0 )->GetKeyForCfdDataSet( 
                        _modelList.at( 0 )->GetCfdDataSet( -1 ) );
      _modelList.at( 0 )->transientDataSets[ key ] = _modelList.at( 0 )->GetCfdDataSet( -1 );
      _modelList.at( 0 )->GetCfdDataSet( -1 )->SetAnimation( animation );
      animation->GetGroup( j )->AddChild( _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS() );

      if (fileIO::isFileReadable( frameFileNames[ order[ j ]  ] ) ) 
      {
         vprDEBUG(vprDBG_ALL,0) << " vtk file = " << frameFileNames[ order[ j ]  ] 
                          << ", dcs = "  << _modelList.at( 0 )->GetCfdDataSet( -1 )->GetDCS()
                          << std::endl << vprDEBUG_FLUSH;
         _modelList.at( 0 )->GetCfdDataSet( -1 )->SetFileName( frameFileNames[ order[ j ]  ] );
      }
      else
      {
         std::cerr << "ERROR: unreadable vtk file = " << frameFileNames[ order[ j ]  ] 
                   << ".  You may need to correct your param file."
                   << std::endl;
         exit( 1 );
      }

      // Set initial precomputed data dir file name 
      if ( fileIO::isDirWritable( frameDirNames[ dirOrder[ j ] ] ) )
      {
         vprDEBUG(vprDBG_ALL,0) << " vtk data dir = " << frameDirNames[ dirOrder[ j ] ]
                      << std::endl << vprDEBUG_FLUSH;
         _modelList.at( 0 )->GetCfdDataSet( -1 )->SetPrecomputedDataSliceDir( frameDirNames[ dirOrder[ j ] ] );
      }
      else
      {
         std::cerr << "ERROR: unreadable vtk file = " << frameDirNames[ dirOrder[ j ] ]
                   << ".  You may need to correct your param file."
                   << std::endl;
         exit( 1 );
      }
   }
   
   frameDirNames.clear();
   frameFileNames.clear();

#ifndef WIN32
   closedir( dir ); //close the directory
   dir = 0;
   file = 0;
#endif
}

