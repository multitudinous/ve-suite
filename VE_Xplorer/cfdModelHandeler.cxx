/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdModel.cxx,v $
 * Date modified: $Date: 2004-05-18 13:44:18 -0700 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdModelHandeler.h"

#include "cfdDCS.h"
#include "cfdObjects.h"
#include "cfdDataSet.h"
#include "fileIO.h"
#include "cfdModel.h"
#include "cfdVectorBase.h"
#include "cfdCommandArray.h"
#include "cfdEnum.h"

#include <string>
#include <cstdio>

#include <vtkPolyDataReader.h>
#include <vtkPolyData.h>

#include <vrj/Util/Debug.h>

cfdModelHandler::cfdModelHandler( char* input, cfdDCS* dcs)
{
   _param = input;
   worldNode = dcs;
}

cfdModelHandler::~cfdModelHandler()
{
}

///////////////////////
// Helper functions
///////////////////////
void cfdModelHandler::SetCommandArray( cfdCommandArray* input )
{
   commandArray = input;
}

cfdObjects* cfdModelHandler::GetActiveSequence( void )
{
   return activeSequenceObject;
}
///////////////////////

void cfdModelHandler::InitScene( void )
{
   // Locate and load the arrow file...
   char * arrowFile = fileIO::GetFile( "arrow", "/VE_Xplorer/data/arrow.vtk" );
   if ( arrowFile == NULL )
   {
      std::cout << " ERROR : The vtkPolyData arrow file could not be found "
                << "        Please create one and put it in the data dir "
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
      }

   for ( unsigned int j = 0; j < _modelList.size(); j++ )
      for ( unsigned int i = 0; i < _modelList.at( j )->GetNumberOfCfdDataSets(); i++)
      {
         int cfdType = _modelList.at( j )->GetCfdDataSet( i )->GetType();
         vprDEBUG(vprDBG_ALL,1) << "cfdType: " << cfdType
                             << std::endl << vprDEBUG_FLUSH;
// I think this functionality will disappear. I think the active dataset should be set by the gui 
// and that indvidula viz routines should check to make sure that it is the appropriate data set 
// for the viz routine. This should be fixed very soon.
/*
         // Initialize active meshed volume and polydata when suitable
         // datasets exist
         if ( cfdObjects::GetActiveMeshedVolume() == NULL && cfdType == 0 )
         {
            cfdObjects::SetActiveMeshedVolume( modelList.at( j )->GetCfdDataSet( i ) );

            vprDEBUG(vprDBG_ALL,1) << "Setting " 
               << cfdObjects::GetActiveMeshedVolume()->GetFileName()
               << " as active meshed volume"
               << std::endl << vprDEBUG_FLUSH;
         }
         else if ( cfdObjects::GetActiveParticleData() == NULL && cfdType == 1 )
         {
            cfdObjects::SetActiveParticleData( modelList.at( j )->GetCfdDataSet( i ) );

            vprDEBUG(vprDBG_ALL,1) << "Setting " 
               << cfdObjects::GetActiveParticleData()->GetFileName()
               << " as active particle set"
               << std::endl << vprDEBUG_FLUSH;
         }
         else if ( cfdObjects::GetActiveSurfaceData() == NULL && cfdType == 2 )
         {
            cfdObjects::SetActiveSurfaceData( modelList.at( j )->GetCfdDataSet( i ) );

            vprDEBUG(vprDBG_ALL,1) << "Setting " 
               << cfdObjects::GetActiveSurfaceData()->GetFileName()
               << " as active surface"
               << std::endl << vprDEBUG_FLUSH;
         }*/
      }
   // set default active dataset to be the meshed volume
   if ( cfdObjects::GetActiveMeshedVolume() )
      cfdObjects::SetActiveDataSet( cfdObjects::GetActiveMeshedVolume() );

   if ( cfdObjects::GetActiveDataSet() != NULL )
   {
      // set first scalar active
      cfdObjects::GetActiveDataSet()->SetActiveScalar( 0 );
      strcpy( oldDatasetName, cfdObjects::GetActiveDataSet()->GetFileName() );

      cfdVectorBase::SetThreshHoldPercentages( 0, 100 );
      cfdVectorBase::UpdateThreshHoldValues();
      cfdVectorBase::SetVectorRatioFactor( 1 );
   }

// Fix this
// Need to configure this to work with Gengxuns functionality in cfdModel.
   // Process any geometry files listed in the parameter file...
/*   for ( unsigned int j = 0; j < _modelList.size(); j++ )
      for ( unsigned int i = 0; i < _modelList.at( j )->GetNumberOfCfdDataSets(); i++)
      {    
         std::cout << "|   Intializing Geometry file " 
                << this->paramReader->files[p]->fileName << std::endl;
         //biv --testing for windows
         cfdFILE* newGeom = 0;
         newGeom = new cfdFILE( this->paramReader->files[ p ],
                                          this->worldDCS );
         this->geomL.push_back( newGeom );
         this->geomL[ p ]->Initialize( 1.0 );

         // give the geometry DCS a name so that it can be detected during a CLEAR_ALL
         this->geomL[ p ]->getpfDCS()->SetName("geometry");
      }*/

      //this->changeGeometry = false;
   #ifdef TABLET
   // Don't know what this is used for fix this 
   //  this->SetCfdReadParam( this->paramReader );
   #endif // TABLET
}

void cfdModelHandler::PreFrameUpdate( void )
{
// code for cfdCommandArray stuff
// Fix this 
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_STEADYSTATE_DATASET )
   {
      unsigned int i = commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      vprDEBUG(vprDBG_ALL,1) 
         << "CHANGE_STEADYSTATE_DATASET " << i
         //<< ", scalarIndex = " << this->cfdSc
         //<< ", min = " << this->cfdMin 
         //<< ", max = " << this->cfdMax
         << std::endl << vprDEBUG_FLUSH;
   // Need to add some sort of model reference in later fix later
   //for ( unsigned int j = 0; j < _modelList.size(); j++ )
      unsigned int j = 0;
      
      if ( i < _modelList.at( j )->GetNumberOfCfdDataSets() )
      {
         vprDEBUG(vprDBG_ALL,0) << " dataset = "
            << _modelList.at( j )->GetCfdDataSet( i )->GetFileName()
            << ", dcs = " << _modelList.at( j )->GetCfdDataSet( i )->GetDCS()
            << std::endl << vprDEBUG_FLUSH;

         int cfdType = _modelList.at( j )->GetCfdDataSet( i )->GetType();
         vprDEBUG(vprDBG_ALL,1) << " cfdType: " << cfdType
                                << std::endl << vprDEBUG_FLUSH;

// This is bad need to get rid of this
// Fix this
/*         // set the dataset as the appropriate dastaset type
         // (and the active dataset as well)
         if ( cfdType == 0 )
         {
            cfdObjects::SetActiveMeshedVolume( _modelList.at( j )->GetCfdDataSet( i ) );
         }
         else if ( cfdType == 1 )
         {
            cfdObjects::SetActiveParticleData( _modelList.at( j )->GetCfdDataSet( i ) );
         }
         else if ( cfdType == 2 )
         {
            cfdObjects::SetActiveSurfaceData( _modelList.at( j )->GetCfdDataSet( i ) );
         }
         else
         {
            std::cerr << "Unsupported cfdType: " << cfdType << std::endl;
         }
*/
         vprDEBUG(vprDBG_ALL,1) << "last active dataset name = " 
                                << oldDatasetName
                                << std::endl << vprDEBUG_FLUSH;

         vprDEBUG(vprDBG_ALL,1) << "Activating steady state file " 
                   << cfdObjects::GetActiveDataSet()->GetFileName()
                   << std::endl << vprDEBUG_FLUSH;

         // make sure that the user did not just hit same dataset button
         // (or change scalar since that is routed through here too)
         if ( strcmp( oldDatasetName, 
                      cfdObjects::GetActiveDataSet()->GetFileName() ) )
         {
            vprDEBUG(vprDBG_ALL,1) << " setting dataset as newly activated" 
                                   << std::endl << vprDEBUG_FLUSH;
            cfdObjects::GetActiveDataSet()->SetNewlyActivated();
            strcpy( oldDatasetName, 
                    cfdObjects::GetActiveDataSet()->GetFileName() );
         }

         // update scalar bar for possible new scalar name
         // Need to fix this or have different gui logic
         //this->setId( CHANGE_SCALAR );
      }
      else
      {
         std::cerr << "ERROR: requested steady state dataset " 
                   << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) << " must be less than " 
                   << _modelList.at( j )->GetNumberOfCfdDataSets()
                   << std::endl;
         // Need to add these in later
         //return true;
      }
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR || 
             commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR_RANGE )
   { 
      int scalarIndex = commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
      vprDEBUG(vprDBG_ALL,1) << "CHANGE_SCALAR || CHANGE_SCALAR_RANGE"
         << ", scalarIndex = " << scalarIndex
         << ", min = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MIN )
         << ", max = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX )
         << std::endl << vprDEBUG_FLUSH;

      cfdObjects::GetActiveDataSet()->SetActiveScalar( scalarIndex );
      cfdObjects::GetActiveDataSet()->GetParent()
                                    ->SetActiveScalar( scalarIndex );

      cfdObjects::GetActiveDataSet()->ResetScalarBarRange( 
                           commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ), commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
      cfdObjects::GetActiveDataSet()->GetParent()->ResetScalarBarRange( 
                           commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ), commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
      //return true;
   }
}
