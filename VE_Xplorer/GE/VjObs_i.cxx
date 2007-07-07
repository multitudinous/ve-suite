/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#include "VE_Xplorer/GE/VjObs_i.h"

#include "VE_Open/XML/DOMDocumentManager.h"

#include "VE_Xplorer/XplorerHandlers/cfdTeacher.h"
#include "VE_Xplorer/XplorerHandlers/cfdQuatCamHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdSteadyStateVizHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdDisplaySettings.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"
#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"
#include "VE_Xplorer/SceneGraph/CADEntity.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"

#ifdef _OSG
#include "VE_Xplorer/XplorerHandlers/cfdTextureBasedVizHandler.h"
#include "VE_Xplorer/TextureBased/cfdVolumeVisualization.h"
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"

using namespace VE_TextureBased;
#endif

#include <vpr/System.h>
#include <vpr/Util/Debug.h>
#include <jccl/RTRC/ConfigManager.h>

#include <iostream>
#include <map>

//vprSingletonImp( VE_Xplorer::VjObs_i );

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_XML;

VjObs_i::VjObs_i()
{
   this->numOfClientInfo = 9;
   isCluster = false;
   //int temp=0;
   //this->setClients( 0 );
   _cfdArray = new cfdCommandArray();
   _cfdArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   _bufferArray = new cfdCommandArray();
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );

   teacher_name   = new VjObs::scalar_p();

   //dataset_types  = new VjObs::obj_p(50);
   //dataset_types->length(50);
   //num_scalars_per_dataset = new VjObs::obj_p(50);
   //num_scalars_per_dataset->length(50);
   //num_vectors_per_dataset = new VjObs::obj_p( 50 );
   //num_vectors_per_dataset->length(50);
   //vec_name = new VjObs::scalar_p( 50 );
   //vec_name->length( 50 );
   // This array is used in place of the call backs
   // to the client because the communication didn't
   // seem to work. There are 9 entries becuase that
   // how many variables are synchronized during an
   // update call in VjObs_i
   clientInfoObserverDataArray = new VjObs::obj_pd(50);
   clientInfoObserverDataArray->length(50);
   clientInfoObserverDataArray->length( this->numOfClientInfo );
   clientInfoObserverDataArray->length(50);
   //this->_unusedNewData = false;
   _models = NULL;
   time_since_start = 0.0f;
   frameNumber = 0;
   bufferCommand = new Command(  );
   bufferCommand->AddDataValuePair( new DataValuePair(  ) );
   bufferCommand->SetCommandName( "wait" );
}
////////////////////////////////////////////////////////////////////////////////
void VjObs_i::SetClusterMode( bool clusterFlag )
{
   isCluster = clusterFlag;
}
////////////////////////////////////////////////////////////////////////////////
bool VjObs_i::GetClusterMode( void )
{
   return isCluster;
}
////////////////////////////////////////////////////////////////////////////////
void VjObs_i::InitCluster( void )
{
   if ( !isCluster )
   {
      return;
   }

   // Cluster Stuff
   vpr::GUID new_guid("15c09c99-ed6d-4994-bbac-83587d4400d1");
   this->mStates.init(new_guid);

   //Initialize cluster variables
   this->mStates->clusterIso_value = 0.0f;
   this->mStates->clusterSc = 0.0f;
   this->mStates->clusterMin = 0.0f;
   this->mStates->clusterMax = 0.0f;
   this->mStates->clusterId = 0.0f;
   this->mStates->clusterGeo_state = 0.0f;
   this->mStates->clusterPostdata_state = 0.0f;
   this->mStates->clusterPre_state = false;
   this->mStates->clusterTimesteps = 0.0f;
   this->mStates->clusterTeacher_state = 0.0f; 
   this->mStates->clusterClientInfoFlag = 0; 
   this->mStates->currentFrame = 0;; // the index of the current frame
   this->mStates->clusterTime_since_start = 0;
   this->mStates->clusterFrameNumber = 0;
   this->mStates->clusterQuatCamIncrement = 0.0f;
   
   for(int i=0;i<16;i++)
   {
      this->mStates->clusterMatrix[i] = 0.0f;
   }
   this->mStates->clusterMatrix[0] = 1.0f;
   this->mStates->clusterMatrix[5] = 1.0f;
   this->mStates->clusterMatrix[10] = 1.0f;
   this->mStates->clusterMatrix[15] = 1.0f;
   
   this->mStates->clusterXMLCommands.clear();
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
VjObs::Model* VjObs_i::GetModel( CORBA::Long modelID )
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   VjObs::Model_var tempModel = 0;
   tempModel = new VjObs::Model();
   //Make sure we have some models
   int numberOfModels = cfdModelHandler::instance()->GetNumberOfModels();
   if ( numberOfModels == 0 )
   {
      return tempModel._retn();
   }
   
   // find the right model
   cfdModel* tempCfdModel = 0;
   for ( int i = 0; i < numberOfModels; ++i )
   {
      tempCfdModel = cfdModelHandler::instance()->GetModel( i );
      if ( (CORBA::Long)tempCfdModel->GetID() == modelID )
      {
         std::cout<<"Found model: "<<modelID<<std::endl;
         break;
      }
      tempCfdModel = 0;
   }
   
   // if we didn't find it then...
   if ( tempCfdModel == 0 )
   {
      std::cout<<"Didn't find model: "<<modelID<<std::endl;
      return tempModel._retn();
   }
   
   // now lets pass the model back
   CORBA::ULong numDatasets = tempCfdModel->GetNumberOfCfdDataSets();
   vprDEBUG(vprDBG_ALL,0) << " numDatasets = " << numDatasets << std::endl << vprDEBUG_FLUSH;
   if ( numDatasets > 0 )
   {
      //tempModel->dataVector = VjObs::Datasets( numDatasets ); 
      tempModel->dataVector.length( numDatasets );

      //tempModel->datasettypes  = VjObs::obj_p( numDatasets );
      tempModel->datasettypes.length( numDatasets );

      //tempModel->num_scalars_per_dataset = VjObs::obj_p( numDatasets );
      tempModel->num_scalars_per_dataset.length( numDatasets );

      //tempModel->num_vectors_per_dataset = VjObs::obj_p( numDatasets );
      tempModel->num_vectors_per_dataset.length( numDatasets );

      CORBA::ULong totalNumberOfScalars = 0;
      CORBA::ULong totalNumberOfVectors = 0;
      for ( CORBA::ULong j=0; j< numDatasets; j++ )
      {
         totalNumberOfScalars = tempCfdModel->GetCfdDataSet( j )->GetNumberOfScalars();
         totalNumberOfVectors = tempCfdModel->GetCfdDataSet( j )->GetNumberOfVectors();

         //tempModel.dataVector[ j ].vectornames = VjObs::scalar_p( totalNumberOfVectors );
         tempModel->dataVector[ j ].vectornames.length( totalNumberOfVectors );
      }
      vprDEBUG(vprDBG_ALL,0)
         << " totalNumberOfScalars: " << totalNumberOfScalars
         << std::endl << vprDEBUG_FLUSH;

      vprDEBUG(vprDBG_ALL,0)
         << " totalNumberOfVectors: " << totalNumberOfVectors
         << std::endl << vprDEBUG_FLUSH;

      for ( CORBA::ULong j=0; j < numDatasets; j++ )
      {
         tempModel->dataVector[ j ].datasetname = CORBA::string_dup( 
                    tempCfdModel->GetCfdDataSet( j )->GetFileName().c_str() );
         vprDEBUG(vprDBG_ALL,1) << " dataset_name:   " 
             << tempModel->dataVector[ j ].datasetname.in()
             << std::endl << vprDEBUG_FLUSH;

         tempModel->datasettypes[ j ] = tempCfdModel->GetCfdDataSet( j )->GetType();
   
         CORBA::Short num_scalars = tempCfdModel->GetCfdDataSet( j )->GetNumberOfScalars();
         tempModel->num_scalars_per_dataset[ j ] = num_scalars;

         //tempModel->dataVector[ j ].scalarVector = VjObs::Scalars( num_scalars ); 
         tempModel->dataVector[ j ].scalarVector.length( num_scalars );
         for (CORBA::ULong k=0; k < (unsigned int)num_scalars; k++ )
         {
            //Set scalar name
            tempModel->dataVector[ j ].scalarVector[ k ].scalarnames = 
                CORBA::string_dup(
                tempCfdModel->GetCfdDataSet( j )->GetScalarName( k ).c_str() );
            // Then get scalar range for a particular scalar
            tempCfdModel->GetCfdDataSet( j )->SetActiveScalar( (int)k );
            double* range = tempCfdModel->GetCfdDataSet( j )->GetRange();
            // Allocate
            tempModel->dataVector[ j ].scalarVector[ k ].scalarrange = 
                VjObs::obj_pd( 2 ); 
            tempModel->dataVector[ j ].scalarVector[ k ].scalarrange.length( 2 );
            // Set
            tempModel->dataVector[ j ].scalarVector[ k ].scalarrange[ 0 ] = 
                range[ 0 ];
            tempModel->dataVector[ j ].scalarVector[ k ].scalarrange[ 1 ] = 
                range[ 1 ];
            vprDEBUG(vprDBG_ALL,1) << "\tscl_name : " 
                << tempModel->dataVector[ j ].scalarVector[ k ].scalarnames.in()
                << tempModel->dataVector[ j ].scalarVector[ k ].scalarrange[ 0 ] 
                << " : "
                << tempModel->dataVector[ j ].scalarVector[ k ].scalarrange[ 1 ] 
                << std::endl << vprDEBUG_FLUSH;
         }

         CORBA::Short num_vectors = tempCfdModel->GetCfdDataSet( j )
                                    ->GetNumberOfVectors();
         tempModel->num_vectors_per_dataset[ j ] = num_vectors;

         for (CORBA::ULong k=0; k < (unsigned int)num_vectors; k++ )
         {
            tempModel->dataVector[ j ].vectornames[ k ] = CORBA::string_dup(
              tempCfdModel->GetCfdDataSet( j )->GetVectorName( k ).c_str() );
            vprDEBUG(vprDBG_ALL,1) << "\tvec_name : " << tempModel->dataVector[ j ].vectornames[ k ]
                          << std::endl << vprDEBUG_FLUSH;
         }
      }
   }
   CORBA::ULong numGeoArrays = tempCfdModel->GetNumberOfGeomDataSets();
   vprDEBUG(vprDBG_ALL,0)
         << " Number of geometries to be transfered to the client: "
         << numGeoArrays 
         << std::endl << vprDEBUG_FLUSH;
   if( numGeoArrays > 0 )
   {
      //(*_models)[ i ].geometrynames = VjObs::scalar_p(50);
      tempModel->geometrynames.length( numGeoArrays );
      for(CORBA::ULong j = 0; j < numGeoArrays; j++)
      {
            vprDEBUG(vprDBG_ALL,0)
                  << " Geometry file ( "
                  << j << " ) = " << tempCfdModel->GetGeomDataSet( j )->GetFilename() 
                  << std::endl << vprDEBUG_FLUSH;
            tempModel->geometrynames[ j ] = CORBA::string_dup(
                            tempCfdModel->GetGeomDataSet( j )->GetFilename().c_str() );
      }
   }

   return tempModel._retn();
}

/////////////////////////////////////////////////////////////
VjObs::Models* VjObs_i::GetModels()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   this->CreateDatasetInfo();
   VjObs::Models* models_= new VjObs::Models(*_models);
   return models_;
}

/////////////////////////////////////////////////////////////
void VjObs_i::CreateDatasetInfo( void )
{   
   CORBA::ULong numberOfModels = cfdModelHandler::instance()->GetNumberOfModels();
   if ( numberOfModels > 0 )
   {
      if ( _models != NULL )
      {
         delete _models;
      }

      _models = new VjObs::Models( numberOfModels );
      _models->length( numberOfModels );
         vprDEBUG(vprDBG_ALL,0) << " Number of Models = " << numberOfModels
                          << std::endl << vprDEBUG_FLUSH;
      for ( CORBA::ULong i = 0; i < numberOfModels; i++ )
      {
         cfdModel* temp = cfdModelHandler::instance()->GetModel( i );
         CORBA::ULong numDatasets = temp->GetNumberOfCfdDataSets();
         vprDEBUG(vprDBG_ALL,0) << " numDatasets = " << numDatasets
                          << std::endl << vprDEBUG_FLUSH;
         if ( numDatasets > 0 )
         {
            (*_models)[ i ].dataVector = VjObs::Datasets( numDatasets ); 
            (*_models)[ i ].dataVector.length( numDatasets );

            (*_models)[ i ].datasettypes  = VjObs::obj_p( numDatasets );
            (*_models)[ i ].datasettypes.length( numDatasets );

            (*_models)[ i ].num_scalars_per_dataset = VjObs::obj_p( numDatasets );
            (*_models)[ i ].num_scalars_per_dataset.length( numDatasets );

            (*_models)[ i ].num_vectors_per_dataset = VjObs::obj_p( numDatasets );
            (*_models)[ i ].num_vectors_per_dataset.length( numDatasets );

            CORBA::ULong totalNumberOfScalars = 0;
            CORBA::ULong totalNumberOfVectors = 0;
            for ( CORBA::ULong j=0; j< numDatasets; j++ )
            {
               totalNumberOfScalars = temp->GetCfdDataSet( j )->GetNumberOfScalars();
               totalNumberOfVectors = temp->GetCfdDataSet( j )->GetNumberOfVectors();

               (*_models)[ i ].dataVector[ j ].vectornames = VjObs::scalar_p( totalNumberOfVectors );
               (*_models)[ i ].dataVector[ j ].vectornames.length( totalNumberOfVectors );
            }
            vprDEBUG(vprDBG_ALL,0)
               << " totalNumberOfScalars: " << totalNumberOfScalars
               << std::endl << vprDEBUG_FLUSH;

            vprDEBUG(vprDBG_ALL,0)
               << " totalNumberOfVectors: " << totalNumberOfVectors
               << std::endl << vprDEBUG_FLUSH;

            for ( CORBA::ULong j=0; j < numDatasets; j++ )
            {
               (*_models)[ i ].dataVector[ j ].datasetname = CORBA::string_dup( 
                          temp->GetCfdDataSet( j )->GetFileName().c_str() );
               vprDEBUG(vprDBG_ALL,1) << " dataset_name:   " << (*_models)[ i ].dataVector[ j ].datasetname
                             << std::endl << vprDEBUG_FLUSH;

               (*_models)[ i ].datasettypes[ j ] = temp->GetCfdDataSet( j )->GetType();
   
               CORBA::Short num_scalars = temp->GetCfdDataSet( j )->GetNumberOfScalars();
               (*_models)[ i ].num_scalars_per_dataset[ j ] = num_scalars;

               (*_models)[ i ].dataVector[ j ].scalarVector = VjObs::Scalars( num_scalars ); 
               (*_models)[ i ].dataVector[ j ].scalarVector.length( num_scalars );
               for (CORBA::ULong k=0; k < (unsigned int)num_scalars; k++ )
               {
                  //Set scalar name
                  (*_models)[ i ].dataVector[ j ].scalarVector[ k ].scalarnames = CORBA::string_dup(
                    temp->GetCfdDataSet( j )->GetScalarName( k ).c_str() );
                  // Then get scalar range for a particular scalar
                  temp->GetCfdDataSet( j )->SetActiveScalar( (int)k );
                  double* range = temp->GetCfdDataSet( j )->GetRange();
                  // Allocate
                  (*_models)[ i ].dataVector[ j ].scalarVector[ k ].scalarrange = VjObs::obj_pd( 2 ); 
                  (*_models)[ i ].dataVector[ j ].scalarVector[ k ].scalarrange.length( 2 );
                  // Set
                  (*_models)[ i ].dataVector[ j ].scalarVector[ k ].scalarrange[ 0 ] = range[ 0 ];
                  (*_models)[ i ].dataVector[ j ].scalarVector[ k ].scalarrange[ 1 ] = range[ 1 ];
                  vprDEBUG(vprDBG_ALL,1) << "\tscl_name : " << (*_models)[ i ].dataVector[ j ].scalarVector[ k ].scalarnames
                                 << (*_models)[ i ].dataVector[ j ].scalarVector[ k ].scalarrange[ 0 ] << " : "
                                 << (*_models)[ i ].dataVector[ j ].scalarVector[ k ].scalarrange[ 1 ] << std::endl << vprDEBUG_FLUSH;
               }

               CORBA::Short num_vectors = temp->GetCfdDataSet( j )
                                          ->GetNumberOfVectors();
               (*_models)[ i ].num_vectors_per_dataset[ j ] = num_vectors;

               for (CORBA::ULong k=0; k < (unsigned int)num_vectors; k++ )
               {
                  (*_models)[ i ].dataVector[ j ].vectornames[ k ] = CORBA::string_dup(
                    temp->GetCfdDataSet( j )->GetVectorName( k ).c_str() );
                  vprDEBUG(vprDBG_ALL,1) << "\tvec_name : " << (*_models)[ i ].dataVector[ j ].vectornames[ k ]
                                << std::endl << vprDEBUG_FLUSH;
               }
            }
         }
         CORBA::ULong numGeoArrays = temp->GetNumberOfGeomDataSets();
         vprDEBUG(vprDBG_ALL,0)
               << " Number of geometries to be transfered to the client: "
               << numGeoArrays 
               << std::endl << vprDEBUG_FLUSH;

         if( numGeoArrays > 0 )
         {
            (*_models)[ i ].geometrynames = VjObs::scalar_p(50);
            (*_models)[ i ].geometrynames.length( numGeoArrays );
            for(CORBA::ULong j = 0; j < numGeoArrays; j++)
            {
                  vprDEBUG(vprDBG_ALL,0)
                        << " Geometry file ( "
                        << j << " ) = " << temp->GetGeomDataSet( j )->GetFilename() 
                        << std::endl << vprDEBUG_FLUSH;
                  (*_models)[ i ].geometrynames[ j ] = CORBA::string_dup(
                                  temp->GetGeomDataSet( j )->GetFilename().c_str() );
            }
         }
      }
   }

   vprDEBUG(vprDBG_ALL,1) << "\tleaving VjObs_i::CreateDatasetInfo()"
                          << std::endl << vprDEBUG_FLUSH;
}

/////////////////////////////////////////////////////////////
void VjObs_i::CreateTeacherInfo( void )
{   
   CORBA::Short numTeacherArrays = cfdEnvironmentHandler::instance()->GetTeacher()->getNumberOfFiles();
   vprDEBUG(vprDBG_ALL,0)
      << " Number of performer binary files to be transfered to the client: "
      << numTeacherArrays
      << std::endl << vprDEBUG_FLUSH;

   //this->setNumTeacherArrays( numTeacherArrays );
   if( numTeacherArrays > 0 )
   {
      this->teacher_name->length( numTeacherArrays );
      for(CORBA::ULong i = 0; i < (unsigned int)numTeacherArrays; i++)
      {
         this->teacher_name[ i ] = CORBA::string_dup(
                                        cfdEnvironmentHandler::instance()->GetTeacher()->getFileName( i ).c_str() );
      }
   }
}

/////////////////////////////////////////////////////////////
VjObs::obj_pd* VjObs_i::getDouble1D( const char* input )
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   VjObs::obj_pd_var array1dData;
   if ( std::string( input ) ==  std::string("getCompletionTest") )
   {
      std::vector< int > completionTest;
      completionTest = cfdQuatCamHandler::instance()->getCompletionTest();
      array1dData = new VjObs::obj_pd( completionTest.size() );
      array1dData->length( completionTest.size() );
      for ( CORBA::ULong i=0; i<completionTest.size(); i++)
      {
         array1dData[i] = completionTest.at(i);
      }
   }
   else
   {
      std::cout << "|\tInput not defined : VjObs_i::getDouble1D " << input << std::endl;
   }
   return array1dData._retn();
}

VjObs::double2DArray* VjObs_i::getDouble2D( const char* input )
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   VjObs::double2DArray_var arrayData;
   if ( strcmp(input,"getFlythroughData") == 0 )
   {
      std::vector< std::vector< int > > flyThroughList;
      flyThroughList = cfdQuatCamHandler::instance()->getFlyThroughs();

      arrayData = new VjObs::double2DArray( flyThroughList.size() );
      arrayData->length( flyThroughList.size() );

      for ( CORBA::ULong i=0; i<flyThroughList.size(); i++)
      {
         arrayData[ i ].length( flyThroughList.at(i).size() );
 
         for ( CORBA::ULong j=0; j<flyThroughList.at(i).size(); j++ )
         {
            arrayData[i][j] = flyThroughList.at(i).at(j);
         }
      }
   }
   else 
   {
      std::cout << "|\tInput not defined : VjObs_i::getDouble2D " << input << std::endl;
   }
   return arrayData._retn();   
}

VjObs::scalar_p* VjObs_i::get_teacher_name()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   CreateTeacherInfo();
   VjObs::scalar_p_var teacher_name_=new VjObs::scalar_p(teacher_name);
   return teacher_name_._retn();
}

CORBA::Long VjObs_i::getIsoValue()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   //return _bufferArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   return cfdQuatCamHandler::instance()->getNumLocs();
}

void VjObs_i::setSc(const CORBA::Long value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_SC, value );
}

short VjObs_i::getPostdataState()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)

   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return (CORBA::Long)_bufferArray->GetCommandValue( cfdCommandArray::CFD_POSTDATA_STATE );
}

short VjObs_i::getTimesteps()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 2)
   //   << "Returning '" << mTimesteps << "' to caller\n" << vprDEBUG_FLUSH;
   return (CORBA::Long)_bufferArray->GetCommandValue( cfdCommandArray::CFD_TIMESTEPS );
}

// These functions are called from the java side
// Need to figure out a better notation so that this all makes sense
short VjObs_i::get_teacher_num()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL,0) << "Returning num teacher'" << this->mTeacher->getNumberOfFiles()<< "' to caller\n"
   //     << vprDEBUG_FLUSH;
   return cfdEnvironmentHandler::instance()->GetTeacher()->getNumberOfFiles();
}

void VjObs_i::GetCfdStateVariables( void )
{
   // Called in post frame to get next command out to all the cfdobjects
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   (*_cfdArray) = (*_bufferArray);
   
   for ( int i = 0; i < 9; i++ )
   {
      cfdShort_data_array[ i ] = mShort_data_array[ i ];
   } 
   
   vprDEBUG(vprDBG_ALL,3) << "|\tVjObs_i::GetCfdStateVariables Cluster Mode " 
                           << isCluster << std::endl << vprDEBUG_FLUSH;
   if ( !isCluster )
   {
      return;
   }

   vprDEBUG(vprDBG_ALL,3) << "|\tVjObs_i::GetCfdStateVariables Node Local " 
                           << mStates.isLocal() << " " 
                           << vpr::System::getHostname() 
                           << std::endl << vprDEBUG_FLUSH;
   //Do for only the master
   if ( !mStates.isLocal() )
   {
      return;
   }

   this->mStates->clusterIso_value        = _bufferArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   this->mStates->clusterSc               = _bufferArray->GetCommandValue( cfdCommandArray::CFD_SC );
   this->mStates->clusterMin              = _bufferArray->GetCommandValue( cfdCommandArray::CFD_MIN );
   this->mStates->clusterMax              = _bufferArray->GetCommandValue( cfdCommandArray::CFD_MAX );
   this->mStates->clusterId               = _bufferArray->GetCommandValue( cfdCommandArray::CFD_ID );
   this->mStates->clusterGeo_state        = _bufferArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE );
   this->mStates->clusterPostdata_state   = _bufferArray->GetCommandValue( cfdCommandArray::CFD_POSTDATA_STATE );
   this->mStates->clusterPre_state        = _bufferArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE );
   this->mStates->clusterTimesteps        = _bufferArray->GetCommandValue( cfdCommandArray::CFD_TIMESTEPS );
   this->mStates->clusterTeacher_state    = _bufferArray->GetCommandValue( cfdCommandArray::CFD_TEACHER_STATE );
   this->mStates->clusterTime_since_start = time_since_start;

   gmtl::Matrix44d matrix=VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->GetMat();

   //std::cout << "master: " << std::endl << matrix << std::endl;
   for(int i=0;i<16;i++)
   {
      this->mStates->clusterMatrix[i]=matrix.mData[i];
   }

   if ( !commandStringQueue.empty() )
   {
      std::vector< std::string >::iterator iter;
      iter = commandStringQueue.begin();
      this->mStates->clusterXMLCommands = (*iter);
      commandStringQueue.erase( iter );
   }
   else
   {
      this->mStates->clusterXMLCommands.erase();
   }
#ifdef _OSG
   if ( cfdTextureBasedVizHandler::instance()->GetActiveTextureManager() )
   {
      this->mStates->clusterFrameNumber = cfdTextureBasedVizHandler::instance()->GetActiveTextureManager()->GetCurrentFrame();   
      //std::cout<<"Master frame :"<<this->mStates->clusterFrameNumber<<std::endl;
   }
   else
   {
      this->mStates->clusterFrameNumber = 0;
   }
#endif
}

void VjObs_i::GetUpdateClusterStateVariables( void )
{
   vprDEBUG(vprDBG_ALL,3) << "|\tVjObs_i::GetUpdateClusterStateVariables Cluster Mode " 
                           << isCluster << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL,3) << "|\tVjObs_i::GetUpdateClusterStateVariables Node Local " 
                           << mStates.isLocal() << " " 
                           << vpr::System::getHostname() 
                           << std::endl << vprDEBUG_FLUSH;
    if ( !isCluster )
    {
        /*std::string commandString;
        {
            vpr::Guard<vpr::Mutex> val_guard(mValueLock);
            std::vector< std::string >::iterator iter;
            iter = commandStringQueue.begin();
            commandString = (*iter);
            commandStringQueue.erase( iter );
        }
        CreatCommandVector( commandString );*/
        return;
    }
   
   {
      vpr::Guard<vpr::Mutex> val_guard(mValueLock);
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_ISO_VALUE, this->mStates->clusterIso_value );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_SC, this->mStates->clusterSc );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_MIN, this->mStates->clusterMin );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_MAX, this->mStates->clusterMax );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_ID, this->mStates->clusterId );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_GEO_STATE, this->mStates->clusterGeo_state );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_POSTDATA_STATE, this->mStates->clusterPostdata_state );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, this->mStates->clusterPre_state );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_TIMESTEPS, this->mStates->clusterTimesteps );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_TEACHER_STATE, this->mStates->clusterTeacher_state );
   }  

    //Do for all the slaves
    if ( this->mStates->clusterXMLCommands.size() > 0 )
    {  
      CreatCommandVector( this->mStates->clusterXMLCommands.c_str() );  
    }

    if( mStates.isLocal() )
    {
        return;
    }
   //sync up the frames on all nodes in the
   //cluster
   {
      //vpr::Guard<vpr::Mutex> val_guard(mValueLock);
      gmtl::Matrix44d matrix;

      for(int i=0;i<16;i++){
         matrix.mData[i]=this->mStates->clusterMatrix[i];
      }
      //std::cout << "slave: " << std::endl << matrix << std::endl;
      VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->SetMat( matrix );

      time_since_start = this->mStates->clusterTime_since_start;
#ifdef _OSG
      if ( cfdTextureBasedVizHandler::instance()->GetActiveTextureManager() )
      {
         //std::cout<<"Updating slaves with frame :"<<this->mStates->clusterFrameNumber<<std::endl;
         cfdTextureBasedVizHandler::instance()->GetActiveTextureManager()->SetCurrentFrame( this->mStates->clusterFrameNumber);
      }
#endif
   }
}

void VjObs_i::SetClientInfoFlag( const short value )
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   this->mGetClientInfo = value;
}

VjObs::obj_pd* VjObs_i::GetClientInfoData()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   return clientInfoObserverDataArray._retn();    //check this
}

void VjObs_i::SetClientInfoData( const VjObs::obj_pd &value )
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
   //do
   //{
   //   vpr::System::msleep( 50 );  // 50 milli-second delay
   //}
   //while ( this->_unusedNewData );
   //this->_unusedNewData = true;
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   // The order of setting these values
   // MUST MATCH the order in which they are set in 
   // my_orb.java
   commandQueue.push_back( new cfdCommandArray() );
   if ( (value[ 7 ] != -1) && (value [ 8 ] != -1) )
   {
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, value[ 0 ] );
      //std::cout<<"update:this->corba_mutex.C_id = "<<this->mId<<std::endl;

      // get the value of the slider bar, used by many visualizations
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ISO_VALUE, value[ 1 ] );
      //std::cout<<"iso_value"<<this->mIso_value<<std::endl;

      //NOTE: Data is oneway transfer from
      //cfdApp -> GUI so we don't need to
      //transfer from the GUI -> cfdApp
      //DON'T set array index [2]  on the
      //GUI side
      //this->mTimesteps = value[ 2 ];

      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_SC, value[ 3 ] );
      //std::cout<<"select scalar:"<<this->mSc<<std::endl;

      // change scalar range or cursor settings
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_MIN, value[ 4 ] );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_MAX, value[ 5 ] );
      //std::cout<<"update:min,max values: "<<this->mMin<<"\t"<<this->mMax<<std::endl;

      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_GEO_STATE, value[ 6 ] );
      //std::cout<<"geometry state:"<< this->mGeo_state <<std::endl;

      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, value[ 7 ] );
      //std::cout<<"pre_state:"<< this->mPre_state <<std::endl;

      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_TEACHER_STATE, value[ 8 ] );
      //std::cout<<"mTeacher state:"<< this->mTeacher_state <<std::endl;
   }
   else
   {
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, value[ 0 ] );
      for ( int i = 0; i < 9; i ++ )
      {
         mShort_data_array[ i ] = value[ i ];
         std::cout << value[ i ] << std::endl;
      }
   }
}

void VjObs_i::PreFrameUpdate( void )
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   // If the data is transient command data
   if ( _bufferArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
            == TRANSIENT_VIS_ACTIVE )
   {
      CreateCommandQueue();
      _bufferArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
      return;
   }

   // Jsut reinitialize the cfdid to null essentially
   if ( _bufferArray->GetCommandValue( cfdCommandArray::CFD_ID ) != GUI_NAV )
      _bufferArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );

   // Populate the buffer array with the next command queue command
   if ( !commandQueue.empty() && !cfdSteadyStateVizHandler::instance()->TransientGeodesIsBusy() )
   {
      std::vector< cfdCommandArray* >::iterator iter;
      iter = commandQueue.begin();
      (*_bufferArray) = (*(*iter));
      delete commandQueue.at( 0 );
      commandQueue.erase( iter );
   }

    // New xml command queue
    if( !commandVectorQueue.empty() )
    {
        std::vector< Command* >::iterator iter;
        iter = commandVectorQueue.begin();
        (*bufferCommand) = (*(*iter));
        delete commandVectorQueue.at( 0 );
        commandVectorQueue.erase( iter );
        cfdQuatCamHandler::instance()->SetVECommand( bufferCommand );
        cfdEnvironmentHandler::instance()->GetDisplaySettings()->SetVECommand( bufferCommand );
        cfdModelHandler::instance()->SetXMLCommand( bufferCommand );
        if( cfdModelHandler::instance()->GetActiveModel() )
        {
            cfdModelHandler::instance()->GetActiveModel()->SetVECommand( bufferCommand );
        }
    }
    else
    {
        // Just reinitialize the cfdid to null essentially if it is NOT GUI_NAV
        if ( bufferCommand->GetDataValuePair( 0 )->GetDataName().compare( "GUI_NAV" ) )
            bufferCommand->SetCommandName( "wait" );
    }
 }

void VjObs_i::CreateCommandQueue( void )
{
   double newId = _bufferArray->GetCommandValue( cfdCommandArray::CFD_SC );
   double newPreState = _bufferArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE );
   double newIsoValue = _bufferArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   
   //if we are doing transient vis then we already have an active model and dataset
   int activeVector = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetActiveVector();
   int activeScalar = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetActiveScalar();

   double activeMinMax[ 2 ];
   cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetRange( activeMinMax );

   std::map< int, cfdDataSet* >::iterator iter;
   
   commandQueue.push_back( new cfdCommandArray() );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, TRANSIENT_ACTIVE );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, 0 );

   /*for ( iter = cfdModelHandler::instance()->GetActiveModel()->transientDataSets.begin(); 
         iter != cfdModelHandler::instance()->GetActiveModel()->transientDataSets.end(); ++iter)
   { 
      // Set the active datasets
      commandQueue.push_back( new cfdCommandArray() );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, CHANGE_STEADYSTATE_DATASET );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ISO_VALUE, iter->first );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_SC, activeScalar);
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_MIN, activeMinMax[ 0 ] );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_MAX, activeMinMax[ 1 ] );
      // Set active Vector
      commandQueue.push_back( new cfdCommandArray() );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, CHANGE_VECTOR );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_SC,  activeVector);
      // Set current viz      
      commandQueue.push_back( new cfdCommandArray() );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, newId );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ISO_VALUE, newIsoValue );
      commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, newPreState );
   }*/

   commandQueue.push_back( new cfdCommandArray() );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, TRANSIENT_ACTIVE );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, 1 );
}
////////////////////////////////////////////////////////////////////////////////
void VjObs_i::SetCommandString( const char* value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
{
    //When starting xplorer it is possible to connect and send a command before 
    // xplorer is ready to receive it
    while ( !jccl::ConfigManager::instance()->isPendingStale() )
    {
        vpr::System::msleep( 50 );  // 50 milli-second delay
    }

    std::string commandString( value );
    if( isCluster )
    {
        if( mStates.isLocal() )
        {
            vpr::Guard<vpr::Mutex> val_guard(mValueLock);
            commandStringQueue.push_back( commandString );
        }
    }
    else
    {
        //commandStringQueue.push_back( commandString );
        CreatCommandVector( commandString );
    }
}
////////////////////////////////////////////////////////////////////////////////
void VjObs_i::CreatCommandVector( std::string commandString )
{
    //This function is called from juggler threads
    vprDEBUG(vprDBG_ALL,2) <<"VjObs::SetCommandString(): "<< std::endl << commandString << std::endl << vprDEBUG_FLUSH;
    vpr::Guard<vpr::Mutex> val_guard(mValueLock);
    VE_XML::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( commandString, "Command", "vecommand" );
    std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

    for ( size_t i = 0; i < objectVector.size(); ++i )
    {
        commandVectorQueue.push_back( static_cast< VE_XML::Command* >( objectVector.at( i ) ) );
    }
}
// Frame sync variables used by osg only at this point
float VjObs_i::GetSetAppTime( float x )
{
   if ( x == -1 )
   {
      return time_since_start;
   }
   else
   {
      time_since_start = x;
      return -1;
   }
}

long VjObs_i::GetSetFrameNumber( long x )
{
   if ( x == -1 )
   {
      return frameNumber;
   }
   else
   {
      frameNumber = x;
      return -1;
   }
}

