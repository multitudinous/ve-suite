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
 * File:          $RCSfile: VjObs_i.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/impl/VjObs_i.h"
#include "VE_Open/XML/DOMDocumentManager.h"

#include "VE_Xplorer/cfdTeacher.h"
#include "VE_Xplorer/cfdQuatCamHandler.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/cfdModelHandler.h"
#include "VE_Xplorer/cfdEnvironmentHandler.h"
#include "VE_Xplorer/cfdModelHandler.h"
#include "VE_Xplorer/cfdSteadyStateVizHandler.h"
#include "VE_Xplorer/cfdSoundHandler.h"
#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdNavigate.h"
#include "VE_Xplorer/cfdDisplaySettings.h"
#include "VE_Xplorer/cfdCommandArray.h"

#include "VE_SceneGraph/cfdTempAnimation.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#ifdef _OSG
#ifdef VE_PATENTED
#include "VE_TextureBased/cfdTextureBasedVizHandler.h"
#include "VE_TextureBased/cfdVolumeVisualization.h"
using namespace VE_TextureBased;
#endif
#endif

#include <vpr/System.h>
#include <vpr/Util/Debug.h>

#include <iostream>
#include <map>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_XML;

VjObs_i::VjObs_i()
{
   this->numOfClientInfo = 9;
   //int temp=0;
   //this->setClients( 0 );
   _cfdArray = new cfdCommandArray();
   _cfdArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   _bufferArray = new cfdCommandArray();
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   //orb=CORBA::ORB_init(temp,0,"omniORB4");
   /*for(temp=0;temp<25;temp++)
   {
      client_list[temp]=0;
   }*/
   // allocate enough space
   //geo_name       = new VjObs::scalar_p(50);
   //geo_name->length(50);
   //scl_name       = new VjObs::scalar_p(50);
   //scl_name->length(50);
   teacher_name   = new VjObs::scalar_p(50);
   teacher_name->length(50);
   //dataset_names  = new VjObs::scalar_p(50);
   //dataset_names->length(50);
   sound_names    = new VjObs::scalar_p(50);
   sound_names->length(50);

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
   domManager = new DOMDocumentManager();
   domManager->SetParseXMLStringOn();
}

void VjObs_i::InitCluster( void )
{
#ifdef _CLUSTER
  // Cluster Stuff
   vpr::GUID new_guid("15c09c99-ed6d-4994-bbac-83587d4400d1");
   this->mStates.init(new_guid);
#endif // _CLUSTER
}

#ifdef _TAO
VjObs::Models* VjObs_i::GetModels()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
VjObs::Models* VjObs_i::GetModels()
#endif
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

#ifdef _TAO
VjObs::obj_pd* VjObs_i::getDouble1D( const char* input )
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
VjObs::obj_pd* VjObs_i::getDouble1D(  const char* input )
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   VjObs::obj_pd_var array1dData;
   if ( strcmp(input,"getCompletionTest") == 0 )
   {
      std::vector< int > completionTest;
      completionTest = cfdEnvironmentHandler::instance()->GetQuatCamHandler()->getCompletionTest();
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

#ifdef _TAO
VjObs::double2DArray* VjObs_i::getDouble2D( const char* input )
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
VjObs::double2DArray* VjObs_i::getDouble2D(  const char* input )
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   VjObs::double2DArray_var arrayData;
   if ( strcmp(input,"getFlythroughData") == 0 )
   {
      std::vector< std::vector< int > > flyThroughList;
      flyThroughList = cfdEnvironmentHandler::instance()->GetQuatCamHandler()->getFlyThroughs();

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

#ifdef _TAO
VjObs::scalar_p* VjObs_i::get_teacher_name()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
VjObs::scalar_p* VjObs_i::get_teacher_name()
#endif
{
   CreateTeacherInfo();
   VjObs::scalar_p_var teacher_name_=new VjObs::scalar_p(teacher_name);
   return teacher_name_._retn();
}

#ifdef _TAO
CORBA::Long VjObs_i::getIsoValue()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Long VjObs_i::getIsoValue()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   //return _bufferArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   return cfdEnvironmentHandler::instance()->GetQuatCamHandler()->getNumLocs();
}

#ifdef _TAO
void VjObs_i::setSc(const CORBA::Long value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setSc(CORBA::Long value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_SC, value );
}

#ifdef _TAO
short VjObs_i::getPostdataState()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Short VjObs_i::getPostdataState()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)

   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return (CORBA::Long)_bufferArray->GetCommandValue( cfdCommandArray::CFD_POSTDATA_STATE );
}

#ifdef _TAO
short VjObs_i::getTimesteps()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Short VjObs_i::getTimesteps()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 2)
   //   << "Returning '" << mTimesteps << "' to caller\n" << vprDEBUG_FLUSH;
   return (CORBA::Long)_bufferArray->GetCommandValue( cfdCommandArray::CFD_TIMESTEPS );
}

// These functions are called from the java side
// Need to figure out a better notation so that this all makes sense
#ifdef _TAO
short VjObs_i::get_teacher_num()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Short VjObs_i::get_teacher_num()
#endif
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

#ifdef _CLUSTER
   if ( mStates.isLocal() )
   {
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
#ifdef _OSG
#ifdef VE_PATENTED      
      if ( cfdTextureBasedVizHandler::instance()->GetActiveVolumeVizNode() )
      {
         this->mStates->clusterFrameNumber   = cfdTextureBasedVizHandler::instance()->GetActiveVolumeVizNode()->GetCurrentTransientTexture();   
      }
      else
      {
         this->mStates->clusterFrameNumber = 0;
      }
#endif
#endif
      if ( cfdEnvironmentHandler::instance()->GetQuatCamHandler()->IsActive() )
      {
         this->mStates->clusterQuatCamIncrement = cfdEnvironmentHandler::instance()->GetQuatCamHandler()->GetQuatCamIncrementor();
      }
      else
      {
         this->mStates->clusterQuatCamIncrement = 0;
      }
   }
   //this->_unusedNewData    = false;
#else
   //this->_unusedNewData    = false;
#endif
}

void VjObs_i::GetUpdateClusterStateVariables( void )
{
#ifdef _CLUSTER
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

   //sync up the frames on all nodes in the
   //cluster
   if ( !mStates.isLocal() )
   {
      if ( cfdSteadyStateVizHandler::instance()->GetActiveAnimation() != NULL )
      {
         cfdTempAnimation* the_sequence = cfdSteadyStateVizHandler::instance()->GetActiveAnimation();
         if ( the_sequence != NULL )
         {
            the_sequence->SetCurrentFrame( (int)this->getTimesteps() );
            //std::cout << " cfdTimesteps in preframe : " << cfdTimesteps << std::endl;
         }
      }
      time_since_start = this->mStates->clusterTime_since_start;
#ifdef _OSG
#ifdef VE_PATENTED
      if ( cfdTextureBasedVizHandler::instance()->GetActiveVolumeVizNode() )
      {
	      cfdTextureBasedVizHandler::instance()->GetActiveVolumeVizNode()->SetCurrentTransientTexture( this->mStates->clusterFrameNumber);
      }
#endif
#endif
      if ( cfdEnvironmentHandler::instance()->GetQuatCamHandler()->IsActive() )
      {
         cfdEnvironmentHandler::instance()->GetQuatCamHandler()->SetQuatCamIncrementor( this->mStates->clusterQuatCamIncrement );
      }
   }
   //this->_unusedNewData    = false;
#endif
}

#ifdef _TAO
short VjObs_i::GetNumberOfSounds()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Short VjObs_i::GetNumberOfSounds()
#endif
{
   return cfdEnvironmentHandler::instance()->GetSoundHandler()->GetNumberOfSounds();
}

#ifdef _TAO
VjObs::scalar_p* VjObs_i::GetSoundNameArray()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
VjObs::scalar_p* VjObs_i::GetSoundNameArray()
#endif
{
   CORBA::ULong numberOfSounds = cfdEnvironmentHandler::instance()->GetSoundHandler()->GetNumberOfSounds();

   vprDEBUG(vprDBG_ALL,0) << " Number of Sounds to be transfered to client: " 
                          << numberOfSounds << std::endl << vprDEBUG_FLUSH;
/*
   if ( sound_names != NULL )
   {
      vprDEBUG(vprDBG_ALL,1) << " deleting sound_names"
                             << std::endl << vprDEBUG_FLUSH;
      delete sound_names;
   }
*/
   if ( numberOfSounds > 0 )
   {
      //sound_names = new VjObs::scalar_p(50);
      this->sound_names->length( numberOfSounds );
      for ( CORBA::ULong i = 0; i < numberOfSounds; i++)
      {
         this->sound_names[ i ] = CORBA::string_dup( 
                     cfdEnvironmentHandler::instance()->GetSoundHandler()->GetSoundFilename( i ).c_str() );
      }
   }
   else
      sound_names = NULL;

   VjObs::scalar_p_var sound_names_=new VjObs::scalar_p(sound_names);
   return sound_names_._retn();
}

#ifdef _TAO
void VjObs_i::SetClientInfoFlag( const short value )
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::SetClientInfoFlag( CORBA::Short value )
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   this->mGetClientInfo = value;
}

#ifdef _TAO
VjObs::obj_pd* VjObs_i::GetClientInfoData()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
VjObs::obj_pd* VjObs_i::GetClientInfoData()
#endif
{
   return clientInfoObserverDataArray._retn();    //check this
}

#ifdef _TAO
void VjObs_i::SetClientInfoData( const VjObs::obj_pd &value )
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::SetClientInfoData( const VjObs::obj_pd &value )
#endif
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

   // Just reinitialize the cfdid to null essentially if it is NOT GUI_NAV
   if(bufferCommand->GetNumberOfDataValuePairs()){
      if ( bufferCommand->GetDataValuePair( 0 )->GetDataName().compare( "GUI_NAV" ) )
         bufferCommand->SetCommandName( "wait" );
   }

   // New xml command queue
   if ( !commandVectorQueue.empty() )
   {
      std::vector< Command* >::iterator iter;
      iter = commandVectorQueue.begin();
      (*bufferCommand) = (*(*iter));
      delete commandVectorQueue.at( 0 );
      commandVectorQueue.erase( iter );
      cfdEnvironmentHandler::instance()->GetNavigate()->SetVECommand( bufferCommand );
      cfdEnvironmentHandler::instance()->GetQuatCamHandler()->SetVECommand( bufferCommand );
      cfdEnvironmentHandler::instance()->GetDisplaySettings()->SetVECommand( bufferCommand );
      if ( cfdModelHandler::instance()->GetActiveModel() )
      cfdModelHandler::instance()->GetActiveModel()->SetVECommand( bufferCommand );
      //std::cout << " command name " << bufferCommand->GetCommandName() << std::endl;
   }
}

void VjObs_i::CreateCommandQueue( void )
{
   double newId = _bufferArray->GetCommandValue( cfdCommandArray::CFD_SC );
   double newPreState = _bufferArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE );
   double newIsoValue = _bufferArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   
   int activeVector = cfdModelHandler::instance()->GetActiveDataSet()->GetActiveVector();
   int activeScalar = cfdModelHandler::instance()->GetActiveDataSet()->GetActiveScalar();

   int activeMinMax[ 2 ];
   cfdModelHandler::instance()->GetActiveDataSet()->GetRange( activeMinMax );

   std::map< int, cfdDataSet* >::iterator iter;
   
   commandQueue.push_back( new cfdCommandArray() );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, TRANSIENT_ACTIVE );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, 0 );

   for ( iter = cfdModelHandler::instance()->GetActiveModel()->transientDataSets.begin(); 
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
   }

   commandQueue.push_back( new cfdCommandArray() );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, TRANSIENT_ACTIVE );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, 1 );
}

#ifdef _TAO
void VjObs_i::SetCommandString( const char* value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::SetCommandString( const char* value)
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);

   std::string commandString( value );
   domManager->Load( commandString );
   std::cout << commandString << std::endl;
   DOMDocument* commandDoc = domManager->GetCommandDocument();

   // Get a list of all the command elements
   DOMNodeList* subElements = commandDoc->getDocumentElement()->getElementsByTagName( xercesString("vecommand") );
   unsigned int numCommands = subElements->getLength();
   // now lets create a list of them
   for ( unsigned int i = 0; i < numCommands; ++i )
   {
      Command* temp = new Command(  );
      DOMElement* tempElement = 0;
      try
      {
         tempElement = dynamic_cast< DOMElement* >( subElements->item(i) );
      }
      catch ( ... )
      {
         std::cerr << "ERROR : dynamic_cast failed. You may need to recompile xerces with RTTI enabled" << std::endl;
         return;
      }
      temp->SetObjectFromXMLData( tempElement );
      commandVectorQueue.push_back( temp );
   }
   // I am pretty sure we now need to release some memory for the domdocument
   domManager->UnLoadParser();
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

