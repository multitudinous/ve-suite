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
 * File:          $RCSfile: VjObs_i.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VjObs_i.h"

#include "cfdTeacher.h"
#include "cfdDataSet.h"
#include "cfdModelHandler.h"
#include "cfdEnvironmentHandler.h"
#include "cfdSteadyStateVizHandler.h"
#include "cfdSoundHandler.h"
#include "cfdModel.h"
#include "cfdFILE.h"
#include "cfdEnum.h"

#ifdef _OSG
#include "cfdTextureBasedVizHandler.h"
#endif

#include <vpr/System.h>
#include <vpr/Util/Debug.h>

#include <iostream>
#include <map>

/*
#ifndef WIN32
#include <sys/time.h>
#else
#include <time.h>
#endif
*/

void VjObs_i::InitCluster( void )
{
#ifdef _CLUSTER
  // Cluster Stuff
   vpr::GUID new_guid("15c09c99-ed6d-4994-bbac-83587d4400d1");
   std::string hostname = "abbott.vrac.iastate.edu";
   this->mStates.init(new_guid,hostname);
   //cluster::ApplicationData* hack = dynamic_cast<cluster::ApplicationData*>(&(*this->mStates));
   //hack->setIsLocal(hostname == cluster::ClusterNetwork::instance()->getLocalHostname());
#endif // _CLUSTER
}
#ifdef _OSG
void VjObs_i::SetHandlers( cfdSteadyStateVizHandler* ssHandler, 
                     cfdEnvironmentHandler* envHandler, 
                     cfdModelHandler* modelHandler,
                     cfdTextureBasedVizHandler* tbvHandler)
{
   _ssHandler = ssHandler;
   _envHandler = envHandler;
   _modelHandler = modelHandler;
   _tbvHandler = tbvHandler;
}
#else
void VjObs_i::SetHandlers( cfdSteadyStateVizHandler* ssHandler, 
                     cfdEnvironmentHandler* envHandler, 
                     cfdModelHandler* modelHandler)
{
   _ssHandler = ssHandler;
   _envHandler = envHandler;
   _modelHandler = modelHandler;
}
#endif

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
   CORBA::ULong numberOfModels = this->_modelHandler->GetNumberOfModels();
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
         cfdModel* temp = this->_modelHandler->GetModel( i );
         CORBA::ULong numDatasets = temp->GetNumberOfCfdDataSets();
         vprDEBUG(vprDBG_ALL,0) << " numDatasets = " << numDatasets
                          << std::endl << vprDEBUG_FLUSH;
         if ( numDatasets > 0 )
         {
            (*_models)[ i ].datasetnames = VjObs::scalar_p( numDatasets ); 
            (*_models)[ i ].datasetnames.length( numDatasets );

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
               //std::cout << i << "\t" << this->mParamReader->GetDataSet( i )->GetNumberOfScalars() << std::endl;
               totalNumberOfScalars += temp->GetCfdDataSet( j )->GetNumberOfScalars();
               totalNumberOfVectors += temp->GetCfdDataSet( j )->GetNumberOfVectors();
            }
            vprDEBUG(vprDBG_ALL,0)
               << " totalNumberOfScalars: " << totalNumberOfScalars
               << std::endl << vprDEBUG_FLUSH;

            vprDEBUG(vprDBG_ALL,0)
               << " totalNumberOfVectors: " << totalNumberOfVectors
               << std::endl << vprDEBUG_FLUSH;

            (*_models)[ i ].scalarnames = VjObs::scalar_p( totalNumberOfScalars );
            (*_models)[ i ].scalarnames.length( totalNumberOfScalars );

            (*_models)[ i ].vectornames = VjObs::scalar_p( totalNumberOfVectors );
            (*_models)[ i ].vectornames.length( totalNumberOfVectors );

            CORBA::ULong sIndex = 0;
            CORBA::ULong vIndex = 0;
            for ( CORBA::ULong j=0; j < numDatasets; j++ )
            {
               (*_models)[ i ].datasetnames[ j ] = CORBA::string_dup( 
                          temp->GetCfdDataSet( j )->GetFileName() );
               vprDEBUG(vprDBG_ALL,1) << " dataset_name:   " << (*_models)[ i ].datasetnames[ j ]
                             << std::endl << vprDEBUG_FLUSH;

               (*_models)[ i ].datasettypes[ j ] = temp->GetCfdDataSet( j )->GetType();
   
               CORBA::Short num_scalars = temp->GetCfdDataSet( j )->GetNumberOfScalars();
               (*_models)[ i ].num_scalars_per_dataset[ j ] = num_scalars;

               for (CORBA::ULong k=0; k < (unsigned int)num_scalars; k++ )
               {
                  (*_models)[ i ].scalarnames[ sIndex ] = CORBA::string_dup(
                    temp->GetCfdDataSet( j )->GetScalarName( k ) );
                  vprDEBUG(vprDBG_ALL,1) << "\tscl_name " 
                                << sIndex << " : " << (*_models)[ i ].scalarnames[ sIndex ]
                                << std::endl << vprDEBUG_FLUSH;
                  sIndex++;
               }

               CORBA::Short num_vectors = temp->GetCfdDataSet( j )
                                          ->GetNumberOfVectors();
               (*_models)[ i ].num_vectors_per_dataset[ j ] = num_vectors;

               for (CORBA::ULong k=0; k < (unsigned int)num_vectors; k++ )
               {
                  (*_models)[ i ].vectornames[ vIndex ] = CORBA::string_dup(
                    temp->GetCfdDataSet( j )->GetVectorName( k ) );
                  vprDEBUG(vprDBG_ALL,1) << "\tvec_name " 
                                << vIndex << " : " << (*_models)[ i ].vectornames[ vIndex ]
                                << std::endl << vprDEBUG_FLUSH;
                  vIndex++;
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
                                  temp->GetGeomDataSet( j )->GetFilename() );
            }
         }
      }
   }

   vprDEBUG(vprDBG_ALL,1) << "leaving VjObs_i::CreateDatasetInfo()"
                          << std::endl << vprDEBUG_FLUSH;

}

/////////////////////////////////////////////////////////////
void VjObs_i::CreateTeacherInfo( void )
{   
   CORBA::Short numTeacherArrays = this->_envHandler->GetTeacher()->getNumberOfFiles();
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
                                        this->_envHandler->GetTeacher()->getFileName( i ) );
      }
   }
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
   VjObs::scalar_p_var teacher_name_=new VjObs::scalar_p(teacher_name);
   return teacher_name_._retn();
}

#ifdef _TAO
char* VjObs_i::get_perf()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else    
char* VjObs_i::get_perf()
#endif
{
   return CORBA::string_dup("abc");
}

#ifdef _TAO
void VjObs_i::setIsoValue(const CORBA::Long value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setIsoValue(CORBA::Long value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_ISO_VALUE, value );
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
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
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
CORBA::Long VjObs_i::getSc()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Long VjObs_i::getSc()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_SC );
}

#ifdef _TAO
void VjObs_i::setMin(const CORBA::Long value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setMin(CORBA::Long value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_MIN, value );
}

#ifdef _TAO
CORBA::Long VjObs_i::getMin()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Long VjObs_i::getMin()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_MIN );
}

#ifdef _TAO
void VjObs_i::setMax(const CORBA::Long value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setMax(CORBA::Long value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_MAX, value );
}

#ifdef _TAO
CORBA::Long VjObs_i::getMax()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Long VjObs_i::getMax()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_MAX );
}

#ifdef _TAO
void VjObs_i::setId(const CORBA::Long value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setId(CORBA::Long value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_ID, value );
}

#ifdef _TAO
CORBA::Long VjObs_i::getId()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Long VjObs_i::getId()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_ID );
}

#ifdef _TAO
void VjObs_i::setGeoState(const CORBA::Long value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setGeoState(CORBA::Long value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_GEO_STATE, value );
}

#ifdef _TAO
CORBA::Long VjObs_i::getGeoState()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Long VjObs_i::getGeoState()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE );
}

#ifdef _TAO
void VjObs_i::setPostdataState(const short value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setPostdataState(CORBA::Short value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_POSTDATA_STATE, value );
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
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_POSTDATA_STATE );
}

#ifdef _TAO
void VjObs_i::setPreState(const short value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setPreState(CORBA::Short value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, value );
}

#ifdef _TAO
short VjObs_i::getPreState()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Short VjObs_i::getPreState()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE );
}

#ifdef _TAO
void VjObs_i::setTimesteps(const short value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setTimesteps(CORBA::Short value)
#endif
{
   vprDEBUG(vprDBG_ALL, 2)
      << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_TIMESTEPS, value );
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
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_TIMESTEPS );
}

#ifdef _TAO
void VjObs_i::setNumTeacherArrays(const short value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setNumTeacherArrays(CORBA::Short value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mNumTeacherArrays = value;
}

#ifdef _TAO
short VjObs_i::getNumTeacherArrays()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Short VjObs_i::getNumTeacherArrays()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return this->_envHandler->GetTeacher()->getNumberOfFiles();
}

#ifdef _TAO
void VjObs_i::setTeacherState(const short value)
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
void VjObs_i::setTeacherState(CORBA::Short value)
#endif
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   _bufferArray->SetCommandValue( cfdCommandArray::CFD_TEACHER_STATE, value );
}

#ifdef _TAO
short VjObs_i::getTeacherState()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
CORBA::Short VjObs_i::getTeacherState()
#endif
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return _bufferArray->GetCommandValue( cfdCommandArray::CFD_TEACHER_STATE );
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
   return this->_envHandler->GetTeacher()->getNumberOfFiles();
}

void VjObs_i::GetCfdStateVariables( void )
{

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
   }
#else
   this->_unusedNewData    = false;
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
      if ( this->_ssHandler->GetActiveAnimation() != NULL )
      {
         cfdTempAnimation* the_sequence = this->_ssHandler->GetActiveAnimation();
         if ( the_sequence != NULL )
         {
            the_sequence->SetCurrentFrame( (int)this->getTimesteps() );
            //std::cout << " cfdTimesteps in preframe : " << cfdTimesteps << std::endl;
         }
      }
   }
   this->_unusedNewData    = false;
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
   return this->_envHandler->GetSoundHandler()->GetNumberOfSounds();
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
   int numberOfSounds = this->_envHandler->GetSoundHandler()->GetNumberOfSounds();

   vprDEBUG(vprDBG_ALL,0) << " Number of Sounds to be transfered to client: " 
                          << numberOfSounds << std::endl << vprDEBUG_FLUSH;

   if ( &sound_names != NULL )
   {
      delete &sound_names;
   }

   if( numberOfSounds > 0 )
   {
      sound_names = new VjObs::scalar_p(50);
      this->sound_names->length( numberOfSounds );
      for(CORBA::ULong i = 0; i < (unsigned int)numberOfSounds; i++)
      {
         this->sound_names[ i ] = CORBA::string_dup( 
                     this->_envHandler->GetSoundHandler()->GetSoundFilename( i ) );
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
VjObs::obj_p* VjObs_i::GetClientInfoData()
  ACE_THROW_SPEC ((
    CORBA::SystemException
  ))
#else
VjObs::obj_p* VjObs_i::GetClientInfoData()
#endif
{
   return clientInfoObserverDataArray._retn();
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
   do
   {
      vpr::System::msleep( 50 );  // 50 milli-second delay
   }
   while ( this->_unusedNewData );

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   // The order of setting these values
   // MUST MATCH the order in which they are set in 
   // my_orb.java
   if ( (value[ 7 ] != -1) && (value [ 8 ] != -1) )
   {
      _bufferArray->SetCommandValue( cfdCommandArray::CFD_ID, value[ 0 ] );
      //std::cout<<"update:this->corba_mutex.C_id = "<<this->mId<<std::endl;

      // get the value of the slider bar, used by many visualizations
      _bufferArray->SetCommandValue( cfdCommandArray::CFD_ISO_VALUE, value[ 1 ] );
      //std::cout<<"iso_value"<<this->mIso_value<<std::endl;

      //NOTE: Data is oneway transfer from
      //cfdApp -> GUI so we don't need to
      //transfer from the GUI -> cfdApp
      //DON'T set array index [2]  on the
      //GUI side
      //this->mTimesteps = value[ 2 ];

      _bufferArray->SetCommandValue( cfdCommandArray::CFD_SC, value[ 3 ] );
      //std::cout<<"select scalar:"<<this->mSc<<std::endl;

      // change scalar range or cursor settings
      _bufferArray->SetCommandValue( cfdCommandArray::CFD_MIN, value[ 4 ] );
      _bufferArray->SetCommandValue( cfdCommandArray::CFD_MAX, value[ 5 ] );
      //std::cout<<"update:min,max values: "<<this->mMin<<"\t"<<this->mMax<<std::endl;

      _bufferArray->SetCommandValue( cfdCommandArray::CFD_GEO_STATE, value[ 6 ] );
      //std::cout<<"geometry state:"<< this->mGeo_state <<std::endl;

      _bufferArray->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, value[ 7 ] );
      //std::cout<<"pre_state:"<< this->mPre_state <<std::endl;

      _bufferArray->SetCommandValue( cfdCommandArray::CFD_TEACHER_STATE, value[ 8 ] );
      //std::cout<<"mTeacher state:"<< this->mTeacher_state <<std::endl;
   }
   else
   {
      _bufferArray->SetCommandValue( cfdCommandArray::CFD_ID, value[ 0 ] );
      for ( int i = 0; i < 9; i ++ )
      {
         mShort_data_array[ i ] = value[ i ];
         std::cout << value[ i ] << std::endl;
      }
   }
   this->_unusedNewData = true;
}

void VjObs_i::PreFrameUpdate( void )
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   if ( _bufferArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
            == TRANSIENT_VIS_ACTIVE )
   {
      CreateCommandQueue();
      _bufferArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
      return;
   }

   if ( _bufferArray->GetCommandValue( cfdCommandArray::CFD_ID ) != GUI_NAV )
      _bufferArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   if ( !commandQueue.empty() && !_ssHandler->TransientGeodesIsBusy() )
   {
   std::cout << commandQueue.empty() << " : " << _ssHandler->TransientGeodesIsBusy() << std::endl;
      (*_bufferArray) = (*(*commandQueue.begin()));
      delete commandQueue.at( 0 );
      commandQueue.erase( commandQueue.begin() );
   }
}

void VjObs_i::CreateCommandQueue( void )
{
   int newId = _bufferArray->GetCommandValue( cfdCommandArray::CFD_SC );
   int newPreState = _bufferArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE );
   int newIsoValue = _bufferArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   
   int activeVector = _modelHandler->GetActiveDataSet()->GetActiveVector();
   int activeScalar = _modelHandler->GetActiveDataSet()->GetActiveScalar();

   int activeMinMax[ 2 ];
   _modelHandler->GetActiveDataSet()->GetRange( activeMinMax );

   std::map< int, cfdDataSet* >::iterator iter;
   
   commandQueue.push_back( new cfdCommandArray() );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, TRANSIENT_ACTIVE );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, 0 );

   for ( iter = _modelHandler->GetActiveModel()->transientDataSets.begin(); 
         iter != _modelHandler->GetActiveModel()->transientDataSets.end(); ++iter)
   { 
      std::cout << iter->first << std::endl;
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

   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, TRANSIENT_ACTIVE );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, 1 );
}
