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
 * Date modified: $Date: 2004/04/25 06:42:42 $
 * Version:       $Revision: 1.12 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <VjObs_i.h>
#include <iostream>
#ifndef WIN32
#include <sys/time.h>
#else
#include <time.h>
#endif
//#include <vpr/vpr.h>
//#include <vpr/Sync/Mutex.h>

#include "cfdReadParam.h"
#include "cfdTeacher.h"
#include "cfdDataSet.h"
#include "cfdFileInfo.h"

#ifdef _CLUSTER
#include "cfdSequence.h"
#endif

#include <vtkSystemIncludes.h>  // for VTK_POLY_DATA
#include <vtkDataSet.h>
#include <vtkPolyData.h>
#include <vtkCellTypes.h>
/*
void VjObs_i::attach(Observer_ptr o)
{
   static int clients = 0;
   //corba_mutex.vjObsFpull();  //yang-REI: you want to get the new value by force
   CORBA::String_var p=orb->object_to_string(o);
   std::cout << "|   IOR of the client : " << std::endl << p << std::endl;
   CORBA::Object_var obj=orb->string_to_object(p);
   Observer_var obsvar=Observer::_narrow(obj);
   //Observer_ptr obsvar=Observer::_narrow(o);
   if(CORBA::is_nil(obsvar))
   {
      std::cerr<<"Can't invoke on nil object reference\n"<<std::endl;
   }
   
   std::cout << "|   Client " << clients <<" connected!" << std::endl;
   client_list[ clients ] = obsvar;
   
   clients++;

   // This array is used in place of the call backs
   // to the client because the communication didn't
   // seem to work. There are 9 entries becuase that 
   // how many variables are synchronized during an
   // update call in VjObs_i
   clientInfoObserverDataArray = new VjObs::obj_p(50);
   clientInfoObserverDataArray->length( this->numOfClientInfo );

   baf_param = Observer::baf_p_alloc();
   this->setClients( clients );
   //corba_mutex.vjObsFpush();  //yang-REI, be sure the number of clients is pushed
}

void VjObs_i::detach(Observer_ptr o)
{
   int i,j;
   //corba_mutex.snapShot2(false); //yang-REI: Get the updated value
   Observer::baf_p_free(baf_param);

   CORBA::String_var p=orb->object_to_string(o);

   CORBA::Object_var obj=orb->string_to_object(p);
   Observer_var obsvar=Observer::_narrow(obj);
   if(CORBA::is_nil(obsvar))
   {
      std::cerr<<"Can't invoke on nil object reference\n"<<std::endl;
   }
   int clients = this->getClients();

   for( i=0; i < clients; i++ )
   {   
      if(obsvar->_is_equivalent(client_list[ i ]))
      {
         for(j=i; j < clients-1;j++)
         {
            client_list[j]=client_list[j+1];      
         }
         client_list[j]=0;
         clients--;   
         //this decrement should be reflected in next attach. --changed by Song Li
         std::cerr<<"Client "<<i<<" disconnected!\n"<<std::endl;
      }
   }
   this->setClients( clients );
}
*/
void VjObs_i::update()
{
/*   int clients = this->getClients();
   cout<<"update:clients = "<<clients<<endl;

   if(clients != 0)
   {
      vpr::Guard<vpr::Mutex> val_guard(mValueLock);
      for(int i=0;i<clients;i++)
      {
         cout << client_list[ i ] << endl;
         //this->setId( client_list[ i ]->update() );
         this->mId = client_list[ i ]->update();
         cout<<"update:this->corba_mutex.C_id = "<<this->mId<<endl;

         // get the value of the slider bar, used by many visualizations
         //this->setIsoValue( client_list[ i ]->get_iso_value() );
         this->mIso_value = client_list[ i ]->get_iso_value();
         cout<<"iso_value"<<this->mIso_value<<endl;

         //this->setTimesteps( client_list[ i ]->get_timesteps() );
         this->mTimesteps = client_list[ i ]->get_timesteps();
         //this->setSc( client_list[ i ]->get_sc() );
         this->mSc = client_list[ i ]->get_sc();
         cout<<"select scalar:"<<this->mSc<<endl;
         // change scalar range or cursor settings
         //this->setMin( client_list[ i ]->get_min() );
         this->mMin = client_list[ i ]->get_min();
         //this->setMax( client_list[ i ]->get_max() );
         this->mMax = client_list[ i ]->get_max();
         cout<<"update:min,max values: "<<this->mMin<<"\t"<<this->mMax<<endl;
         //this->setGeoState( client_list[ i ]->get_geo_state() );
         this->mGeo_state = client_list[ i ]->get_geo_state();
         //cout<<"geometry state:"<<geo_state<<endl;
         //this->setPreState( client_list[ i ]->get_pre_state() );
         this->mPre_state = client_list[ i ]->get_pre_state();
         //cout<<"pre_state:"<<pre_state<<endl;
         //baf_param = client_list[ i ]->get_baf_param();
         //for(int j = 0; j < 15; j++)
         //{
            //cout<<"param "<<j<<":"<<baf_param[j]<<endl;
         //}
         //this->setTeacherState( client_list[ i ]->get_teacher_state() );
         this->mTeacher_state = client_list[ i ]->get_teacher_state();
         //cout<<"mTeacher state:"<<mTeacher_state<<endl;
      }
   }
   else
   {
      std::cout<<"No clients connected!\n"<<std::endl;
   }*/
}

VjObs::scalar_p* VjObs_i::update_scalar()
{
   return this->scl_name._retn();
}

VjObs::scalar_p* VjObs_i::update_vector()
{
   return this->vec_name._retn();
}

VjObs::scalar_p* VjObs_i::get_geo_name()
{
   return this->geo_name._retn();
}

VjObs::scalar_p* VjObs_i::get_teacher_name()
{
   return this->teacher_name._retn();
}
/*
void VjObs_i::put_cur_obj(Observer::obj_p_var o)
{
   int clients = mClients;

   if(clients!=0)
   {
      for(int i=0;i<clients;i++)
         client_list[ i ]->put_cur_obj(o);
   }
}
*/    
char* VjObs_i::get_perf()
{
   return CORBA::string_dup("abc");
}
/*
short VjObs_i::get_postdata()
{
   corba_mutex.snapShot2(false);
   return corba_mutex.C_postdata_state;
}

short VjObs_i::get_timesteps()
{
   corba_mutex.snapShot2(false);
   return corba_mutex.C_timesteps;
}
*/
VjObs::scalar_p * VjObs_i::get_dataset_names()
{
   return this->dataset_names._retn();
}

VjObs::obj_p * VjObs_i::get_dataset_types()
{
   return this->dataset_types._retn();
}

VjObs::obj_p * VjObs_i::get_num_scalars_per_dataset()
{
   return this->num_scalars_per_dataset._retn();
}

VjObs::obj_p * VjObs_i::get_num_vectors_per_dataset()
{
   return this->num_vectors_per_dataset._retn();
}

void VjObs_i::setNumDatasets(CORBA::Short value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mNumScalars = value;
}

CORBA::Short VjObs_i::getNumDatasets()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL,0) << "Returning '" << mNumScalars << "' to caller\n"
   //     << vprDEBUG_FLUSH;
   return mNumScalars;
}

CORBA::Short VjObs_i::getTotalNumberOfScalars()
{
   return this->totalNumberOfScalars;
}

CORBA::Short VjObs_i::getTotalNumberOfVectors()
{
   return this->totalNumberOfVectors;
}

void VjObs_i::setNumVectors(CORBA::Short value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mNnumVectors = value;
}

CORBA::Short VjObs_i::getNumVectors()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mNnumVectors;
}

void VjObs_i::setNumGeoArrays(CORBA::Short value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mNumGeoArrays = value;
}

CORBA::Short VjObs_i::getNumGeoArrays()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mNumGeoArrays;
}

void VjObs_i::setClients(CORBA::Long value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //cout   << "Setting number of Clients to " << value << endl; 
   //<< vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mClients = value;
}

CORBA::Long VjObs_i::getClients()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mClients;
}

void VjObs_i::setIsoValue(CORBA::Long value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mIso_value = value;
}

CORBA::Long VjObs_i::getIsoValue()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mIso_value;
}

void VjObs_i::setSc(CORBA::Long value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mSc = value;
}

CORBA::Long VjObs_i::getSc()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mSc;
}

void VjObs_i::setMin(CORBA::Long value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mMin = value;
}

CORBA::Long VjObs_i::getMin()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mMin;
}

void VjObs_i::setMax(CORBA::Long value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mMax = value;
}

CORBA::Long VjObs_i::getMax()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mMax;
}

void VjObs_i::setId(CORBA::Long value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mId = value;
}

CORBA::Long VjObs_i::getId()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mId;
}

void VjObs_i::setGeoState(CORBA::Long value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mGeo_state = value;
}

CORBA::Long VjObs_i::getGeoState()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mGeo_state;
}

void VjObs_i::setPostdataState(CORBA::Short value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mPostdata_state = value;
}

CORBA::Short VjObs_i::getPostdataState()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mPostdata_state;
}

void VjObs_i::setPreState(CORBA::Short value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mPre_state = (bool)value;
}

CORBA::Short VjObs_i::getPreState()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mPre_state;
}

void VjObs_i::setTimesteps(CORBA::Short value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mTimesteps = value;
}

CORBA::Short VjObs_i::getTimesteps()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mTimesteps;
}

void VjObs_i::setNumTeacherArrays(CORBA::Short value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mNumTeacherArrays = value;
}

CORBA::Short VjObs_i::getNumTeacherArrays()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mNumTeacherArrays;
}

void VjObs_i::setTeacherState(CORBA::Short value)
{
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Setting mValue to '" << value << "'\n" << vprDEBUG_FLUSH;

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   mTeacher_state = value;
}

CORBA::Short VjObs_i::getTeacherState()
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   //vprDEBUG(vprDBG_ALL, 0)
   //   << "Returning '" << mValue << "' to caller\n" << vprDEBUG_FLUSH;
   return mTeacher_state;
}

// These functions are called from the java side
// Need to figure out a better notation so that this all makes sense
CORBA::Short VjObs_i::get_sc_num() 
{
   int i;

   int numDatasets = this->mParamReader->GetNumberOfDataSets();
   vprDEBUG(vprDBG_ALL,0) << " numDatasets = " << numDatasets
                          << std::endl << vprDEBUG_FLUSH;
   
   this->setNumDatasets( numDatasets );

   this->dataset_names  = new VjObs::scalar_p( numDatasets );
   this->dataset_names->length( numDatasets );

   this->dataset_types  = new VjObs::obj_p( numDatasets );
   this->dataset_types->length( numDatasets );

   this->num_scalars_per_dataset = new VjObs::obj_p( numDatasets );
   this->num_scalars_per_dataset->length( numDatasets );

   this->num_vectors_per_dataset = new VjObs::obj_p( numDatasets );
   this->num_vectors_per_dataset->length( numDatasets );

   this->totalNumberOfScalars = 0;
   this->totalNumberOfVectors = 0;
   for ( i=0; i<numDatasets; i++ )
   {
      //cout << i << "\t" << this->mParamReader->GetDataSet( i )->GetNumberOfScalars() << endl;
      this->totalNumberOfScalars += this->mParamReader->GetDataSet( i )
                                                      ->GetNumberOfScalars();
      this->totalNumberOfVectors += this->mParamReader->GetDataSet( i )
                                                      ->GetNumberOfVectors();
   }
   vprDEBUG(vprDBG_ALL,0)
      << " totalNumberOfScalars: " << this->totalNumberOfScalars
      << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vprDBG_ALL,0)
      << " totalNumberOfVectors: " << this->totalNumberOfVectors
      << std::endl << vprDEBUG_FLUSH;

   this->scl_name = new VjObs::scalar_p( this->totalNumberOfScalars );
   this->scl_name->length( this->totalNumberOfScalars );

   this->vec_name = new VjObs::scalar_p( this->totalNumberOfVectors );
   this->vec_name->length( this->totalNumberOfVectors );

   int sIndex = 0;
   int vIndex = 0;
   for ( i=0; i < numDatasets; i++ )
   {
      this->dataset_names[ i ] = CORBA::string_dup( 
                          this->mParamReader->GetDataSet( i )->GetFileName() );
      vprDEBUG(vprDBG_ALL,1) << " dataset_name:   " << this->dataset_names[ i ]
                             << std::endl << vprDEBUG_FLUSH;

      this->dataset_types[ i ] = this->mParamReader->GetDataSet( i )->GetType();

      int num_scalars = this->mParamReader->GetDataSet( i )
                                          ->GetNumberOfScalars();
      this->num_scalars_per_dataset[ i ] = num_scalars;

      for (int ii=0; ii < num_scalars; ii++ )
      {
         this->scl_name[ sIndex ] = CORBA::string_dup( 
                    this->mParamReader->GetDataSet( i )->GetScalarName( ii ) );
         vprDEBUG(vprDBG_ALL,1) << "\tscl_name " 
                                << sIndex << " : " << this->scl_name[ sIndex ]
                                << std::endl << vprDEBUG_FLUSH;
         sIndex++;
      }

      int num_vectors = this->mParamReader->GetDataSet( i )
                                          ->GetNumberOfVectors();
      this->num_vectors_per_dataset[ i ] = num_vectors;

      for (int ii=0; ii < num_vectors; ii++ )
      {
         this->vec_name[ vIndex ] = CORBA::string_dup( 
                    this->mParamReader->GetDataSet( i )->GetVectorName( ii ) );
         vprDEBUG(vprDBG_ALL,1) << "\tvec_name " 
                                << vIndex << " : " << this->vec_name[ vIndex ]
                                << std::endl << vprDEBUG_FLUSH;
         vIndex++;
      }
   }

   vprDEBUG(vprDBG_ALL,1) << "leaving VjObs_i::get_sc_num()"
                          << std::endl << vprDEBUG_FLUSH;

   return this->getNumDatasets();
}

CORBA::Short VjObs_i::get_geo_num()
{
   // yang-REI: block lock to protect the cfdApp data since no buffer is used, 
   // and the corba servant side is waiting for the result
   this->setPreState( 1 );

   int numGeoArrays = this->mParamReader->numGeoms;
   vprDEBUG(vprDBG_ALL,0)
      << " Number of geometries to be transfered to the client: "
      << numGeoArrays
      << std::endl << vprDEBUG_FLUSH;

   this->setNumGeoArrays( numGeoArrays );

   if( numGeoArrays > 0 )
   {
      this->geo_name = new VjObs::scalar_p( numGeoArrays );
      this->geo_name->length( numGeoArrays );
      for(int i = 0; i < numGeoArrays; i++)
      {
         this->geo_name[ i ] = CORBA::string_dup(
                                  this->mParamReader->files[ i ]->fileName );
      }
   }
   return numGeoArrays;
}

CORBA::Short VjObs_i::get_teacher_num()
{
   CORBA::Short numTeacherArrays = this->mTeacher->getNumberOfFiles();
   vprDEBUG(vprDBG_ALL,0)
      << " Number of performer binary files to be transfered to the client: "
      << numTeacherArrays
      << std::endl << vprDEBUG_FLUSH;

   this->setNumTeacherArrays( numTeacherArrays );
   if( numTeacherArrays > 0 )
   {
      this->teacher_name = new VjObs::scalar_p( numTeacherArrays );
      this->teacher_name->length( numTeacherArrays );
      for(CORBA::Short i = 0; i < numTeacherArrays; i++)
      {
         this->teacher_name[ i ] = CORBA::string_dup(
                                        this->mTeacher->getFileName( i ) );
      }
   }

   return numTeacherArrays;
}

void VjObs_i::SetCfdReadParam( cfdReadParam *value )
{
   this->mParamReader = value;
}

void VjObs_i::SetCfdTeacher( cfdTeacher *value )
{
   this->mTeacher = value;
}

void VjObs_i::GetCfdStateVariables( void )
{

   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   this->cfdIso_value      = this->mIso_value;
   this->cfdSc             = this->mSc;
   this->cfdMin            = this->mMin;
   this->cfdMax            = this->mMax;
   this->cfdId             = this->mId;
   this->cfdGeo_state      = this->mGeo_state;
   this->cfdPostdata_state = this->mPostdata_state;
   this->cfdPre_state      = this->mPre_state;
   this->cfdTimesteps      = this->mTimesteps;
   this->cfdTeacher_state  = this->mTeacher_state;
   
   for ( int i = 0; i < 9; i++ )
   {
      cfdShort_data_array[ i ] = mShort_data_array[ i ];
   } 

#ifdef _CLUSTER
   if ( mStates.isLocal() )
   {
      this->mStates->clusterIso_value        = this->mIso_value;
      this->mStates->clusterSc               = this->mSc;
      this->mStates->clusterMin              = this->mMin;
      this->mStates->clusterMax              = this->mMax;
      this->mStates->clusterId               = this->mId;
      this->mStates->clusterGeo_state        = this->mGeo_state;
      this->mStates->clusterPostdata_state   = this->mPostdata_state;
      this->mStates->clusterPre_state        = this->mPre_state;
      this->mStates->clusterTimesteps        = this->mTimesteps;
      this->mStates->clusterTeacher_state    = this->mTeacher_state;
      //This value only matters to the cluster setting;
      //if this is master, get the value from the sequence node
      if ( this->activeSequenceObject != NULL )
      {
         cfdSequence* the_sequence = this->activeSequenceObject->GetpfSequence();
         if ( the_sequence != NULL )
         {
            this->mStates->currentFrame = the_sequence->getFrame(); 
         }
      }
   }
#endif
}

#ifdef _CLUSTER
void VjObs_i::GetUpdateClusterStateVariables( void )
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   this->cfdIso_value      = this->mStates->clusterIso_value;    
   this->cfdSc             = this->mStates->clusterSc;            
   this->cfdMin            = this->mStates->clusterMin;           
   this->cfdMax            = this->mStates->clusterMax;          
   this->cfdId             = this->mStates->clusterId;            
   this->cfdGeo_state      = this->mStates->clusterGeo_state;     
   this->cfdPostdata_state = this->mStates->clusterPostdata_state;
   this->cfdPre_state      = this->mStates->clusterPre_state;     
   this->cfdTimesteps      = this->mStates->clusterTimesteps;     
   this->cfdTeacher_state  = this->mStates->clusterTeacher_state; 
   //This value only matters to the cluster setting;
   //if this is master, get the value from the sequence node
   if ( this->activeSequenceObject != NULL )
   {
      cfdSequence* the_sequence = this->activeSequenceObject->GetpfSequence();
      if ( the_sequence != NULL )
         the_sequence->setCurrentFrame( this->mStates->currentFrame );
   }
}
#endif

CORBA::Short VjObs_i::GetNumberOfSounds()
{
   int numberOfSounds = this->mParamReader->soundFile;
   this->sound_names = new VjObs::scalar_p( numberOfSounds );
   this->sound_names->length( numberOfSounds );

   vprDEBUG(vprDBG_ALL,0) << " Number of Sounds to be transfered to client: " 
                          << numberOfSounds << std::endl << vprDEBUG_FLUSH;

   if( numberOfSounds > 0 )
   {
      for(int i = 0; i < numberOfSounds; i++)
      {
         this->sound_names[ i ] = CORBA::string_dup(this->mParamReader->soundFiles[i]->fileName);
      }
   }

   return numberOfSounds;
}

VjObs::scalar_p* VjObs_i::GetSoundNameArray()
{
   return this->sound_names._retn();
}

void VjObs_i::SetClientInfoFlag( CORBA::Short value )
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   this->mGetClientInfo = value;
}

VjObs::obj_p* VjObs_i::GetClientInfoData()
{
   return clientInfoObserverDataArray._retn();
}

void VjObs_i::SetClientInfoData( const VjObs::obj_pd &value )
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
   // The order of setting these values
   // MUST MATCH the order in which they are set in 
   // my_orb.java
   if ( (value[ 7 ] != -1) && (value [ 8 ] != -1) )
   {
      this->mId = value[ 0 ];
      //cout<<"update:this->corba_mutex.C_id = "<<this->mId<<endl;

      // get the value of the slider bar, used by many visualizations
      this->mIso_value = value[ 1 ];
      //cout<<"iso_value"<<this->mIso_value<<endl;

      this->mTimesteps = value[ 2 ];

      this->mSc = value[ 3 ];
      //cout<<"select scalar:"<<this->mSc<<endl;

      // change scalar range or cursor settings
      this->mMin = value[ 4 ];
      this->mMax = value[ 5 ];
      //cout<<"update:min,max values: "<<this->mMin<<"\t"<<this->mMax<<endl;

      this->mGeo_state = value[ 6 ];
      //cout<<"geometry state:"<< this->mGeo_state <<endl;

   this->mPre_state = (bool)value[ 7 ];
   //cout<<"pre_state:"<< this->mPre_state <<endl;

      this->mTeacher_state = value[ 8 ];
      //cout<<"mTeacher state:"<< this->mTeacher_state <<endl;
   }
   else
   {
      this->mId = value[ 0 ];
      for ( int i = 0; i < 9; i ++ )
      {
         mShort_data_array[ i ] = value[ i ];
         cout << value[ i ] << endl;
      }
   }
}
