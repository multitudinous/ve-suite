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
 * File:          $RCSfile: VjObs_i.h,v $
 * Date modified: $Date: 2004/04/25 06:42:42 $
 * Version:       $Revision: 1.5 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VJOBS_I_H_
#define _VJOBS_I_H_

#include "VjObs.h"
//#include "CorbaManager.h"
//#include "omniORB4/CORBA.h"
//#include "Observer.h"
#include <cstdlib>
#include <vpr/vpr.h>
#include <vpr/Sync/Mutex.h>
#include <vpr/Sync/Guard.h>
#include <vpr/Util/Debug.h>

#ifdef _CLUSTER
// Cluster Stuff
#include <vpr/IO/SerializableObject.h>
#include <cluster/ClusterManager.h>
#include <plugins/ApplicationDataManager/UserData.h>
#include <cluster/ClusterNetwork/ClusterNetwork.h>
#include <cluster/ClusterNetwork/ClusterNode.h>
#include "cfdStateInfo.h"
#endif

class cfdTeacher;
class cfdReadParam;

//#include "MutexInterface.h"   //yang-REI, the mutex interface
//#include "cfdStateInfoImpl.h"   

class VjObs_i:public virtual POA_VjObs, //public virtual CorbaManager,
               public PortableServer::RefCountServantBase
{
public:
   
   VjObs_i()
   {
      this->numOfClientInfo = 9;
      int temp=0;
      this->setClients( 0 );
      orb=CORBA::ORB_init(temp,0,"omniORB4");
      /*for(temp=0;temp<25;temp++)
      {
         client_list[temp]=0;
      }*/
   }
   virtual ~VjObs_i(){}
   
   //void SetApplicationPointer( cfdApp * );

   //void attach(Observer_ptr o);
   //void detach(Observer_ptr o);
   void update();
   //Observer::baf_p_slice* baf_param;
   VjObs::scalar_p* update_scalar();
   VjObs::scalar_p* update_vector();
   VjObs::scalar_p* get_geo_name();
   VjObs::scalar_p* get_teacher_name();
   //yang-REI : Change the design a little here
   //The original code's idea to set shared flag. Using that flag to wait for
   //the excution of the cfd::get_geo() and the cfd::get_scalar() to finish
   //the cfd::get_geo() and the cfd::get_scalar() will set the flag to be
   //false to break the while loop here.  This is unnecessary and caused
   //synchronization problem.  Since the corba servant and the cfdApp are 
   //the same object, this is equivalent to call a cfdApp member function and
   //wait for it return, which is the basic behavior of function calls.  I
   //reimplemented it with virtual functions, which will be overided in
   //cfdApp. So they are actually direct function calls to the cfdApp.
   short get_sc_num();//{return this->get_sc_num();}; //*
   short get_geo_num();//{return this->get_geo_num();}; //*
   short get_teacher_num();//{return this->get_teacher_num();}; //*
   char* get_perf();

   CORBA::Short GetNumberOfSounds();
   VjObs::scalar_p* GetSoundNameArray();
   //short get_postdata(){ return NULL; }
   //short get_timesteps(){ return NULL; }

   void SetClientInfoFlag( CORBA::Short );
   void SetClientInfoData( const VjObs::obj_pd &value );
   VjObs::obj_p* GetClientInfoData();
   VjObs::scalar_p * get_dataset_names();
   VjObs::obj_p * get_dataset_types();
   VjObs::obj_p * get_num_scalars_per_dataset();
   VjObs::obj_p * get_num_vectors_per_dataset();

private:
   CORBA::Object_var obj;
   //Observer_ptr client_list[25];
   //Observer_var client_list[25];
   CORBA::ORB_ptr orb;
   //Observer_ptr test_ptr;

protected:
   void SetCfdReadParam( cfdReadParam * );
   void SetCfdTeacher( cfdTeacher * );

   cfdReadParam *mParamReader;
   cfdTeacher  *mTeacher;

   //void put_cur_obj(Observer::obj_p_var o);

   VjObs::scalar_p_var scl_name;
   VjObs::scalar_p_var vec_name;
   VjObs::scalar_p_var geo_name;
   VjObs::scalar_p_var sound_names;
   VjObs::scalar_p_var teacher_name;
   //Observer::obj_p_var cur_obj;
   int totalNumberOfScalars;
   int totalNumberOfVectors;

   VjObs::scalar_p_var dataset_names;
   VjObs::obj_p_var dataset_types;
   VjObs::obj_p_var num_scalars_per_dataset;
   VjObs::obj_p_var num_vectors_per_dataset;
   VjObs::obj_p_var clientInfoObserverDataArray;
   int numOfClientInfo;
   //MutexInterface corba_mutex;   //yang-REI, list of things replaced with the mutex interface version
   void GetCfdStateVariables( void );
#ifdef _CLUSTER
   // Cluster Stuff
   virtual void GetUpdateClusterStateVariables( void );
#endif
   /**
    * Sets this subject's internal value.
    */
   void setNumDatasets(CORBA::Short value);
   CORBA::Short getNumDatasets();

   CORBA::Short getTotalNumberOfScalars();
   CORBA::Short getTotalNumberOfVectors();

   void setNumVectors(CORBA::Short value);
   CORBA::Short getNumVectors( void );

   void setNumGeoArrays(CORBA::Short value);
   CORBA::Short getNumGeoArrays( void );

   void setClients(CORBA::Long value);
   CORBA::Long getClients( void );

   void setIsoValue(CORBA::Long value);
   CORBA::Long getIsoValue( void );

   void setSc(CORBA::Long value);
   CORBA::Long getSc( void );

   void setMin(CORBA::Long value);
   CORBA::Long getMin( void );

   void setMax(CORBA::Long value);
   CORBA::Long getMax( void );

   void setId(CORBA::Long value);
   CORBA::Long getId( void );

   void setGeoState(CORBA::Long value);
   CORBA::Long getGeoState( void );

   void setPostdataState(CORBA::Short value);
   CORBA::Short getPostdataState( void );

   void setPreState(CORBA::Short value);
   CORBA::Short getPreState( void );

   void setTimesteps(CORBA::Short value);
   CORBA::Short getTimesteps( void );

   void setNumTeacherArrays(CORBA::Short value);
   CORBA::Short getNumTeacherArrays( void );

   void setTeacherState(CORBA::Short value);
   CORBA::Short getTeacherState( void );

   vpr::Mutex  mValueLock;  /**< A mutex to protect variables accesses */

   // Buffer variables...always right
   short mNumScalars;
   short mNnumVectors;
   short mNumGeoArrays;
   int   mClients;
   int   mIso_value;
   int   mSc;
   int   mMin;
   int   mMax;
   long  mId;
   long  mGeo_state;
   short mPostdata_state;
   bool  mPre_state;
   short mTimesteps;
   short mNumTeacherArrays;
   short mTeacher_state;
   short mGetClientInfo;
   double mShort_data_array[ 9 ];

   // cfdApp side variables
   int   cfdIso_value;
   int   cfdSc;
   int   cfdMin;
   int   cfdMax;
   long  cfdId;
   long  cfdGeo_state;
   short cfdPostdata_state;
   bool  cfdPre_state;
   short cfdTimesteps;
   short cfdTeacher_state; 
   double cfdShort_data_array[ 9 ];

#ifdef _CLUSTER
   // Cluster Stuff for the above state variables
   cluster::UserData< vpr::SerializableObjectMixin< ClusterVariables::StateVariables > >  mStates;
#endif
};

#endif
