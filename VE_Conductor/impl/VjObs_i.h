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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VJOBS_I_H_
#define _VJOBS_I_H_
#ifdef _TAO
#include "VjObsS.h"
#else
#include "VjObs.h"
#endif

#include <vpr/Sync/Mutex.h>

#ifdef _CLUSTER
#include <cluster/ClusterManager.h>
#include <cluster/ClusterNetwork/ClusterNetwork.h>
#include <cluster/ClusterNetwork/ClusterNode.h>
#include <plugins/ApplicationDataManager/UserData.h>
#include <vpr/IO/SerializableObject.h>
#include "cfdStateInfo.h"
#endif

class cfdTeacher;
class cfdReadParam;
class cfdCommandArray;

#include "cfdGlobalBase.h"

class VjObs_i : public virtual POA_VjObs, //public virtual CorbaManager,
                public PortableServer::RefCountServantBase,
                public cfdGlobalBase
{
public:
   
   VjObs_i()
   {
      this->numOfClientInfo = 9;
      //int temp=0;
      this->setClients( 0 );
            this->mId = -1;
      //orb=CORBA::ORB_init(temp,0,"omniORB4");
      /*for(temp=0;temp<25;temp++)
      {
         client_list[temp]=0;
      }*/
         // allocate enough space
	   geo_name       = new VjObs::scalar_p(50);
	   geo_name->length(50);
	   scl_name       = new VjObs::scalar_p(50);
	   scl_name->length(50);
	   teacher_name   = new VjObs::scalar_p(50);
	   teacher_name->length(50);
	   dataset_names  = new VjObs::scalar_p(50);
	   dataset_names->length(50);
	   sound_names    = new VjObs::scalar_p(50);
	   sound_names->length(50);

	   dataset_types  = new VjObs::obj_p(50);
	   dataset_types->length(50);
	   num_scalars_per_dataset = new VjObs::obj_p(50);
	   num_scalars_per_dataset->length(50);
	   num_vectors_per_dataset = new VjObs::obj_p( 50 );
	   num_vectors_per_dataset->length(50);
	   vec_name = new VjObs::scalar_p( 50 );
	   vec_name->length( 50 );
      // This array is used in place of the call backs
      // to the client because the communication didn't
      // seem to work. There are 9 entries becuase that
      // how many variables are synchronized during an
      // update call in VjObs_i
	   clientInfoObserverDataArray = new VjObs::obj_p(50);
	   clientInfoObserverDataArray->length(50);
	   clientInfoObserverDataArray->length( this->numOfClientInfo );
	   clientInfoObserverDataArray->length(50);
      this->_unusedNewData = false;

   }
   virtual ~VjObs_i(){}
   
   // Only Call these functions once in the constructor
   // These initialize and populate all the passed arrays
   void CreateSoundInfo( void );
   void CreateGeometryInfo( void );
   void CreateDatasetInfo( void );
   void CreateTeacherInfo( void );

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();
#ifdef _TAO   
   void update() throw (CORBA::SystemException);
   //Observer::baf_p_slice* baf_param;
   VjObs::scalar_p* update_scalar() throw (CORBA::SystemException);
   VjObs::scalar_p* update_vector() throw (CORBA::SystemException);
   VjObs::scalar_p* get_geo_name() throw (CORBA::SystemException);
   VjObs::scalar_p* get_teacher_name() throw (CORBA::SystemException);
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
   short get_sc_num() throw (CORBA::SystemException);//{return this->get_sc_num();}; //*
   short get_teacher_num() throw (CORBA::SystemException);//{return this->get_teacher_num();}; //*
   short get_geo_num() throw (CORBA::SystemException);//{return this->get_geo_num();}; //*
   char* get_perf() throw (CORBA::SystemException);

   short GetNumberOfSounds() throw (CORBA::SystemException);
   VjObs::scalar_p* GetSoundNameArray() throw (CORBA::SystemException);
   //short get_postdata(){ return NULL; }
   //short get_timesteps(){ return NULL; }

   void SetClientInfoFlag( short ) throw (CORBA::SystemException);
   void SetClientInfoData( const VjObs::obj_pd &value ) throw (CORBA::SystemException);
   VjObs::obj_p* GetClientInfoData() throw (CORBA::SystemException);
   VjObs::scalar_p * get_dataset_names() throw (CORBA::SystemException);
   VjObs::obj_p * get_dataset_types() throw (CORBA::SystemException);
   VjObs::obj_p * get_num_scalars_per_dataset() throw (CORBA::SystemException);
   VjObs::obj_p * get_num_vectors_per_dataset() throw (CORBA::SystemException);
#else   
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
#endif   

private:
   //CORBA::Object_var obj;
   //Observer_ptr client_list[25];
   //Observer_var client_list[25];
   //CORBA::ORB_ptr orb;
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
   bool _unusedNewData;
   void GetCfdStateVariables( void );

#ifdef _CLUSTER
   virtual void GetUpdateClusterStateVariables( void );
#endif

#ifdef _TAO   
   void setNumDatasets(const short value) throw (CORBA::SystemException);
   short getNumDatasets( void ) throw (CORBA::SystemException);

   short getTotalNumberOfScalars( void ) throw (CORBA::SystemException);
   CORBA::Short getTotalNumberOfVectors() throw (CORBA::SystemException);

   void setNumVectors(const short value) throw (CORBA::SystemException);
   short getNumVectors( void ) throw (CORBA::SystemException);

   void setNumGeoArrays(const short value) throw (CORBA::SystemException);
   short getNumGeoArrays( void ) throw (CORBA::SystemException);

   void setClients(const CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getClients( void ) throw (CORBA::SystemException);

   void setIsoValue(const CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getIsoValue( void ) throw (CORBA::SystemException);

   void setSc(const CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getSc( void ) throw (CORBA::SystemException);

   void setMin(const CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getMin( void ) throw (CORBA::SystemException);

   void setMax(const CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getMax( void ) throw (CORBA::SystemException);

   void setId(const CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getId( void ) throw (CORBA::SystemException);

   void setGeoState(const CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getGeoState( void ) throw (CORBA::SystemException);

   void setPostdataState(const short value) throw (CORBA::SystemException);
   short getPostdataState( void ) throw (CORBA::SystemException);

   void setPreState(const short value) throw (CORBA::SystemException);
   short getPreState( void ) throw (CORBA::SystemException);

   void setTimesteps(const short value) throw (CORBA::SystemException);
   short getTimesteps( void ) throw (CORBA::SystemException);

   void setNumTeacherArrays(const short value) throw (CORBA::SystemException);
   short getNumTeacherArrays( void ) throw (CORBA::SystemException);

   void setTeacherState(const short value) throw (CORBA::SystemException);
   short getTeacherState( void ) throw (CORBA::SystemException);
#else   
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
#endif
   vpr::Mutex mValueLock;  /**< A mutex to protect variables accesses */

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
