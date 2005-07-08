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
 * File:          $RCSfile: VjObs_i.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VJOBS_I_H_
#define _VJOBS_I_H_

#ifdef _TAO
#include "VEOpen/skel/VjObsS.h"
#else
#include "VEOpen/skel/VjObs.h"
#endif

#include <vpr/Sync/Mutex.h>
#include "VE_Xplorer/cfdCommandArray.h"

#ifdef _CLUSTER
#include <cluster/ClusterManager.h>
#include <cluster/ClusterNetwork.h>
#include <cluster/ClusterNode.h>
#include <plugins/ApplicationDataManager/UserData.h>
#include <vpr/IO/SerializableObject.h>
#include "VE_Conductor/impl/cfdStateInfo.h"
#endif

namespace VE_Xplorer
{
   class cfdModelHandler;
}

#include <vector>

namespace VE_Xplorer
{
class VjObs_i : public virtual POA_VjObs, //public virtual CorbaManager,
                public PortableServer::RefCountServantBase
{
public:
   
   VjObs_i()
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
	   clientInfoObserverDataArray = new VjObs::obj_p(50);
	   clientInfoObserverDataArray->length(50);
	   clientInfoObserverDataArray->length( this->numOfClientInfo );
	   clientInfoObserverDataArray->length(50);
      this->_unusedNewData = false;
      _models = NULL;
      time_since_start = 0.0f;
      frameNumber = 0;
   }
   virtual ~VjObs_i(){}
   
   void CreateGeometryInfo( void );
   void CreateDatasetInfo( void );
   void CreateTeacherInfo( void );

   void PreFrameUpdate( void );
   void InitCluster( void );
   void GetUpdateClusterStateVariables( void );

   // Frame sync variables used by osg only at this point
   float GetSetAppTime( float );
   long GetSetFrameNumber( long );
#ifdef _TAO   
   //VjObs::scalar_p* get_geo_name() throw (CORBA::SystemException);
   VjObs::scalar_p* get_teacher_name() throw (CORBA::SystemException);
   VjObs::Models* GetModels() throw (CORBA::SystemException);
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
   short get_teacher_num() throw (CORBA::SystemException);//{return this->get_teacher_num();}; //*
   //short get_geo_num() throw (CORBA::SystemException);//{return this->get_geo_num();}; //*
   char* get_perf() throw (CORBA::SystemException);

   short GetNumberOfSounds() throw (CORBA::SystemException);
   VjObs::scalar_p* GetSoundNameArray() throw (CORBA::SystemException);

   void SetClientInfoFlag( short ) throw (CORBA::SystemException);
   void SetClientInfoData( const VjObs::obj_pd &value ) throw (CORBA::SystemException);
   VjObs::obj_pd* GetClientInfoData() throw (CORBA::SystemException);
#else   
   VjObs::Models* GetModels();
   //VjObs::scalar_p* get_geo_name();
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
   //short get_geo_num();//{return this->get_geo_num();}; //*
   short get_teacher_num();//{return this->get_teacher_num();}; //*
   char* get_perf();

   CORBA::Short GetNumberOfSounds();
   VjObs::scalar_p* GetSoundNameArray();
   //short get_postdata(){ return NULL; }
   //short get_timesteps(){ return NULL; }

   void SetClientInfoFlag( CORBA::Short );
   void SetClientInfoData( const VjObs::obj_pd &value );
   VjObs::obj_pd* GetClientInfoData();
#endif   

   void GetCfdStateVariables( void );
   cfdCommandArray* _cfdArray;
   double cfdShort_data_array[ 9 ];
   
   std::vector< cfdCommandArray* > commandQueue;
protected:
   void CreateCommandQueue( void );

#ifdef _OSG
   //cfdTextureBasedVizHandler* _tbvHandler;
#endif
   //VjObs::scalar_p_var scl_name;
   //VjObs::scalar_p_var vec_name;
   //VjObs::scalar_p_var geo_name;
   VjObs::scalar_p_var sound_names;
   VjObs::scalar_p_var teacher_name;
   VjObs::Models* _models;

   //int totalNumberOfScalars;
   //int totalNumberOfVectors;

   //VjObs::scalar_p_var dataset_names;
   //VjObs::obj_p_var dataset_types;
   //VjObs::obj_p_var num_scalars_per_dataset;
   //VjObs::obj_p_var num_vectors_per_dataset;
   VjObs::obj_p_var clientInfoObserverDataArray;
   int numOfClientInfo;
   bool _unusedNewData;
   float time_since_start;
   long frameNumber;
#ifdef _TAO   
   //void setNumGeoArrays(const short value) throw (CORBA::SystemException);
   //short getNumGeoArrays( void ) throw (CORBA::SystemException);

   virtual void setIsoValue( CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getIsoValue( void ) throw (CORBA::SystemException);

   void setSc( CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getSc( void ) throw (CORBA::SystemException);

   void setMin( CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getMin( void ) throw (CORBA::SystemException);

   void setMax( CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getMax( void ) throw (CORBA::SystemException);

   void setId( CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getId( void ) throw (CORBA::SystemException);

   void setGeoState( CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getGeoState( void ) throw (CORBA::SystemException);

   void setPostdataState( short value) throw (CORBA::SystemException);
   short getPostdataState( void ) throw (CORBA::SystemException);

   void setPreState( short value) throw (CORBA::SystemException);
   short getPreState( void ) throw (CORBA::SystemException);

   void setTimesteps( short value) throw (CORBA::SystemException);
   short getTimesteps( void ) throw (CORBA::SystemException);

   void setNumTeacherArrays( short value) throw (CORBA::SystemException);
   short getNumTeacherArrays( void ) throw (CORBA::SystemException);

   void setTeacherState( short value) throw (CORBA::SystemException);
   short getTeacherState( void ) throw (CORBA::SystemException);
#else   
   /**
    * Sets this subject's internal value.
    */
   //void setNumGeoArrays(CORBA::Short value);
   //CORBA::Short getNumGeoArrays( void );

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
   //int   mClients;
   //int   mIso_value;
   //int   mSc;
   //int   mMin;
   //int   mMax;
   //long  mId;
   //long  mGeo_state;
   //short mPostdata_state;
   //bool  mPre_state;
   //short mTimesteps;
   short mNumTeacherArrays;
   //short mTeacher_state;
   short mGetClientInfo;
   double mShort_data_array[ 9 ];

   // cfdApp side variables
   // Now in cfdCommandArray
   /*int   cfdIso_value;
   int   cfdSc;
   int   cfdMin;
   int   cfdMax;
   long  cfdId;
   long  cfdGeo_state;
   short cfdPostdata_state;
   bool  cfdPre_state;
   short cfdTimesteps;
   short cfdTeacher_state; */

   cfdCommandArray* _bufferArray;
#ifdef _CLUSTER
   // Cluster Stuff for the above state variables
   cluster::UserData< vpr::SerializableObjectMixin< ClusterVariables::StateVariables > >  mStates;
#endif
};
}
#endif
