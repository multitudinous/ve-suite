//Coded by Chongguan Yang, REI 05/09/2003
#ifndef MUTEXINTERFACE_H
#define MUTEXINTERFACE_H

#include <vpr/Sync/Mutex.h>
#include <iostream>

//using std::cerr;

//this class defines the interface between the cfdApp Visualization Engine and
//the Corba Interface
//Mutex are putted on for the two threads to access it exclusivly.

class MutexInterface
{
   // Actually, there are 4 threads in total
   // 1. the render loop threads 
   // 2. the intra_parallel thread 
   // 3. the corba servant thread. 
   // 4. the intraFrame function
   // This implementation is based on the precondition that the render loop 
   // threads, the intra_parallel thread, and the intraFrame thread are not 
   // going to step on the each other's data (I'm going to put mutex on the 
   // preframe, intra_parallel, and the intra_frame function) 
   
   // Since you don't want to block preframe's execution (lower the frame
   // rate), I will buffer the data by using three sets of data. A snapShot is
   // called to update the data from CORBA servant to the preframe or the other
   // direction.  An internal buffer is needed because the lock only prevent
   // the write to each other's data. There is no "read lock". So it is
   // possible that when the VjObs_i side do a snapShot(false) to read the
   // cfdApp's data and cfdApp is doing an corba_mutex.max = 10. An extra
   // internal buffer will prevent this possibility. And since the internal
   // buffer's access is always protected by lock, both sides now and push and
   // pull data freely.

 public:
  //The set of Variables used by the cfdApp side
  //short numScalars;
  //short numVectors;
  //short numGeoArrays;
  //int   clients;
  //int   iso_value;
  //int   sc;
  //int   min;
  //int   max;
  //long  id;
  //long  geo_state;
  //short postdata_state;
  //bool  pre_state;
  //short timesteps;
  //short numTeacherArrays;
  //short teacher_state;
  
  //The set of Variables used by the Corba servant side
  //short C_numScalars;
  //short C_numVectors;
  //short C_numGeoArrays;
  //int   C_clients;
  //int   C_iso_value;
  //int   C_sc;
  //int   C_min;
  //int   C_max;
  //long  C_id;
  //long  C_geo_state;
  //short C_postdata_state;
  //bool  C_pre_state;
  //short C_timesteps;
  //short C_numTeacherArrays;
  //short C_teacher_state;

 protected:
  //The set of internal buffer
  //short I_numScalars;
  //short I_numVectors;
  //short I_numGeoArrays;
  //int   I_clients;
  //int   I_iso_value;
  //int   I_sc;
  //int   I_min;
  //int   I_max;
  //long  I_id; 
  ///long  I_geo_state;
  //short I_postdata_state;
  //bool  I_pre_state;
  //short I_timesteps;
  //short I_numTeacherArrays;
  //short I_teacher_state;
  
  vpr::Mutex inter_lock; //the mutex used to guarantee exclusion
 

 public:
/*   void snapShot( bool direction )
   //if direction is true, copy from the internal buffer to cfdApp, otherwise vice versa
   {
      if ( inter_lock.tryAcquire() != vpr::ReturnStatus::Fail )
	   //use tryAcquire so the snapShot call will not be block
	   {
	      //lock acquired successfully
	      if (direction)
	      {
	         numScalars = I_numScalars;
	         numVectors = I_numVectors;
	         numGeoArrays = I_numGeoArrays;
	         clients = I_clients;
	         iso_value = I_iso_value;
	         sc = I_sc;
	         min = I_min;
	         max = I_max;
	         id = I_id;
	         geo_state = I_geo_state;
	         postdata_state = I_postdata_state;
	         pre_state = I_pre_state;
	         timesteps = I_timesteps;
            numTeacherArrays = I_numTeacherArrays;
            teacher_state = I_teacher_state;
	      }
	      else
	      {
	         I_numScalars = numScalars;
	         I_numVectors = numVectors;
	         I_numGeoArrays = numGeoArrays;
	         I_clients = clients;
	         I_iso_value = iso_value;
	         I_sc = sc;
	         I_min = min;
	         I_max = max;
	         I_id = id;
	         I_geo_state = geo_state;
	         I_postdata_state = postdata_state;
	         I_pre_state = pre_state;
	         I_timesteps = timesteps;
            I_numTeacherArrays = numTeacherArrays;
            I_teacher_state = teacher_state;
	      }
	  
	      if (inter_lock.release()==vpr::ReturnStatus::Fail) 
         { 
            cerr<<"interface lock release failed"<<endl; 
         }
      }
      else //can't get the lock, which means the other thread is updating something currently
	   {
         //so the buffered values will be used, which is fine
	      //inter_lock.release();
      }
      
      return;
   }

   void snapShot2(bool direction)
   //if direction is true, copy from the CORBA servant to internal buffer, otherwise vice versa
   {
      if (inter_lock.tryAcquire()!=vpr::ReturnStatus::Fail)
	   //use tryAcquire so the snapShot call will not be block
	   {
	      //lock acquired successfully
	      if (direction)
	      {
	         I_numScalars = C_numScalars;
	         I_numVectors = C_numVectors;
	         I_numGeoArrays = C_numGeoArrays;
	         I_clients = C_clients;
	         I_iso_value = C_iso_value;
	         I_sc = C_sc;
	         I_min = C_min;
	         I_max = C_max;
	         I_id = C_id;
	         I_geo_state = C_geo_state;
	         I_postdata_state = C_postdata_state;
	         I_pre_state = C_pre_state;
	         I_timesteps = C_timesteps;
            I_numTeacherArrays = C_numTeacherArrays;
            I_teacher_state = C_teacher_state;
	      }
	      else
	      {
	         C_numScalars = I_numScalars;
	         C_numVectors = I_numVectors;
	         C_numGeoArrays = I_numGeoArrays;
	         C_clients = I_clients;
	         C_iso_value = I_iso_value;
	         C_sc = I_sc;
	         C_min = I_min;
	         C_max = I_max;
	         C_id = I_id;
	         C_geo_state = I_geo_state;
	         C_postdata_state = I_postdata_state;
	         C_pre_state = I_pre_state;
	         C_timesteps = I_timesteps;
            C_numTeacherArrays = I_numTeacherArrays;
            C_teacher_state = I_teacher_state;
	      }
	  
	      if ( inter_lock.release() == vpr::ReturnStatus::Fail ) 
         { 
            cerr<<"interface lock release failed"<<endl; 
         }
      }
      else //can't get the lock, which means the other thread is updating something currently
	   {   
         //so the buffered values will be used, which is fine
	      //inter_lock.release();	
      }

      return;
   }

   void cfdFpush()
   //block calls to ensure the data is pushed
   {
      inter_lock.acquire();
      
      I_numScalars = numScalars;
      I_numVectors = numVectors;
      I_numGeoArrays = numGeoArrays;
      I_clients = clients;
      I_iso_value = iso_value;
      I_sc = sc;
      I_min = min;
      I_max = max;
      I_id = id;
      I_geo_state = geo_state;
      I_postdata_state = postdata_state;
      I_pre_state = pre_state;
	   I_timesteps = timesteps;
      I_numTeacherArrays = numTeacherArrays;
      I_teacher_state = teacher_state;
      
      inter_lock.release();     
   }

   void cfdFpull()
   //block calls to ensure the data is pushed
   {
      inter_lock.acquire();
      
      numScalars = I_numScalars;
      numVectors = I_numVectors;
      numGeoArrays = I_numGeoArrays;
      clients = I_clients;
      iso_value = I_iso_value;
      sc = I_sc;
      min = I_min;
      max = I_max;
      id = I_id;
      geo_state = I_geo_state;
      postdata_state = I_postdata_state;
      pre_state = I_pre_state;
	   timesteps = I_timesteps;
      numTeacherArrays = I_numTeacherArrays;
      teacher_state = I_teacher_state;
      
      inter_lock.release();     
    }

 void vjObsFpull()
    //block calls to ensure the data is pushed
    {
      inter_lock.acquire();
      
      C_numScalars = I_numScalars;
      C_numVectors = I_numVectors;
      C_numGeoArrays = I_numGeoArrays;
      C_clients = I_clients;
      C_iso_value = I_iso_value;
      C_sc = I_sc;
      C_min = I_min;
      C_max = I_max;
      C_id = I_id;
      C_geo_state = I_geo_state;
      C_postdata_state = I_postdata_state;
      C_pre_state = I_pre_state;
	   C_timesteps = I_timesteps;
      C_numTeacherArrays = I_numTeacherArrays;
      C_teacher_state = I_teacher_state;
      
      inter_lock.release();     
    }

   void vjObsFpush()
   //block calls to ensure the data is pushed
   {
      inter_lock.acquire();
      
      I_numScalars = C_numScalars;
      I_numVectors = C_numVectors;
      I_numGeoArrays = C_numGeoArrays;
      I_clients = C_clients;
      I_iso_value = C_iso_value;
      I_sc = C_sc;
      I_min = C_min;
      I_max = C_max;
      I_id = C_id;
      I_geo_state = C_geo_state;
      I_postdata_state = C_postdata_state;
      I_pre_state = C_pre_state;
	   I_timesteps = C_timesteps;
      I_numTeacherArrays = C_numTeacherArrays;
      I_teacher_state = C_teacher_state;
      
      inter_lock.release();     
   }*/
};

#endif
