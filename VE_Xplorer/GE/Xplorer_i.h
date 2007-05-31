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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _XPLORER_I_H_
#define _XPLORER_I_H_

#include "VE_Open/skel/moduleS.h"

#include <vpr/Sync/Mutex.h>
#include <vpr/Sync/Guard.h>
#include <vpr/IO/SerializableObject.h>

#include <cluster/ClusterManager.h>
#include <cluster/ClusterNetwork.h>

#include "VE_Xplorer/GE/cfdStateInfo.h"

#include <vrj/vrjParam.h>
#if __VJ_version <= 2000003
#include <cluster/ClusterNode.h>
#endif

#include <plugins/ApplicationDataManager/UserData.h>

namespace VE_Xplorer
{
	class cfdModelHandler;
	class cfdCommandArray;
}
namespace VE_XML
{
	class Command;
}

#include <vector>
#include <iostream>

class Body_VEXplorer_i
  : public virtual POA_Body::VEXplorer
{
public:
  ///Constructor 
  Body_VEXplorer_i (void);
  
  ///Destructor 
  virtual ~Body_VEXplorer_i (void);
  
  virtual char* GetStatusMessage ( )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));
  
  virtual void SetParams ( const char* param )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));
  
  virtual void SetID ( ::CORBA::Long id )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));
  
  virtual ::CORBA::Long GetID ( )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));
  
  virtual void SetName ( const char* name )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));
  
  virtual char* GetName ( )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));
  
  virtual void SetNetwork ( const char* network )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));
  
  virtual void SetCommand ( const char* command )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));
  
  virtual void RegisterUI ( const char* UIName, ::Body::UI_ptr ui )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown  ));
  
  virtual void UnRegisterUI ( const char* UIName )
    ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));

   void PreFrameUpdate( void );
   ///Initialize cluster stuff depending on cluster mode
   void InitCluster( void );
   ///Called in post frame to get variables
   void GetUpdateClusterStateVariables( void );
   /// Frame sync variables used by osg only at this point
   float GetSetAppTime( float );
   ///Get/Set the frame number
   long GetSetFrameNumber( long );
   ///Set the cluster mode flag
   void SetClusterMode( bool clusterFlag );
   ///Determine if we are in cluster mode or not
   bool GetClusterMode( void );

   VE_Xplorer::cfdCommandArray* _cfdArray;///< Data to hold command data shoudl be delete in the future
   VE_XML::Command* bufferCommand;///< Data to hold command data
   std::vector< VE_Xplorer::cfdCommandArray* > commandQueue; ///< vector to hold current list of commands

protected:
   ///Create command queue for transient vis
   ///Should be removed once non texture pipelines are created
   void CreateCommandQueue( void );

   VE_Xplorer::cfdCommandArray* _bufferArray;///< command data
   std::vector< VE_XML::Command* > commandVectorQueue;///< command vector may be a duplicate
   std::vector< std::string > commandStringQueue;///< command queue with raw string data

   vpr::Mutex mValueLock;  ///< A mutex to protect variables accesses

   cluster::UserData< vpr::SerializableObjectMixin< ClusterVariables::StateVariables > >  mStates;
   bool isCluster;///<cluster mode

   float time_since_start;///< start time
   long frameNumber;///< frame number

   Body::UI_var uiCom;


};
#endif