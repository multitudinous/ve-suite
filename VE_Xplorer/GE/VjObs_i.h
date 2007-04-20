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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VJOBS_I_H_
#define _VJOBS_I_H_
/*!\file VjObs_i.h
VjObs_i API
*/

/*!\class VE_Xplorer::VjObs_i
*
*/
#include "VE_Open/skel/VjObsS.h"

//do this to remove compile warning on linux platforms
#undef _REENTRANT

#include <vpr/Sync/Mutex.h>

#include <cluster/ClusterManager.h>
#include <cluster/ClusterNetwork.h>
#include <cluster/ClusterNode.h>

#include <plugins/ApplicationDataManager/UserData.h>

#include <vpr/IO/SerializableObject.h>

#include "VE_Xplorer/GE/cfdStateInfo.h"

namespace VE_Xplorer
{
   class cfdModelHandler;
   class cfdCommandArray;
}

namespace VE_XML
{
   class DOMDocumentManager;
   class Command;
}

#include <vector>

namespace VE_Xplorer
{
class VjObs_i : public virtual POA_VjObs, //public virtual CorbaManager,
                public PortableServer::RefCountServantBase
{
public:
   
   VjObs_i();
   virtual ~VjObs_i(){;}
   
   void CreateGeometryInfo( void );
   void CreateDatasetInfo( void );
   void CreateTeacherInfo( void );

   void PreFrameUpdate( void );
   void InitCluster( void );
   void GetUpdateClusterStateVariables( void );

   // Frame sync variables used by osg only at this point
   float GetSetAppTime( float );
   long GetSetFrameNumber( long );
   ///Set the cluster mode flag
   void SetClusterMode( bool clusterFlag );
   bool GetClusterMode( void );
   
   VjObs::scalar_p* get_teacher_name() throw (CORBA::SystemException);
   VjObs::Models* GetModels() throw (CORBA::SystemException);
   VjObs::Model* GetModel( CORBA::Long modelID ) throw (CORBA::SystemException);
   short get_teacher_num() throw (CORBA::SystemException);
   void SetClientInfoFlag( short ) throw (CORBA::SystemException);
   void SetClientInfoData( const VjObs::obj_pd &value ) throw (CORBA::SystemException);
   VjObs::obj_pd* GetClientInfoData() throw (CORBA::SystemException);

   void GetCfdStateVariables( void );
   cfdCommandArray* _cfdArray;
   VE_XML::Command* bufferCommand;
   double cfdShort_data_array[ 9 ];
   
   std::vector< cfdCommandArray* > commandQueue;
protected:
   void CreateCommandQueue( void );

   VjObs::scalar_p_var teacher_name;
   VjObs::Models* _models;

   VjObs::obj_pd_var clientInfoObserverDataArray;
   int numOfClientInfo;
   //bool _unusedNewData;
   float time_since_start;
   float quatCamIncrement;
   long frameNumber;

   VjObs::obj_pd* getDouble1D( const char* input ) throw (CORBA::SystemException);

   VjObs::double2DArray* getDouble2D( const char* input ) throw (CORBA::SystemException);

   void SetCommandString( const char* value) throw (CORBA::SystemException);

   //virtual void setIsoValue( CORBA::Long value) throw (CORBA::SystemException);
   CORBA::Long getIsoValue( void ) throw (CORBA::SystemException);

   void setSc( CORBA::Long value) throw (CORBA::SystemException);

   short getPostdataState( void ) throw (CORBA::SystemException);

   short getTimesteps( void ) throw (CORBA::SystemException);

   vpr::Mutex mValueLock;  /**< A mutex to protect variables accesses */

   // Buffer variables...always right
   short mNumScalars;
   short mNnumVectors;
   short mNumGeoArrays;
   short mNumTeacherArrays;
   short mGetClientInfo;
   double mShort_data_array[ 9 ];

   VE_XML::DOMDocumentManager* domManager;
   std::vector< VE_XML::Command* > commandVectorQueue;
   std::vector< std::string > commandStringQueue;
   cfdCommandArray* _bufferArray;
   // Cluster Stuff for the above state variables
   cluster::UserData< vpr::SerializableObjectMixin< ClusterVariables::StateVariables > >  mStates;
   bool isCluster;
};
}
#endif
