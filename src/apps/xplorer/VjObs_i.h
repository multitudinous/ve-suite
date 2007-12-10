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
#ifndef _VJOBS_I_H_
#define _VJOBS_I_H_
/*!\file VjObs_i.h
VjObs_i API
*/

/*!\class ves::xplorer::VjObs_i
*
*/
#include <ves/open/VjObsS.h>

//do this to remove compile warning on linux platforms
#undef _REENTRANT

#include <vpr/Sync/Mutex.h>

#include <cluster/ClusterManager.h>
#include <cluster/ClusterNetwork.h>

#include <vrj/vrjParam.h>
#if __VJ_version <= 2000003
#include <cluster/ClusterNode.h>
#endif

#include <plugins/ApplicationDataManager/UserData.h>

#include <vpr/IO/SerializableObject.h>

#include <ves/xplorer/StateInfo.h>
#include <ves/xplorer/ModelHandlerPtr.h>

#include <ves/open/xml/CommandPtr.h>

namespace ves
{
namespace open
{
namespace xml
{
class DOMDocumentManager;
}
}
}

#include <vector>

namespace ves
{
namespace xplorer
{
class VjObs_i : public virtual POA_VjObs, //public virtual CorbaManager,
            public PortableServer::RefCountServantBase
{
public:
    ///Constructor
    VjObs_i();
    ///Destructor
    virtual ~VjObs_i()
    {
        ;
    }
    ///Creates the geom info transfered to conductor
    ///may not be needed anymore
    void CreateGeometryInfo( void );
    ///Create the dataset info transferred to conductor
    ///May not be needed anymore
    void CreateDatasetInfo( void );
    ///Create the treacher info for conductor
    ///May not be needed anymore
    void CreateTeacherInfo( void );
    ///Called everyframe to update parameters
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
    ///Get the teacher name files
    VjObs::scalar_p* get_teacher_name() throw( CORBA::SystemException );
    ///Get the models in IDL format
    VjObs::Models* GetModels() throw( CORBA::SystemException );
    ///Get the model with a specific id
    ///\param modelID the model id to get
    VjObs::Model* GetModel( CORBA::Long modelID ) throw( CORBA::SystemException );
    ///Get the number of teacher files
    short get_teacher_num() throw( CORBA::SystemException );
    ///Set the client info flag
    ///\param flag the client info flags
    void SetClientInfoFlag( short ) throw( CORBA::SystemException );
    ///Set the client info data
    ///\param value the client info data
    void SetClientInfoData( const VjObs::obj_pd &value ) throw( CORBA::SystemException );
    ///Get the client info data
    VjObs::obj_pd* GetClientInfoData() throw( CORBA::SystemException );
    ///Called in latepreframe to sync state variables
    void GetCfdStateVariables( void );
    ///Create the vector with XML Commands to be used by Xplorer
    void CreatCommandVector( std::string commandString );
    ves::open::xml::Command* bufferCommand;///< Data to hold command data
    double cfdShort_data_array[ 9 ];///< hold command data shoudl be deleted in the future

protected:
    VjObs::scalar_p_var teacher_name; ///< hold the list of teacher file names
    VjObs::Models* _models;///< fold the list of IDL models

    VjObs::obj_pd_var clientInfoObserverDataArray; ///< hold the list of double arrays may not be needed
    int numOfClientInfo;///< number of teacher files
    //bool _unusedNewData;
    float time_since_start;///< start time
    float quatCamIncrement;///< should be removed
    long frameNumber;///< frame number

    ///Get 2d double array for a given input
    ///\param input the given input
    VjObs::obj_pd* getDouble1D( const char* input ) throw( CORBA::SystemException );
    ///Get 2d double array for a given input
    ///\param input the given input to get
    VjObs::double2DArray* getDouble2D( const char* input ) throw( CORBA::SystemException );
    ///Set the command string from conductor
    ///\param value the command
    void SetCommandString( const char* value ) throw( CORBA::SystemException );
    ///The iso value to get from xplorer
    CORBA::Long getIsoValue( void ) throw( CORBA::SystemException );
    ///Set sc
    ///\param value the sc
    void setSc( CORBA::Long value ) throw( CORBA::SystemException );
    ///get post data

    vpr::Mutex mValueLock;  ///< A mutex to protect variables accesses

    /// Buffer variables...always right
    short mNumScalars;///< scalar number
    short mNnumVectors;///< vector number
    short mNumGeoArrays;///< geo numerber
    short mNumTeacherArrays;///< teacher number
    short mGetClientInfo;///< client info number
    double mShort_data_array[ 9 ];///< buffer data

    ves::open::xml::DOMDocumentManager* domManager; ///< dom manger should be removed
    std::vector< ves::open::xml::Command* > commandVectorQueue;///< command vector may be a duplicate
    std::vector< std::string > commandStringQueue;///< command queue with raw string data
    // Cluster Stuff for the above state variables
    cluster::UserData< vpr::SerializableObjectMixin< ClusterVariables::StateVariables > >  mStates;
    bool isCluster;///<cluster mode
};
}
}
#endif
