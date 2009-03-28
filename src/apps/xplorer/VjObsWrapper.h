/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CFD_VJOBSWRAPPER_H
#define CFD_VJOBSWRAPPER_H
/*!\file VjObsWrapper.h
VjObsWrapper API
*/

/*!\class ves::xplorer::VjObsWrapper
*
*/

#include <ves/open/xml/CommandPtr.h>

namespace ves
{
namespace xplorer
{
class VjObs_i;
}
}

class Body_VEXplorer_i;

namespace CosNaming
{
class NamingContext;
}
namespace CORBA
{
class ORB;
}
namespace PortableServer
{
class POA;
}

class ACE_Time_Value;
class ACE_Countdown_Time;

#include <vector>
#include <string>


namespace ves
{
namespace xplorer
{
class VjObsWrapper
{
public:
    ///Constructor
    VjObsWrapper( void );
    ///Destructor
    ~VjObsWrapper( void );
    ///init function to pass corba pointers arounf for registration purposes
    void init( CosNaming::NamingContext*, CORBA::ORB*, PortableServer::POA*, PortableServer::POA*, int, char** );
    ///get xml command data
    const ves::open::xml::CommandPtr& GetXMLCommand( void );
    ///get cfd state variables to be called by cfd app
    void GetCfdStateVariables( void );
    ///Called every frame
    void PreFrameUpdate( void );
    ///parse some command line input not sure what for
    int getStringTokens( const char* buffer, char* delim, std::vector<std::string> &toks ); // YANG, a string parsing utility, it is a not thread safe call.
    ///Initialize the cluster data
    void InitCluster( void );
    ///Get the clsuter data in preframe
    void GetUpdateClusterStateVariables( void );
    ///Get/set the app time
    float GetSetAppTime( float );
    ///Set the frame number for the app
    long GetSetFrameNumber( long );
    ///This should be removed as soon as the quat cam code is fixed
    bool IsMaster( void );
    ///Check to see if the orb needs to work. This ties the orb performance
    /// to the framerate
    void CheckORBWorkLoad();
    ///Cleanup
    void Cleanup();

    CosNaming::NamingContext* naming_context;///< holds the naming context for tao
    PortableServer::POA* child_poa;///< holds the poa server for tao
    PortableServer::POA* poa;///< holds the poa server for tao
    VjObs_i* _vjObs;///< holds the vjobs pointer for tao
    Body_VEXplorer_i* m_xplorer;///< holds the xplorer pointer for tao
private:
    ///Time out value used for pending orb work call
    ACE_Time_Value* mTimeOutValue;
    ///Time out value used for doing orb work call
    ACE_Time_Value* mTimeZero;
    ///Profile timer to pause for the ord
    ACE_Countdown_Time* mTimer;
    ///holds the orb pointer for tao
    CORBA::ORB* m_orbPtr;
    ///is the master should be removed
    bool isMaster;
};
}
}
#endif
