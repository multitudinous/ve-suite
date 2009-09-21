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
#include <ves/xplorer/communication/Xplorer_i.h>

//#include <ves/xplorer/ModelHandler.h>
//#include <ves/xplorer/EnvironmentHandler.h>
//#include <ves/xplorer/environment/cfdQuatCamHandler.h>
//#include <ves/xplorer/SteadyStateVizHandler.h>
//#include <ves/xplorer/Model.h>
//#include <ves/xplorer/environment/cfdDisplaySettings.h>
//#include <ves/xplorer/DataSet.h>
//#include <ves/xplorer/environment/cfdEnum.h>

//#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

//#include <ves/xplorer/TextureBasedVizHandler.h>
//#include <ves/xplorer/volume/cfdVolumeVisualization.h>
//using namespace ves::xplorer::volume;

#include <vpr/System.h>
#include <vpr/Util/Debug.h>

#include <jccl/RTRC/ConfigManager.h>

//using namespace ves::xplorer;
using namespace ves::open::xml;
//using namespace ves::xplorer::scenegraph;
//using namespace ves::xplorer::volume;

////////////////////////////////////////////////////////////////////////////////
Body_VEXplorer_i::Body_VEXplorer_i( void )
        :
        m_xplorerAMIHandler()
{
    bufferCommand = CommandPtr( new Command() );
    bufferCommand->AddDataValuePair( DataValuePairPtr( new DataValuePair() ) );
    bufferCommand->SetCommandName( "wait" );
    isCluster = false;
}
////////////////////////////////////////////////////////////////////////////////
Body_VEXplorer_i::~Body_VEXplorer_i( void )
{
    if( bufferCommand )
    {
        bufferCommand = CommandPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
char* Body_VEXplorer_i::GetStatusMessage( )
ACE_THROW_SPEC(( ::CORBA::SystemException,::Error::EUnknown ) )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetParams( const char* param )
ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ) )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetID( ::CORBA::Long id )
ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ) )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
::CORBA::Long Body_VEXplorer_i::GetID( )
ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ) )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetName( const char* name )
ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ) )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
char* Body_VEXplorer_i::GetName()
ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ) )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetNetwork( const char* network )
ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ) )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetCommand( const char* command )
ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ) )
{
    //When starting xplorer it is possible to connect and send a command before
    // xplorer is ready to receive it
    while( !jccl::ConfigManager::instance()->isPendingStale() )
    {
        vpr::System::msleep( 50 );  // 50 milli-second delay
    }

    vpr::Guard<vpr::Mutex> val_guard( mValueLock );

    std::string commandString( command );
    if( isCluster )
    {
        if( mStates.isLocal() )
        {
            commandStringQueue.push_back( commandString );
        }
    }

    vprDEBUG( vprDBG_ALL, 2 ) << "VjObs::SetCommandString(): " << std::endl << commandString << std::endl << vprDEBUG_FLUSH;
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( commandString, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    for( size_t i = 0; i < objectVector.size(); ++i )
    {
        commandVectorQueue.push_back( boost::dynamic_pointer_cast<ves::open::xml::Command>(  objectVector.at( i ) ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::RegisterUI( const char* UIName, ::Body::UI_ptr ui )
ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ) )
{
    std::cout << "Body_VEXplorer_i::RegisterUI Registering " << UIName << std::endl;
    uiCom[std::string( UIName )] = Body::UI::_duplicate( ui );
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::UnRegisterUI( const char* UIName )
ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ) )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::PreFrameUpdate( void )
{
    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
}
////////////////////////////////////////////////////////////////////////////////
float Body_VEXplorer_i::GetSetAppTime( float x )
{
    if( x == -1 )
    {
        return time_since_start;
    }
    else
    {
        time_since_start = x;
        return -1;
    }
}
////////////////////////////////////////////////////////////////////////////////
long Body_VEXplorer_i::GetSetFrameNumber( long x )
{
    if( x == -1 )
    {
        return frameNumber;
    }
    else
    {
        frameNumber = x;
        return -1;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetClusterMode( bool clusterFlag )
{
    isCluster = clusterFlag;
}
////////////////////////////////////////////////////////////////////////////////
bool Body_VEXplorer_i::GetClusterMode( void )
{
    return isCluster;
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetXplorerData( std::string input )
{
    ///AMI call
    Body::AMI_UIHandler_var xplorerComAMIHandler = m_xplorerAMIHandler._this();
    std::map<std::string, Body::UI_var>::iterator currentConductor;
    for( currentConductor = uiCom.begin();
            currentConductor != uiCom.end();
            ++currentConductor )
    {

        currentConductor->second->sendc_SetXplorerData( xplorerComAMIHandler.in(), input.c_str() );
    }
}
