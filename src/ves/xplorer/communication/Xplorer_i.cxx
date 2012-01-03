/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <vpr/System.h>
#include <vpr/Util/Debug.h>

using namespace ves::open::xml;

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
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetParams( const char* param )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetID( ::CORBA::Long id )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
::CORBA::Long Body_VEXplorer_i::GetID( )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetName( const char* name )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
char* Body_VEXplorer_i::GetName()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetNetwork( const char* network )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetCommand( const char* command )
{
    /*//When starting xplorer it is possible to connect and send a command before
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
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::RegisterUI( const char* UIName, ::Body::UI_ptr ui )
{
    std::cout << "|\tBody_VEXplorer_i::RegisterUI Registering " << UIName << std::endl;
    uiCom[std::string( UIName )] = Body::UI::_duplicate( ui );
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::UnRegisterUI( const char* UIName )
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
