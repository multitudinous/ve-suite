/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <veWebService_i.h>
#include <iostream>
#include <vpr/Sync/Guard.h>
#include <cfdWebServices.h>

// Implementation skeleton constructor
veWebService_i::veWebService_i( Body::Executive_ptr exec, std::string name )
        : executive_( Body::Executive::_duplicate( exec ) )
{
    UIName_ = name;
}

// Implementation skeleton destructor
veWebService_i::~veWebService_i( void )
{}

// This returns the latest network string
// This is here so that the drawing sequence in vrjuggler
// doesn't thread lock with executive CORBA calls
std::string veWebService_i::GetNetworkString( void )
{
    printf( "webservice_i:  getting network string:  guarding mutex\n" );
    vpr::Guard<vpr::Mutex> val_guard( stringBufferLock );
    printf( "checking buffer\n" );
    if( !networkStringBuffer.empty() )
    {
        printf( "check 1\n" );
        std::vector< std::string >::iterator iter;
        printf( "check 2\n" );
        iter = networkStringBuffer.begin();
        std::string temp(( *iter ) );
        printf( "check 3\n" );
        networkStringBuffer.erase( iter );
        return temp;
    }
    else
    {
        printf( "buffer is empty\n" );
    }
    return "0";
}

// Complimentary function to the above function
void veWebService_i::SetNetworkString( char* temp )
{
    printf( "added %s to network string\n", temp );
    vpr::Guard<vpr::Mutex> val_guard( stringBufferLock );
    networkStringBuffer.push_back( std::string( temp ) );
}

bool veWebService_i::GetCalcFlag( void )
{
    vpr::Guard<vpr::Mutex> val_guard( stringBufferLock );
    if( !networkStringBuffer.empty() )
    {
        return true;
    }
    else
    {
        return false;
    }
}

void veWebService_i::UpdateNetwork(
    const char * network
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    if( network != NULL )
        std::cout << network << std::endl;
    std::cout << UIName_ << " :UpdateNetwork called" << std::endl;
}

void veWebService_i::UpdateModuleUI(
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    if( msg != NULL )
        std::cout << module_id << " : " << msg << std::endl;
    std::cout << UIName_ << " :UpdateModuleUI called" << std::endl;
}

void veWebService_i::UpdateModuleResult(
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    if( msg != NULL )
        std::cout << module_id << " : " << msg << std::endl;
    std::cout << UIName_ << " :UpdateModuleResult called" << std::endl;
}

void veWebService_i::UpdateLinkContent(
    CORBA::Long id,
    const char * msg
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    if( msg != NULL )
        std::cout << id << " : " << msg << std::endl;
    std::cout << UIName_ << " :UpdateLinkContent called" << std::endl;
}

void veWebService_i::setWebServices( cfdWebServices* webServe )
{
    webServices = webServe;

}


void veWebService_i::Raise(
    const char * notification
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // Add your implementation here
    if( notification != NULL )
    {
        std::cout << "|\tNotification Message : " << notification << " : Raise called " << std::endl
        << "|\tModule Being Called : " << UIName_ << " : Raise called" << std::endl;
        std::string temp( notification );
        if( !temp.compare( 0, 26, "Network execution complete" ) ||
                !temp.compare( 0, 30, "Successfully Scheduled Network" ) ||
                !temp.compare( 0, 22, "Connected to Executive" ) ||
                !temp.compare( 0, 17, "Error in Schedule" ) )
        {
            std::cout << "|\tGoing To Do Something" << std::endl;
            try
            {
                char* network = 0;
                network = executive_->GetNetwork();
                this->SetNetworkString( network );
                webServices->GetNetwork();
            }
            catch ( CORBA::Exception & )
            {
                std::cerr << "CFD DEBUG: cfdWebServices : no exec found! " << std::endl;
            }
        }
        else
        {
            std::cout << "|\tNot Going To Do Anything" << std::endl;
        }
        std::cout << "|\tEnd Raise " << std::endl;
    }
}

