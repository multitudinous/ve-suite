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
#include <ves/ce/Body_AMI_UIHandler_i.h>

#include <boost/concept_check.hpp>

using namespace ves::ce;

////////////////////////////////////////////////////////////////////////////////
Body_AMI_UIHandler_i::Body_AMI_UIHandler_i( void )
{}
////////////////////////////////////////////////////////////////////////////////
Body_AMI_UIHandler_i::Body_AMI_UIHandler_i(
    PortableServer::POA_ptr p,
    Body::AMH_ExecutiveResponseHandler_ptr rh)
    :
    m_poa( PortableServer::POA::_duplicate( p ) ),
    m_responseHandler( Body::AMH_ExecutiveResponseHandler::_duplicate( rh ) )
{
    
}
// Implementation skeleton destructor
////////////////////////////////////////////////////////////////////////////////
Body_AMI_UIHandler_i::~Body_AMI_UIHandler_i( void )
{}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::UpdateNetwork( )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::UpdateNetwork_excep(
    ::Messaging::ExceptionHolder * excep_holder
)
{
    boost::ignore_unused_variable_warning( excep_holder );
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::UpdateModuleUI( )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::UpdateModuleUI_excep(
    ::Messaging::ExceptionHolder * excep_holder
)
{
    boost::ignore_unused_variable_warning( excep_holder );
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::UpdateModuleResult( )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::UpdateModuleResult_excep(
    ::Messaging::ExceptionHolder * excep_holder
)
{
    boost::ignore_unused_variable_warning( excep_holder );
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::UpdateLinkContent()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::UpdateLinkContent_excep(
    ::Messaging::ExceptionHolder * excep_holder
)
{
    boost::ignore_unused_variable_warning( excep_holder );
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::Raise()
{
    m_responseHandler->SetModuleMessage();
    
    //std::cout << "Body_AMI_UIHandler_i deactivating self" << std::endl;
    PortableServer::ObjectId_var oid = m_poa->servant_to_id( this );
    m_poa->deactivate_object( oid.in() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::Raise_excep(
    ::Messaging::ExceptionHolder * excep_holder
)
{
    // Here, we need to extract the exception from this holder, and package
    // it in another so the AMH response handler may forward it on.
    try
    {
        excep_holder->raise_exception();
    }
    catch(const CORBA::Exception& ex)
    {
        CORBA::Exception* local_ex = ex._tao_duplicate();
        ::Body::AMH_ExecutiveExceptionHolder amh_excep_holder( local_ex );
        m_responseHandler->SetModuleMessage_excep( &amh_excep_holder );
    }
    catch(...)
    {
        std::cout
            << "Raise_excep got an unknown exception"
            << std::endl;
        
        CORBA::Exception* unknown_ex = new CORBA::UNKNOWN;
        ::Body::AMH_ExecutiveExceptionHolder amh_excep_holder( unknown_ex );
        m_responseHandler->SetModuleMessage_excep( &amh_excep_holder );
    }
    
    //std::cout << "Body_AMI_UIHandler_i deactivating self" << std::endl;
    PortableServer::ObjectId_var oid = m_poa->servant_to_id( this );
    m_poa->deactivate_object( oid.in() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::SetXplorerData()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::SetXplorerData_excep(
    ::Messaging::ExceptionHolder * excep_holder
)
{
    boost::ignore_unused_variable_warning( excep_holder );
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::SetCommand()
{ 
    m_responseHandler->SetParams();
    
    //std::cout << "Body_AMI_UIHandler_i deactivating self" << std::endl;
    PortableServer::ObjectId_var oid = m_poa->servant_to_id( this );
    m_poa->deactivate_object( oid.in() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UIHandler_i::SetCommand_excep( 
    ::Messaging::ExceptionHolder * excep_holder)
{
    // Here, we need to extract the exception from this holder, and package
    // it in another so the AMH response handler may forward it on.
    try
    {
        excep_holder->raise_exception();
    }
    catch(const CORBA::Exception& ex)
    {
        CORBA::Exception* local_ex = ex._tao_duplicate();
        ::Body::AMH_ExecutiveExceptionHolder amh_excep_holder( local_ex );
        m_responseHandler->SetParams_excep( &amh_excep_holder );
    }
    catch(...)
    {
        std::cout
            << "SetParams_excep got an unknown exception"
            << std::endl;
        
        CORBA::Exception *unknown_ex = new CORBA::UNKNOWN;
        ::Body::AMH_ExecutiveExceptionHolder amh_excep_holder( unknown_ex );
        m_responseHandler->SetParams_excep( &amh_excep_holder );
    }
    
    //std::cout << "Body_AMI_UIHandler_i deactivating self" << std::endl;
    PortableServer::ObjectId_var oid = m_poa->servant_to_id(this);
    m_poa->deactivate_object (oid.in());
}
////////////////////////////////////////////////////////////////////////////////
