#include <ves/open/idl/Body_AMI_UnitHandler_i.h>

////////////////////////////////////////////////////////////////////////////////
// Implementation skeleton constructor
Body_AMI_UnitHandler_i::Body_AMI_UnitHandler_i()
{
}
////////////////////////////////////////////////////////////////////////////////
// Implementation skeleton destructor
Body_AMI_UnitHandler_i::~Body_AMI_UnitHandler_i()
{
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::StartCalc()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::StartCalc_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::StopCalc()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::StopCalc_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::PauseCalc()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::PauseCalc_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::Resume()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::Resume_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetStatusMessage( const char* ami_return_val )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetStatusMessage_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetUserData( const char* ami_return_val )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetUserData_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::SetParams()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::SetParams_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::SetID()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::SetID_excep (
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::SetCurID()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::SetCurID_excep(
    ::Messaging::ExceptionHolder* excep_holder )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetID( const ::Types::ArrayLong& ami_return_val )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetID_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetCurID( ::CORBA::Long ami_return_val )
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetCurID_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::SetName()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::SetName_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetName( const char* ami_return_val)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::GetName_excep(
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::Query( const char* ami_return_val )
{
    m_responseHandler->Query( ami_return_val );
    
    std::cout << "Body_AMI_UnitHandler_i deactivating self" << std::endl;
    PortableServer::ObjectId_var oid = m_poa->servant_to_id( this );
    m_poa->deactivate_object( oid.in() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::Query_excep (
    ::Messaging::ExceptionHolder* excep_holder)
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
        m_responseHandler->Query_excep( &amh_excep_holder );
    }
    catch(...)
    {
        std::cout
            << "Query_excep got an unknown exception"
            << std::endl;
        
        CORBA::Exception *unknown_ex = new CORBA::UNKNOWN;
        ::Body::AMH_ExecutiveExceptionHolder amh_excep_holder( unknown_ex );
        m_responseHandler_->Query_excep( &amh_excep_holder );
    }
    
    std::cout << "Body_AMI_UnitHandler_i deactivating self" << std::endl;
    PortableServer::ObjectId_var oid = m_poa->servant_to_id(this);
    m_poa->deactivate_object (oid.in());
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::DeleteModuleInstance()
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMI_UnitHandler_i::DeleteModuleInstance_excep (
    ::Messaging::ExceptionHolder* excep_holder)
{
    // Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
