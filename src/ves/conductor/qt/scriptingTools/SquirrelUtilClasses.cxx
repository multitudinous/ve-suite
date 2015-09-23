#include <ves/conductor/qt/scriptingTools/SquirrelUtilClasses.h>

#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/conductor/qt/VisFeatureManager.h>

#include <vpr/System.h>

#include <iostream>

using namespace ves::conductor;

// TweakStore
TweakStore::TweakStore()
    : m_transactionKey( 0 )
{
    ;
}

TweakStore::~TweakStore()
{
    delete m_transactionKey;
}

TweakStore::TweakStore( const TweakStore& rhs )
{
    m_transactionKey = rhs.m_transactionKey;
}

void TweakStore::OpenTransaction()
{
    m_transactionKey = new crunchstore::SQLiteTransactionKey( ves::xplorer::data::DatabaseManager::instance()->OpenBulkMode() );
}

void TweakStore::CloseTransaction()
{
    ves::xplorer::data::DatabaseManager::instance()->CloseBulkMode( *m_transactionKey );
}

crunchstore::SQLiteTransactionKey TweakStore::GetKey() const
{
    return *m_transactionKey;
}

// VizPropertySetWrapper
void VizPropertySetWrapper::CreateNewFeature( const std::string& featureType )
{
    m_set = ves::conductor::VisFeatureManager::instance()->CreateNewFeature( featureType );
}

void VizPropertySetWrapper::SetBoolPropertyValue( const std::string& key, bool value )
{
    m_set->SetPropertyValue( key, value );
}

void VizPropertySetWrapper::SetIntPropertyValue( const std::string& key, int value )
{
    m_set->SetPropertyValue( key, value );
}

void VizPropertySetWrapper::SetFloatPropertyValue( const std::string& key, float value )
{
    m_set->SetPropertyValue( key, value );
}

void VizPropertySetWrapper::SetDoublePropertyValue( const std::string& key, double value )
{
    m_set->SetPropertyValue( key, value );
}

void VizPropertySetWrapper::SetStringPropertyValue( const std::string& key, std::string value )
{
    m_set->SetPropertyValue( key, value );
}

std::string VizPropertySetWrapper::GetUUIDAsString()
{
    return m_set->GetUUIDAsString();
}

void VizPropertySetWrapper::Save()
{
    m_set->Save();
}

void VizPropertySetWrapper::BulkSave( TweakStore* tweakstore )
{
    m_set->Save( tweakstore->GetKey() );
}

// Sleeper
void Sleeper::Sleep( unsigned long time )
{
    vpr::System::msleep( time );
}

// Logger
Logger::Logger()
    : m_logger( Poco::Logger::has( "conductor.Squirrel" ) ?
          Poco::Logger::get( "conductor.Squirrel" ) :
          Poco::Logger::create( "conductor.Squirrel" , new Poco::ConsoleChannel ) ),
      m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    ;
}

void Logger::Info( const std::string& message )
{
    LOG_INFO( message );
}

void Logger::Notice( const std::string& message )
{
    LOG_NOTICE( message );
}

void Logger::Warning( const std::string& message )
{
    LOG_WARNING( message );
}

void Logger::Error( const std::string& message )
{
    LOG_ERROR( message );
}

// BaseObject
BaseObject::BaseObject()
    // get a reference to the Squirrel instance from the top of the stack
    : m_instance( Sqrat::DefaultVM::Get(), -1 )
{
    ;        
}

// BaseEvent
BaseEvent::BaseEvent()
{
    ;
}

// BaseState
BaseState::BaseState()
    : BaseObject()
{
    ;
}

void BaseState::_OnEnter( BaseContext* context )
{
    ; // dummy function - Squirrel subclasses that do not override OnEnter() will call this
}

void BaseState::OnEnter( BaseContext* context )
{
    try
    {
        Sqrat::Function( m_instance.value, "OnEnter" ).Execute< BaseContext* >( context );
    }
    catch( Sqrat::Exception& e )
    {
        std::cerr << "[BaseState::OnEnter] Oops: " << e.Message() << std::endl << std::flush;
    }
}

void BaseState::_OnExit( BaseContext* context )
{
    ; // dummy function - Squirrel subclasses that do not override OnExit() will call this
}

void BaseState::OnExit( BaseContext* context )
{
    try
    {
        Sqrat::Function( m_instance.value, "OnExit" ).Execute< BaseContext* >( context );
    }
    catch( Sqrat::Exception& e )
    {
        std::cerr << "[BaseState::OnExit] Oops: " << e.Message() << std::endl << std::flush;
    }
}

BaseState* BaseState::_OnEvent( BaseContext* context, BaseEvent* event )
{
    // dummy function - Squirrel subclasses that do not override OnEvent() will call this
    return static_cast< BaseState* >( 0 );    
}

BaseState* BaseState::OnEvent( BaseContext* context, BaseEvent* event )
{
    try
    {
        BaseState* new_state = Sqrat::Function( m_instance.value, "OnEvent" )
            .Evaluate< BaseState*, BaseContext*, BaseEvent* >( context, event );
        return new_state;
    }
    catch( Sqrat::Exception& e )
    {
        std::cerr << "[BaseState::OnEvent] Oops: " << e.Message() << std::endl << std::flush;
    }
}

// BaseContext
BaseContext::BaseContext()
    : m_state( static_cast< BaseState* >( 0 ) )
{
    ;
}

void BaseContext::SetInitialState( BaseState* state )
{
    m_state = state;
    m_state->OnEnter( this );
}

void BaseContext::HandleEvent( BaseEvent* event )
{
    if( m_state )
    {
        BaseState* new_state = m_state->OnEvent( this, event );
        if( new_state )
        {
            m_state->OnExit( this );
            m_state = new_state;
            m_state->OnEnter( this );
        }
    }
}

// CADPropertySetWrapper
CADPropertySetWrapper::CADPropertySetWrapper()
    : m_set( new ves::xplorer::data::CADPropertySet() )
{
    ;
}

void CADPropertySetWrapper::SetBoolPropertyValue( const std::string& key, bool value )
{
    m_set->SetPropertyValue( key, value );
}

bool CADPropertySetWrapper::GetBoolPropertyValue( const std::string& key )
{
    try
    {
        return boost::any_cast< bool >( m_set->GetPropertyValue( key ) );
    }
    catch( const boost::bad_any_cast& )
    {
        std::cerr << "Oops: no such bool property \"" << key << "\"" << std::endl << std::flush;
    }
}

void CADPropertySetWrapper::SetIntPropertyValue( const std::string& key, int value )
{
    m_set->SetPropertyValue( key, value );
}

int CADPropertySetWrapper::GetIntPropertyValue( const std::string& key )
{
    try
    {
        return boost::any_cast< int >( m_set->GetPropertyValue( key ) );
    }
    catch( const boost::bad_any_cast& )
    {
        std::cerr << "Oops: no such int property \"" << key << "\"" << std::endl << std::flush;
    }
}

void CADPropertySetWrapper::SetFloatPropertyValue( const std::string& key, float value )
{
    m_set->SetPropertyValue( key, value );
}

float CADPropertySetWrapper::GetFloatPropertyValue( const std::string& key )
{
    try
    {
        return boost::any_cast< float >( m_set->GetPropertyValue( key ) );
    }
    catch( const boost::bad_any_cast& )
    {
        std::cerr << "Oops: no such float property \"" << key << "\"" << std::endl << std::flush;
    }
}

void CADPropertySetWrapper::SetDoublePropertyValue( const std::string& key, double value )
{
    m_set->SetPropertyValue( key, value );
}

double CADPropertySetWrapper::GetDoublePropertyValue( const std::string& key )
{
    try
    {
        return boost::any_cast< double >( m_set->GetPropertyValue( key ) );
    }
    catch( const boost::bad_any_cast& )
    {
        std::cerr << "Oops: no such double property \"" << key << "\"" << std::endl << std::flush;
    }
}

void CADPropertySetWrapper::SetStringPropertyValue( const std::string& key, std::string value )
{
    m_set->SetPropertyValue( key, value );
}

std::string CADPropertySetWrapper::GetStringPropertyValue( const std::string& key )
{
    try
    {
        return boost::any_cast< std::string >( m_set->GetPropertyValue( key ) );
    }
    catch( const boost::bad_any_cast& )
    {
        std::cerr << "Oops: no such string property \"" << key << "\"" << std::endl << std::flush;
    }
}

void CADPropertySetWrapper::SetUUID( const std::string& uuid )
{
    m_set->SetUUID( uuid );
}

bool CADPropertySetWrapper::Load()
{
    return m_set->Load();
}

void CADPropertySetWrapper::EmitValueChangedSignals()
{
    m_set->EmitValueChangedSignals();
}

void CADPropertySetWrapper::EnableLiveProperties( bool live )
{
    m_set->EnableLiveProperties( live );
}
