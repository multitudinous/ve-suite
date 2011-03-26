#if 1
#include "EventFactory.h"

namespace ves
{
namespace xplorer
{
namespace eventmanager
{

vprSingletonImp( EventFactory );

EventFactory::EventFactory():
    m_logger( Poco::Logger::get("xplorer.EventFactory") ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    LOG_TRACE( "ctor" );

    EventManager* evm = EventManager::instance();

    ///Delete viz feature
    {
        SignalWrapperBase* swb =
                new SignalWrapper< ves::util::StringSignal_type >( &m_deleteVizSignal );
        evm->RegisterSignal( swb, "VizBasePropertySet.DeleteVizFeature",
            ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VizBasePropertySet.DeleteVizFeature" ] = swb;
    }

    ///Add viz feature
    {
        SignalWrapperBase* swb =
                new SignalWrapper< ves::util::TwoStringSignal_type >( &m_addVizSignal );
        evm->RegisterSignal( swb, "VizBasePropertySet.AddVizFeature",
            ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VizBasePropertySet.AddVizFeature" ] = swb;
    }
}

EventFactory::~EventFactory()
{

}

SignalWrapperBase* EventFactory::GetSignal( const std::string& signalName )
{
    std::map< std::string, SignalWrapperBase* >::const_iterator iter =
            m_signals.find( signalName );
    if( iter != m_signals.end() )
    {
        return iter->second;
    }
    else
    {
        return 0;
    }
}

}
}
}
#endif
