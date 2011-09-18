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

    //Delete viz feature
    {
        SignalWrapperBase* swb =
                new SignalWrapper< ves::util::StringSignal_type >( &m_deleteVizSignal );
        evm->RegisterSignal( swb, "VizBasePropertySet.DeleteVizFeature",
            ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VizBasePropertySet.DeleteVizFeature" ] = swb;
    }

    //Add viz feature
    {
        SignalWrapperBase* swb =
                new SignalWrapper< ves::util::TwoStringSignal_type >( &m_addVizSignal );
        evm->RegisterSignal( swb, "VizBasePropertySet.AddVizFeature",
            ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VizBasePropertySet.AddVizFeature" ] = swb;
    }

    //Ves File Loading
    {
        SignalWrapperBase* swb =
                new SignalWrapper< ves::util::StringSignal_type >( &m_vesFileLoadingSignal );
        evm->RegisterSignal( swb, "VesFileLoading",
            ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VesFileLoading" ] = swb;
    }

    //Ves File Loaded
    {
        SignalWrapperBase* swb =
                new SignalWrapper< ves::util::StringSignal_type >( &m_vesFileLoadedSignal );
        evm->RegisterSignal( swb, "VesFileLoaded",
            ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VesFileLoaded" ] = swb;
    }
    //Working Directory has changed
    {
        SignalWrapperBase* swb =
                new SignalWrapper< ves::util::StringSignal_type >( &m_workingDirChangedSignal );
        evm->RegisterSignal( swb, "WorkingDirectoryChanged",
            ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "WorkingDirectoryChanged" ] = swb;
    }
    // Delete a CAD node
    {
        SignalWrapperBase* swb =
                new SignalWrapper< ves::util::ThreeStringSignal_type >( &m_deleteCADNodeSignal );
        evm->RegisterSignal( swb, "DeleteCADNode",
            ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "DeleteCADNode" ] = swb;
    }
    //Change the active model
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::StringSignal_type >( &m_changeActiveModelSignal );
        evm->RegisterSignal( swb, "ChangeActiveModel",
                            ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "ChangeActiveModel" ] = swb;
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
