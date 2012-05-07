#include <Persistence/Multiplexer.h>
#include <Persistence/Persistable.h>

namespace Persistence
{

Multiplexer::Multiplexer()
{
}
////////////////////////////////////////////////////////////////////////////////
void Multiplexer::Save( const Persistable& persistable, Role role )
{
    if( role & WORKING_ROLE )
    {
        //std::cout << "Multiplexer::Save -- WORKING_ROLE" << std::endl;
        if( m_workingStore )
        {
            m_workingStore->Save( persistable );
        }
    }

    if( role & BACKING_ROLE )
    {
        //std::cout << "Multiplexer::Save -- BACKING_ROLE" << std::endl;
        std::vector< DataAbstractionLayerPtr >::iterator it = m_backingStores.begin();
        while( it != m_backingStores.end() )
        {
            (*it)->Save( persistable );
            ++it;
        }
    }

    if( role & VERSIONING_ROLE )
    {
        //std::cout << "Multiplexer::Save -- VERSIONING_ROLE" << std::endl;
        m_fullVersioningStore->Save( persistable );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Multiplexer::Load( Persistable& persistable, Role role )
{
    switch( role )
    {
    case VERSIONING_ROLE:
    {
        m_fullVersioningStore->Load( persistable );
        break;
    }
    case WORKING_ROLE:
    {
        m_workingStore->Load( persistable );
        break;
    }
    case DEFAULT_ROLE:
    {
        if( m_workingStore )
        {
            const boost::uuids::uuid id = persistable.GetUUID();
            // Find out which store contains this persistable and load from there.
            // If workingStore has it, load from there and move on
            // TODO: Should we be caching locations of IDs on load?
            if( m_workingStore->HasIDForTypename( id, persistable.GetTypeName() ) )
            {
                m_workingStore->Load( persistable );
                break;
            }
        }
        // If we didn't find it in the workingStore, fall through to backingStore.
    }
    case BACKING_ROLE:
    {
        // Find out which store contains this persistable and load from there.
        // TODO: Should we be caching locations of IDs on load?
        const boost::uuids::uuid id = persistable.GetUUID();
        std::vector< DataAbstractionLayerPtr >::iterator it = m_backingStores.begin();
        while( it != m_backingStores.end() )
        {
            if( (*it)->HasIDForTypename( id, persistable.GetTypeName() ) )
            {
                (*it)->Load( persistable );
                break;
            }
            ++it;
        }
        break;
    }
    default:
    {
        ;// do nothing
    }
    } // switch( role )
}
////////////////////////////////////////////////////////////////////////////////
void Multiplexer::Remove( Persistable& persistable, Role role )
{
    switch( role )
    {
    case VERSIONING_ROLE:
    {
        m_fullVersioningStore->Remove( persistable );
        break;
    }
    case WORKING_ROLE:
    {
        m_workingStore->Remove( persistable );
        break;
    }
    case DEFAULT_ROLE:
    {
        const boost::uuids::uuid id = persistable.GetUUID();
        if( m_workingStore->HasIDForTypename( id, persistable.GetTypeName() ) )
        {
            m_workingStore->Remove( persistable );
        }
        // DEFAULT_ROLE always falls through to BACKING_ROLE
    }
    case BACKING_ROLE:
    {
        // Find out which backingStore contains this persistable and load from there.
        const boost::uuids::uuid id = persistable.GetUUID();
        std::vector< DataAbstractionLayerPtr >::iterator it = m_backingStores.begin();
        while( it != m_backingStores.end() )
        {
            if( (*it)->HasIDForTypename( id, persistable.GetTypeName() ) )
            {
                (*it)->Remove( persistable );
                break;
            }
            ++it;
        }
        break;
    }
    default:
    {
        ;// do nothing
    }
    } // switch( role )
}
////////////////////////////////////////////////////////////////////////////////
void Multiplexer::GetIDsForTypename( const std::string& typeName,
                                               std::vector< std::string >& resultIDs,
                                               Role role )
{
    switch( role )
    {
    case VERSIONING_ROLE:
    {
        m_fullVersioningStore->GetIDsForTypename( typeName, resultIDs );
        break;
    }
    case WORKING_ROLE:
    {
        m_workingStore->GetIDsForTypename( typeName, resultIDs );
        break;
    }
    case DEFAULT_ROLE:
    {
        m_workingStore->GetIDsForTypename( typeName, resultIDs );
        // DEFAULT_ROLE always falls through to BACKING_ROLE
    }
    case BACKING_ROLE:
    {
        std::vector< DataAbstractionLayerPtr >::iterator it = m_backingStores.begin();
        while( it != m_backingStores.end() )
        {
            (*it)->GetIDsForTypename( typeName, resultIDs );
            ++it;
        }
        break;
    }
    default:
    {
        ;// do nothing
    }
    } // switch( role )
}
////////////////////////////////////////////////////////////////////////////////
void Multiplexer::Search( const std::string& typeName,
                          std::vector< SearchCriterion >& criteria,
                          const std::string& returnField,
                          std::vector< std::string >& results )
{
    // For now send all search requests to working_store. Need to think about
    // whether this method should take a role parameter, and if not, how to
    // search non=working role stores.
    if( m_workingStore )
    {
        m_workingStore->Search( typeName, criteria, returnField, results );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Multiplexer::ProcessBackgroundTasks()
{
    m_workingStore->ProcessBackgroundTasks();
    m_fullVersioningStore->ProcessBackgroundTasks();
    std::vector< DataAbstractionLayerPtr >::iterator it = m_backingStores.begin();
    while( it != m_backingStores.end() )
    {
        (*it)->ProcessBackgroundTasks();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Multiplexer::AttachStore( DataAbstractionLayerPtr store,
                                         Store::StoreRole role,
                                         Store::SyncMode mode )
{
    //std::cout << "Multiplexer::AttachStore" << std::endl;
    std::pair< DataAbstractionLayerPtr, Store::StoreRole > storePair( store, role );
    m_stores.push_back( storePair );

    if( role == Store::WORKINGSTORE_ROLE )
    {
        m_workingStore = store;
    }
    else if( role == Store::BACKINGSTORE_ROLE )
    {
        m_backingStores.push_back( store );
    }

    static_cast< Store* >(store.get())->Attach();
}
////////////////////////////////////////////////////////////////////////////////
void Multiplexer::DetachStore( DataAbstractionLayerPtr store )
{
    StoreListType::iterator it = m_stores.begin();
    while( it != m_stores.end() )
    {
        if( it->first == store  )
        {
            static_cast< Store* >(it->first.get())->Detach();
            m_stores.erase( it );
            break;
        }
        ++it;
    }
    throw "DetachStore: unable to find store.";
}
////////////////////////////////////////////////////////////////////////////////
void Multiplexer::Drop( const std::string& typeName, Role role  )
{
    switch( role )
    {
    case VERSIONING_ROLE:
    {
        m_fullVersioningStore->Drop( typeName, role );
        break;
    }
    case WORKING_ROLE:
    {
        m_workingStore->Drop( typeName, role );
        break;
    }
    case DEFAULT_ROLE:
    {
        m_workingStore->Drop( typeName, role );
        // DEFAULT_ROLE always falls through to BACKING_ROLE
    }
    case BACKING_ROLE:
    {
        std::vector< DataAbstractionLayerPtr >::iterator it = m_backingStores.begin();
        while( it != m_backingStores.end() )
        {
            (*it)->Drop( typeName, role );
            ++it;
        }
        break;
    }
    default:
    {
        ;// do nothing
    }
    } // switch( role )
}
////////////////////////////////////////////////////////////////////////////////
} // namespace Persistence
