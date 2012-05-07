#include <Persistence/DataManager.h>

namespace Persistence
{
////////////////////////////////////////////////////////////////////////////////
DataManager::DataManager()
{
    m_dataMultiplexer = DataAbstractionLayerPtr( new Multiplexer );
}
////////////////////////////////////////////////////////////////////////////////
DataManager::~DataManager()
{

}
////////////////////////////////////////////////////////////////////////////////
void DataManager::ProcessBackgroundTasks()
{

}
////////////////////////////////////////////////////////////////////////////////
void DataManager::AttachStore( DataAbstractionLayerPtr store,
                               Store::StoreRole role,
                               Store::SyncMode mode )
{
    static_cast< Multiplexer* >( m_dataMultiplexer.get() )->
            AttachStore( store, role, mode );
}
////////////////////////////////////////////////////////////////////////////////
void DataManager::DetachStore( DataAbstractionLayerPtr store )
{
    static_cast< Multiplexer* >( m_dataMultiplexer.get() )->
            DetachStore( store );
}
////////////////////////////////////////////////////////////////////////////////
void DataManager::SetCache( DataAbstractionLayerPtr cache )
{
    SetChild( cache );
}
////////////////////////////////////////////////////////////////////////////////
void DataManager::SetBuffer( DataAbstractionLayerPtr buffer )
{
    m_child->SetChild( buffer );
    m_buffer = buffer;
    m_buffer->SetChild( m_dataMultiplexer );
}
////////////////////////////////////////////////////////////////////////////////
void DataManager::Buffer( std::vector< std::string > ids,
                          BufferBase::BufferPriority priority )
{
    (static_cast< Cache* >(m_child.get()))->Buffer( ids, priority );
}
////////////////////////////////////////////////////////////////////////////////
} // namespace Persistence
