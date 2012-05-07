#include <Persistence/DataAbstractionLayer.h>
#include <Persistence/SearchCriterion.h>

namespace Persistence
{
////////////////////////////////////////////////////////////////////////////////
DataAbstractionLayer::DataAbstractionLayer()
{
}
////////////////////////////////////////////////////////////////////////////////
DataAbstractionLayer::~DataAbstractionLayer()
{

}
////////////////////////////////////////////////////////////////////////////////
void DataAbstractionLayer::Save( const Persistable& persistable, Role role  )
{
    if( m_child )
        m_child->Save( persistable, role );
}
////////////////////////////////////////////////////////////////////////////////
void DataAbstractionLayer::Load( Persistable& persistable, Role role )
{
    if( m_child )
        m_child->Load( persistable, role );
}
////////////////////////////////////////////////////////////////////////////////
void DataAbstractionLayer::Remove( Persistable& persistable,
                                   Role role )
{
    if( m_child )
        m_child->Remove( persistable );
}
////////////////////////////////////////////////////////////////////////////////
bool DataAbstractionLayer::HasIDForTypename( const boost::uuids::uuid& id, const std::string& typeName )
{
    return m_child->HasIDForTypename( id, typeName );
}
////////////////////////////////////////////////////////////////////////////////
void DataAbstractionLayer::GetIDsForTypename( const std::string& typeName, std::vector< std::string >& resultIDs )
{
    if( m_child )
        m_child->GetIDsForTypename( typeName, resultIDs );
}
////////////////////////////////////////////////////////////////////////////////
void DataAbstractionLayer::Search( const std::string& typeName,
                                   std::vector< SearchCriterion >& criteria,
                                   const std::string& returnField,
                                   std::vector< std::string >& results )
{
    if( m_child )
        m_child->Search( typeName, criteria, returnField, results );
}
////////////////////////////////////////////////////////////////////////////////
void DataAbstractionLayer::ProcessBackgroundTasks()
{
    // Do nothing in base case.
}
////////////////////////////////////////////////////////////////////////////////
void DataAbstractionLayer::SetChild( DataAbstractionLayerPtr child )
{
    m_child = child;
}
////////////////////////////////////////////////////////////////////////////////
void DataAbstractionLayer::Drop( const std::string& typeName, Role role )
{
    if( m_child )
        m_child->Drop( typeName, role );
}
////////////////////////////////////////////////////////////////////////////////
} // namespace Persistence

