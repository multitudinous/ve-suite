#include <ves/xplorer/data/PartManipulatorPropertySet.h>

#include <ves/xplorer/data/DatabaseManager.h>

namespace ves
{
namespace xplorer
{
namespace data
{

PartManipulatorPropertySet::PartManipulatorPropertySet()
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );
    SetTypeName( "PartManipulatorPropertySet" );

    CreateSkeleton();
}

PartManipulatorPropertySet::PartManipulatorPropertySet( const PartManipulatorPropertySet& orig )
    : PropertySet( orig )
{
}

PartManipulatorPropertySet::~PartManipulatorPropertySet()
{
}

propertystore::PropertySetPtr PartManipulatorPropertySet::CreateNew()
{
    return propertystore::PropertySetPtr( new PartManipulatorPropertySet() );
}

bool PartManipulatorPropertySet::Load()
{
    Load();

    // TODO: walk up the node path and load any parent PartManipulatorPropertySet

    // TODO: schedule the insertion of a new MatrixTransform node
}

void PartManipulatorPropertySet::CreateSkeleton()
{
    AddProperty( "ParentUUID", "", "parent uuid" );

    AddProperty( "NodePath", "", "node path" );

    AddProperty( "Transform_Translation_X", 0.0, "x" );
    AddProperty( "Transform_Translation_Y", 0.0, "y" );
    AddProperty( "Transform_Translation_Z", 0.0, "z" );

    AddProperty( "Transform_Rotation_X", 0.0, "x" );
    AddProperty( "Transform_Rotation_Y", 0.0, "y" );
    AddProperty( "Transform_Rotation_Z", 0.0, "z" ); 
}

void PartManipulatorPropertySet::UpdateTransformCallback()
{
    m_connections.DropConnections();
}

void PartManipulatorPropertySet::InsertTransformNodeCallback()
{
    // TODO: create a MatrixTransform node and insert it into the scene graph

    GetProperty( "Transform_Translation_X" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransform, this, _1 )
    );
    GetProperty( "Transform_Translation_Y" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransform, this, _1 )
    );
    GetProperty( "Transform_Translation_Z" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransform, this, _1 )
    );

    GetProperty( "Transform_Rotation_X" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransform, this, _1 )
    );
    GetProperty( "Transform_Rotation_Y" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransform, this, _1 )
    );
    GetProperty( "Transform_Rotation_Z" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransform, this, _1 )
    );

    m_connections.DropConnections();
}

void PartManipulatorPropertySet::CalculateNewTransform( propertystore::PropertyPtr prop )
{
    // TODO: calculate the new matrix

    // TODO: schedule the transfer of the new matrix to the scene graph
}

} // namespace data
} // namespace xplorer
} // namespace ves
