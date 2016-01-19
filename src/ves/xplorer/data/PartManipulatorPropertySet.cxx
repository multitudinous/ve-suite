#include <ves/xplorer/data/PartManipulatorPropertySet.h>

#include <ves/xplorer/data/DatabaseManager.h>

#include <switchwire/OptionalMacros.h>

#include <osg/Vec3d>

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

    // schedule the insertion of a new MatrixTransform node
    {
        boost::mutex::scoped_lock lock( m_connectionsLock );
        CONNECTSIGNAL_0( "App.LatePreFrame", void(), &PartManipulatorPropertySet::InsertTransformNodeCallback,
                         m_connections, normal_Priority );
    }
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
    // TODO: update the MatrixTransform node with the new matrix

    {
        boost::mutex::scoped_lock lock( m_connectionsLock );
        m_connections.DropConnections();
    }
}

void PartManipulatorPropertySet::InsertTransformNodeCallback()
{
    // TODO: create a MatrixTransform node and insert it into the scene graph

    {
        boost::mutex::scoped_lock lock( m_connectionsLock );
        m_connections.DropConnections();
    }

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
}

void PartManipulatorPropertySet::CalculateNewTransform( propertystore::PropertyPtr prop )
{
    osg::Matrix trans_component = osg::Matrix::translate( boost::any_cast< double >( GetPropertyValue( "Transform_Translation_X" ) ),
                                                          boost::any_cast< double >( GetPropertyValue( "Transform_Translation_Y" ) ),
                                                          boost::any_cast< double >( GetPropertyValue( "Transform_Translation_Z" ) ) );

    osg::Matrix rot_component = osg::Matrix::rotate( boost::any_cast< double >( GetPropertyValue( "Transform_Rotation_X" ) ),
                                                     osg::Vec3d( 1.0, 0.0, 0.0 ),
                                                     boost::any_cast< double >( GetPropertyValue( "Transform_Rotation_Y" ) ),
                                                     osg::Vec3d( 0.0, 1.0, 0.0 ),
                                                     boost::any_cast< double >( GetPropertyValue( "Transform_Rotation_Z" ) ),
                                                     osg::Vec3d( 0.0, 0.0, 1.0 ) );

    osg::Matrix new_matrix = osg::Matrix::identity() * trans_component * rot_component;

    {
        boost::mutex::scoped_lock lock( m_dataLock );
        m_transform = new_matrix;
    }

    // schedule the transfer of the new matrix to the scene graph
    {
        boost::mutex::scoped_lock lock( m_connectionsLock );
        CONNECTSIGNAL_0( "App.LatePreFrame", void(), &PartManipulatorPropertySet::UpdateTransformCallback,
                         m_connections, normal_Priority );
    }
}

} // namespace data
} // namespace xplorer
} // namespace ves
