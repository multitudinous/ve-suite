#include <ves/xplorer/data/PartManipulatorPropertySet.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <switchwire/OptionalMacros.h>

#include <osg/Vec3d>
#include <osg/MatrixTransform>

#include <osgwTools/NodePathUtils.h>
#include <osgwTools/InsertRemove.h>

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/join.hpp>

namespace ves
{
namespace xplorer
{
namespace data
{

PartManipulatorPropertySet::PartManipulatorPropertySet()
    : m_transformNode( new osg::MatrixTransform )
{
    SetDataManager( DatabaseManager::instance()->GetDataManager() );
    SetTypeName( "PartManipulatorPropertySet" );

    CreateSkeleton();

    m_transformNode->addDescription( "CreatedByPartManipulatorPropertySet" );
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
    bool did_load = PropertySet::Load();

    if( !did_load )
    {
        return false;
    }

    // TODO: walk up the node path and load any parent PartManipulatorPropertySet
    std::string parent_uuid = boost::any_cast< std::string >( GetPropertyValue( "ParentUUID" ) );

    if( parent_uuid != "" )
    {
        PartManipulatorPropertySet parent;
        parent.SetUUID( parent_uuid );
        bool parent_did_load = parent.Load();

        if( !parent_did_load )
        {
            return false;
        }
    }

    std::string node_path_string = boost::any_cast< std::string >( GetPropertyValue( "NodePath" ) );

    std::vector< std::string > node_path_string_vector;

    boost::split( node_path_string_vector, node_path_string, boost::is_any_of(",") );

    osg::NodePath node_path = osgwTools::stringToNodePath(
        node_path_string,
        ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot()
    );

    if( node_path.size() == node_path_string_vector.size() )
    {
        // We found our "target" node in the scene graph using the node path.

        m_nodePath = node_path;

        // schedule the insertion of a new MatrixTransform node
        {
            boost::mutex::scoped_lock lock( m_connectionsLock );
            CONNECTSIGNAL_0( "App.LatePreFrame", void(),
                             &PartManipulatorPropertySet::InsertTransformNodeCallback,
                             m_connections, normal_Priority );
        }
    }
    else
    {
        // We did not find a matching node in the scene graph for the given node path.
        // It's possible that a PartManipulatorPropertySet for this node has already
        // been loaded once before, and that the scene has already been altered by
        // inserting a MatrixTransform above the target node.
        //
        // Try modifying the node path to include a MatrixTransform and search for the
        // target node again.

        std::string to_insert( "MatrixTransform:0" );
        node_path_string_vector.insert( node_path_string_vector.end(), to_insert );

        std::string modified_node_path_string = boost::join( node_path_string_vector, "," );

        osg::NodePath modified_node_path = osgwTools::stringToNodePath(
            modified_node_path_string,
            ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot()
        );

        if( node_path_string_vector.size() == modified_node_path.size() )
        {
            // we found the target node using the modified node path

            // TODO: check that the MatrixTransform was created by us
            // if it is, store a reference to it
        }
    }

    return true;
}

void PartManipulatorPropertySet::CreateSkeleton()
{
    AddProperty( "ParentUUID", "", "parent uuid" );

    AddProperty( "NodePath", "", "node path" );
    //AddProperty( "ModifiedNodePath", "", "modified node path" );

    AddProperty( "Transform_Translation_X", 0.0, "x" );
    AddProperty( "Transform_Translation_Y", 0.0, "y" );
    AddProperty( "Transform_Translation_Z", 0.0, "z" );

    AddProperty( "Transform_Rotation_X", 0.0, "x" );
    AddProperty( "Transform_Rotation_Y", 0.0, "y" );
    AddProperty( "Transform_Rotation_Z", 0.0, "z" ); 
}

void PartManipulatorPropertySet::UpdateTransformCallback()
{
    {
        boost::mutex::scoped_lock lock( m_dataLock );
        m_transformNode->setMatrix( m_transform );
    }

    {
        boost::mutex::scoped_lock lock( m_connectionsLock );
        m_connections.DropConnections();
    }
}

void PartManipulatorPropertySet::InsertTransformNodeCallback()
{
    // insert the MatrixTransform node into the scene graph
    osgwTools::insertAbove( m_nodePath.back(), m_transformNode.get() );

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
