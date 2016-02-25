#include <ves/xplorer/data/PartManipulatorPropertySet.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <switchwire/OptionalMacros.h>

#include <osg/Vec3d>
#include <osg/MatrixTransform>
#include <osg/BoundingBox>
#include <osg/Quat>

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

void PartManipulatorPropertySet::InitializeWithNodePath( const std::string& node_path )
{
    bool did_load_by_key = LoadByKey( "NodePath", node_path );

    if( did_load_by_key )
    {
        // the node path was found in the database

        // locate it in the scene graph
        osg::NodePath node_path_vector = osgwTools::stringToNodePath(
            node_path,
            ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot()
        );

        m_targetNode = node_path_vector.back();

        GetLocalToWorldRotation( node_path_vector );

        m_transform = CalculateNewTransform();
        m_transformNode->setMatrix( m_transform );

        // schedule the insertion of the MatrixTransform node
        {
            boost::mutex::scoped_lock lock( m_connectionsLock );
            CONNECTSIGNAL_0( "App.LatePreFrame", void(),
                             &PartManipulatorPropertySet::InsertTransformNodeCallback,
                             m_connections, normal_Priority );
        }
    }
    else
    {
        // the node path was not found in the database

        // locate it in the scene graph
        osg::NodePath node_path_vector = osgwTools::stringToNodePath(
            node_path,
            ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot()
        );

        m_targetNode = node_path_vector.back();

        GetLocalToWorldRotation( node_path_vector );

        // check if this node has a single parent that's a MatrixTransform created by us
        if( 1 == m_targetNode->getNumParents() )
        {
            if( osg::MatrixTransform* mt = dynamic_cast< osg::MatrixTransform* >( m_targetNode->getParent( 0 ) ) )
            {
                osg::Node::DescriptionList descs = mt->getDescriptions();

                osg::Node::DescriptionList::const_iterator i;

                for( i = descs.begin(); i != descs.end(); i++ )
                {
                    if( "CreatedByPartManipulatorPropertySet" == *i )
                    {
                        // re-use the existing MatrixTransform and property set
                        std::vector< std::string > node_path_string_vector;

                        boost::split( node_path_string_vector, node_path, boost::is_any_of(",") );

                        std::vector< std::string >::reverse_iterator find_it;
                        find_it = std::find( node_path_string_vector.rbegin(), node_path_string_vector.rend(), "\"MatrixTransform\"" );

                        if( find_it != node_path_string_vector.rend() )
                        {
                            node_path_string_vector.erase( (find_it + 1).base(), (find_it - 1).base() );
                            std::string modified_node_path_string = boost::join( node_path_string_vector, "," );

                            if( LoadByKey( "NodePath", modified_node_path_string ) )
                            {
                                m_transformNode = mt;
                                m_transform = mt->getMatrix();

                                ConnectValueChangedSignals();
                            }
                            else
                            {
                                std::cout << "ERROR: An existing transform created by us was found, "
                                          << "but the corresponding property set failed to load"
                                          << std::endl << std::flush;
                            }
                        }

                        return;
                    }
                }
            }
        }
        // if we reach this point, it means that a property set for this node path
        // was not found in the database, and that this node path appears to have
        // *not* been modified by us before

        SetPropertyValue( "NodePath", node_path );

        // schedule the insertion of the MatrixTransform node
        {
            boost::mutex::scoped_lock lock( m_connectionsLock );
            CONNECTSIGNAL_0( "App.LatePreFrame", void(),
                             &PartManipulatorPropertySet::InsertTransformNodeCallback,
                             m_connections, normal_Priority );
        }
    }
}

void PartManipulatorPropertySet::CreateSkeleton()
{
    std::string empty_string;
    AddProperty( "NodePath", empty_string, "node path" );

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
    osgwTools::insertAbove( m_targetNode, m_transformNode.get() );

    {
        boost::mutex::scoped_lock lock( m_connectionsLock );
        m_connections.DropConnections();
    }

    ConnectValueChangedSignals();
}

osg::Matrix PartManipulatorPropertySet::CalculateNewTransform()
{
    osg::Matrix translation = osg::Matrix::translate( boost::any_cast< double >( GetPropertyValue( "Transform_Translation_X" ) ),
                                                      boost::any_cast< double >( GetPropertyValue( "Transform_Translation_Y" ) ),
                                                      boost::any_cast< double >( GetPropertyValue( "Transform_Translation_Z" ) ) );

    osg::Matrix rotation = osg::Matrix::rotate( boost::any_cast< double >( GetPropertyValue( "Transform_Rotation_X" ) ),
                                                osg::Vec3d( 1.0, 0.0, 0.0 ),
                                                boost::any_cast< double >( GetPropertyValue( "Transform_Rotation_Y" ) ),
                                                osg::Vec3d( 0.0, 1.0, 0.0 ),
                                                boost::any_cast< double >( GetPropertyValue( "Transform_Rotation_Z" ) ),
                                                osg::Vec3d( 0.0, 0.0, 1.0 ) );

    osg::BoundingBox bbox;
    bbox.expandBy( m_targetNode->getBound() );
    osg::Vec3d bbox_center = bbox.center();

    osg::Matrix bbox_trans = osg::Matrix::translate( bbox_center );

    osg::Matrix trans = m_localToWorldRotation * translation * m_localToWorldRotationInverse;

    return osg::Matrix::identity() * osg::Matrix::inverse( bbox_trans ) * rotation * bbox_trans * trans;
}

void PartManipulatorPropertySet::CalculateNewTransformSlot( propertystore::PropertyPtr prop )
{
    osg::Matrix new_matrix = CalculateNewTransform();

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

void PartManipulatorPropertySet::ConnectValueChangedSignals()
{
    GetProperty( "Transform_Translation_X" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransformSlot, this, _1 )
    );
    GetProperty( "Transform_Translation_Y" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransformSlot, this, _1 )
    );
    GetProperty( "Transform_Translation_Z" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransformSlot, this, _1 )
    );

    GetProperty( "Transform_Rotation_X" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransformSlot, this, _1 )
    );
    GetProperty( "Transform_Rotation_Y" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransformSlot, this, _1 )
    );
    GetProperty( "Transform_Rotation_Z" )->SignalValueChanged.connect(
        boost::bind( &PartManipulatorPropertySet::CalculateNewTransformSlot, this, _1 )
    );
}

void PartManipulatorPropertySet::GetLocalToWorldRotation( const osg::NodePath& path )
{
    osg::Matrix m = osg::computeLocalToWorld( path );

    osg::Vec3d translation;
    osg::Quat rotation;
    osg::Vec3d scale;
    osg::Quat scale_orientation;

    m.decompose( translation, rotation, scale, scale_orientation );

    m_localToWorldRotation = osg::Matrix( rotation );
    m_localToWorldRotationInverse = osg::Matrix::inverse( m_localToWorldRotation );
}
} // namespace data
} // namespace xplorer
} // namespace ves
