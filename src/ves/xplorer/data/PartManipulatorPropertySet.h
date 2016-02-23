#ifndef VES_XPLORER_DATA_PARTMANIPULATORPROPERTYSET_H
#define VES_XPLORER_DATA_PARTMANIPULATORPROPERTYSET_H

#include <propertystore/PropertySet.h>
#include <propertystore/PropertySetPtr.h>
#include <propertystore/Property.h>
#include <propertystore/PropertyPtr.h>

#include <switchwire/ScopedConnectionList.h>

#include <boost/thread/mutex.hpp>

#include <osg/Node>
#include <osg/Matrix>
#include <osg/ref_ptr>

// forward declaration
namespace osg { class MatrixTransform; }

namespace ves
{
namespace xplorer
{
namespace data
{
class PartManipulatorPropertySet : public propertystore::PropertySet
{
public:
    PartManipulatorPropertySet();

    PartManipulatorPropertySet( const PartManipulatorPropertySet& orig );

    virtual ~PartManipulatorPropertySet();

    virtual propertystore::PropertySetPtr CreateNew();

    void InitializeWithNodePath( const std::string& );
private:
    void CreateSkeleton();

    void UpdateTransformCallback();

    void InsertTransformNodeCallback();

    osg::Matrix CalculateNewTransform();
    void CalculateNewTransformSlot( propertystore::PropertyPtr );

    void ConnectValueChangedSignals();

    void GetLocalToWorldRotation( const osg::NodePath& );

    switchwire::ScopedConnectionList m_connections;

    boost::mutex m_dataLock;
    boost::mutex m_connectionsLock;

    osg::Matrix m_transform;
    osg::Matrix m_localToWorldRotation;
    osg::Matrix m_localToWorldRotationInverse;

    osg::ref_ptr< osg::MatrixTransform > m_transformNode;
    osg::ref_ptr< osg::Node > m_targetNode;
};
}
}
}

#endif
