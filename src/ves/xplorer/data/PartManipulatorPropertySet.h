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

    //virtual bool Load();
    void InitializeWithNodePath( const std::string& );
private:
    void CreateSkeleton();

    void UpdateTransformCallback();

    void InsertTransformNodeCallback();

    void ProcessNodePath( propertystore::PropertyPtr );
    void CalculateNewTransform( propertystore::PropertyPtr );

    switchwire::ScopedConnectionList m_connections;

    boost::mutex m_dataLock;
    boost::mutex m_connectionsLock;

    osg::Matrix m_transform;

    osg::ref_ptr< osg::MatrixTransform > m_transformNode;

    osg::NodePath m_nodePath;
};
}
}
}

#endif
