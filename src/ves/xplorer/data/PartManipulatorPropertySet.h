#ifndef VES_XPLORER_DATA_PARTMANIPULATORPROPERTYSET_H
#define VES_XPLORER_DATA_PARTMANIPULATORPROPERTYSET_H

#include <propertystore/PropertySet.h>
#include <propertystore/PropertySetPtr.h>
#include <propertystore/Property.h>
#include <propertystore/PropertyPtr.h>

#include <switchwire/ScopedConnectionList.h>

#include <boost/thread/mutex.hpp>

#include <osg/Matrix>

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

    virtual bool Load();
private:
    void CreateSkeleton();

    void UpdateTransformCallback();

    void InsertTransformNodeCallback();

    void CalculateNewTransform( propertystore::PropertyPtr );

    switchwire::ScopedConnectionList m_connections;

    boost::mutex m_dataLock;
    boost::mutex m_connectionsLock;

    osg::Matrix m_transform;
};
}
}
}

#endif
