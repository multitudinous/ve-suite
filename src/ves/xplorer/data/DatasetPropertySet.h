#ifndef _DATASETPROPERTYSET_H
#define	_DATASETPROPERTYSET_H

#include <ves/xplorer/data/PropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
class Property;


class DatasetPropertySet : public PropertySet
{
public:
    DatasetPropertySet( );
    DatasetPropertySet( const DatasetPropertySet& orig );
    virtual ~DatasetPropertySet( );

protected:

    
private:
    void _createSkeleton( );

};

} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* _DATASETPROPERTYSET_H */

