#ifndef _CONTOURPLANEPROPERTYSET_H
#define	_CONTOURPLANEPROPERTYSET_H

#include <ves/xplorer/data/PropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
class Property;


class ContourPlanePropertySet : public PropertySet
{
public:
    ContourPlanePropertySet( );
    ContourPlanePropertySet( const ContourPlanePropertySet& orig );
    virtual ~ContourPlanePropertySet( );

private:
    void LockIntToZero( Property* property );
    void UpdateModeOptions( Property* property );
    bool ValidateScalarMinMax( Property* property, boost::any value );
    void UpdateScalarDataOptions( Property* property );
    void UpdateScalarDataRange( Property* property );


private:
    void _createSkeleton( );
};

} // namespace data
} // namespace xplorer
} // namespace ves

#endif	/* _CONTOURPLANEPROPERTYSET_H */

