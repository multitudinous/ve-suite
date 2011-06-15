#pragma once

#include <ves/xplorer/data/PropertySet.h>
#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{

class VE_DATA_EXPORTS ConstraintBasePropertySet : public PropertySet
{
public:
    ConstraintBasePropertySet();
    virtual ~ConstraintBasePropertySet();
protected:
    void RegisterPropertySet( std::string const& tableName );
private:
    ///Create skeleton
    void CreateSkeleton();
};

}
}
}
}
