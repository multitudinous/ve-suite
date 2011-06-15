#pragma once

#include <ves/xplorer/data/constraints/ConstraintBasePropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{

class VE_DATA_EXPORTS WheelSuspensionConstraintPropertySet : public ConstraintBasePropertySet
{
public:
    WheelSuspensionConstraintPropertySet();

    /// Factory ctor
    virtual PropertySetPtr CreateNew();

private:
    ///Create skeleton
    void CreateSkeleton();
};

}
}
}
}
