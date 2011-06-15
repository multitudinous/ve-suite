#pragma once

#include<ves/xplorer/data/constraints/ConstraintBasePropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{

class VE_DATA_EXPORTS FixedConstraintPropertySet : public ConstraintBasePropertySet
{
public:
    FixedConstraintPropertySet();

    /// Factory ctor
    virtual PropertySetPtr CreateNew();
private:
    void CreateSkeleton();
};

}
}
}
}
