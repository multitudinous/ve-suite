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

class VE_DATA_EXPORTS HingeConstraintPropertySet : public ConstraintBasePropertySet
{
public:
    HingeConstraintPropertySet();

    /// Factory ctor
    virtual PropertySetPtr CreateNew();
private:
    void CreateSkeleton();
};

}
}
}
}