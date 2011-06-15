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

class VE_DATA_EXPORTS CardanConstraintPropertySet : public ConstraintBasePropertySet
{
public:
    CardanConstraintPropertySet();

    /// Factory ctor
    virtual PropertySetPtr CreateNew();
private:
    void CreateSkeleton();
};

}
}
}
}
