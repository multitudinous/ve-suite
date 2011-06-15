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

class VE_DATA_EXPORTS SliderConstraintPropertySet : public ConstraintBasePropertySet
{
public:
    SliderConstraintPropertySet();

    /// Factory ctor
    virtual PropertySetPtr CreateNew();
private:
    void CreateSkeleton();
};

}
}
}
}
