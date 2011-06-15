#pragma once

#include <ves/xplorer/data/constraints/SpringConstraintBasePropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{

class VE_DATA_EXPORTS LinearSpringConstraintPropertySet : public SpringConstraintBasePropertySet
{
public:
    LinearSpringConstraintPropertySet();

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
