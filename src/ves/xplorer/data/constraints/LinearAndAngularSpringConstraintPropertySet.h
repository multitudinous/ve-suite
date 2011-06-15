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

class VE_DATA_EXPORTS LinearAndAngularSpringConstraintPropertySet :
        public SpringConstraintBasePropertySet
{
public:
    LinearAndAngularSpringConstraintPropertySet();

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
