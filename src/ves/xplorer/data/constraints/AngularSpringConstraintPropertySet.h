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

class VE_DATA_EXPORTS AngularSpringConstraintPropertySet : public SpringConstraintBasePropertySet
{
public:
    AngularSpringConstraintPropertySet();

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
