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

class VE_DATA_EXPORTS SpringConstraintBasePropertySet : public ConstraintBasePropertySet
{
public:
    SpringConstraintBasePropertySet();
    virtual ~SpringConstraintBasePropertySet();

private:
    ///Create skeleton
    void CreateSkeleton();
};

}
}
}
}
