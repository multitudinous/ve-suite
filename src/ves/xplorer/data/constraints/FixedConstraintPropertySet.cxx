#include <ves/xplorer/data/constraints/FixedConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
FixedConstraintPropertySet::FixedConstraintPropertySet()
{
    mTableName = "FixedConstraint";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr FixedConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new FixedConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void FixedConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
