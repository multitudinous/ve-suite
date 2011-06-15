#include <ves/xplorer/data/constraints/PlanarConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
PlanarConstraintPropertySet::PlanarConstraintPropertySet()
{
    mTableName = "FixedConstraint";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr PlanarConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new PlanarConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void PlanarConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );
    AddProperty( "LowerLimitX", false, "Lower Limit X" );
    AddProperty( "LowerLimitX_Limit", -200, "From" );
    AddProperty( "UpperLimitX", false, "Upper Limit X" );
    AddProperty( "UpperLimitX_Limit", 200, "To" );
    AddProperty( "LowerLimitZ", false, "Lower Limit Z" );
    AddProperty( "LowerLimitZ_Limit", -200, "From" );
    AddProperty( "UpperLimitZ", false, "Upper Limit Z" );
    AddProperty( "UpperLimitZ_Limit", 200, "To" );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
