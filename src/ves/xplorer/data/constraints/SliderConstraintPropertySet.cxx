#include <ves/xplorer/data/constraints/SliderConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
SliderConstraintPropertySet::SliderConstraintPropertySet()
{
    mTableName = "FixedConstraint";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr SliderConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new SliderConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void SliderConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );
    AddProperty( "Bounce", 0, "Bounce (%)" );
    SetPropertyAttribute( "Bounce", "minimumValue", 0 );
    SetPropertyAttribute( "Bounce", "maximumValue", 100 );
    AddProperty( "LowerLimitZ", false, "Lower Limit Z" );
    AddProperty( "LowerLimitZ_Limit", -200, "Limit" );
    AddProperty( "UpperLimitZ", false, "Upper Limit Z" );
    AddProperty( "UpperLimitZ_Limit", 200, "Limit" );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
