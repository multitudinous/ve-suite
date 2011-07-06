#include <ves/xplorer/data/constraints/BoxConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
BoxConstraintPropertySet::BoxConstraintPropertySet()
{
    mTableName = "BoxConstraint";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr BoxConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new BoxConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void BoxConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );

    AddProperty( "LowerLimitX", false, "Lower Limit X" );
    AddProperty( "LowerLimitX_Limit", -200, "From" );
    AddProperty( "UpperLimitX", false, "Upper Limit X" );
    AddProperty( "UpperLimitX_Limit", 200, "To" );

    AddProperty( "LowerLimitY", false, "Lower Limit Y" );
    AddProperty( "LowerLimitY_Limit", -200, "From" );
    AddProperty( "UpperLimitY", false, "Upper Limit Y" );
    AddProperty( "UpperLimitY_Limit", 200, "To" );

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
