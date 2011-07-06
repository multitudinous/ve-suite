#include <ves/xplorer/data/constraints/WheelSuspensionConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
WheelSuspensionConstraintPropertySet::WheelSuspensionConstraintPropertySet()
{
    mTableName = "WheelSuspensionConstraint";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr WheelSuspensionConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new WheelSuspensionConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void WheelSuspensionConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );

    AddProperty( "SteeringAngle", 0, "Steering Angle" );

    AddProperty( "RestLength", 100, "Suspension Rest Position" );
    SetPropertyAttribute( "RestLength", "minimumValue", 0 );

    AddProperty( "Stiffness", 1, "Suspension Stiffness");
    SetPropertyAttribute( "Stiffness", "minimumValue", 0 );

    AddProperty( "Damping", 20, "Suspension Damping (%)");
    SetPropertyAttribute( "Damping", "minimumValue", 0 );
    SetPropertyAttribute( "Damping", "maximumValue", 100 );

    AddProperty( "LowerLimitY", false, "Lower Limit Y" );
    AddProperty( "LowerLimitY_Limit", -200, "From" );
    AddProperty( "UpperLimitY", false, "Upper Limit Y" );
    AddProperty( "UpperLimitY_Limit", 200, "To" );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
