#include <ves/xplorer/data/constraints/TwistSliderConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
TwistSliderConstraintPropertySet::TwistSliderConstraintPropertySet()
{
    mTableName = "FixedConstraint";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr TwistSliderConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new TwistSliderConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void TwistSliderConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );

    AddProperty( "Bounce", 0, "Bounce (%)" );
    SetPropertyAttribute( "Bounce", "minimumValue", 0 );
    SetPropertyAttribute( "Bounce", "maximumValue", 100 );

    AddProperty( "LowerLimitZ", false, "Lower Limit Z" );
    AddProperty( "LowerLimitZ_Limit", -200, "From" );
    AddProperty( "UpperLimitZ", false, "Upper Limit Z" );
    AddProperty( "UpperLimitZ_Limit", 200, "To" );

    AddProperty( "AngularLimit", false, "Angular Limit" );
    AddProperty( "AngularLimit_From", 0, "From" );
    AddProperty( "AngularLimit_To", 180, "To" );

    AddProperty( "ObjectA_ReferenceAxisA", 0, "Reference Axis A" );
    std::vector<std::string> enumValues;
    enumValues.push_back("X of Object");
    enumValues.push_back("Y of Object");
    enumValues.push_back("Z of Object");
    SetPropertyAttribute( "ObjectA_ReferenceAxisA", "enumValues", enumValues );
    SetPropertyAttribute( "ObjectA_ReferenceAxisA", "userVisible", false );

    AddProperty( "ObjectB_ReferenceAxisB", 0, "Reference Axis B" );
    SetPropertyAttribute( "ObjectB_ReferenceAxisB", "enumValues", enumValues );
    SetPropertyAttribute( "ObjectB_ReferenceAxisB", "userVisible", false );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
