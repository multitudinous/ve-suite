#include <ves/xplorer/data/constraints/CardanConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
CardanConstraintPropertySet::CardanConstraintPropertySet()
{
    mTableName = "CardanConstraint";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr CardanConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new CardanConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void CardanConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );

    AddProperty( "AngularLimit", false, "Angular Limit" );
    AddProperty( "AngularLimit_From", 0, "From" );
    AddProperty( "AngularLimit_To", 180, "To" );

    AddProperty( "AngularLimit2", false, "Angular Limit 2" );
    AddProperty( "AngularLimit2_From", 0, "From" );
    AddProperty( "AngularLimit2_To", 180, "To" );

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
