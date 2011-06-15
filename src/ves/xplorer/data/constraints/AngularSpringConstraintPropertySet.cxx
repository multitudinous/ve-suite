#include <ves/xplorer/data/constraints/AngularSpringConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
AngularSpringConstraintPropertySet::AngularSpringConstraintPropertySet()
{
    mTableName = "AngularSpring";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr AngularSpringConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new AngularSpringConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void AngularSpringConstraintPropertySet::CreateSkeleton()
{
    SetPropertyAttribute( "ObjectA_ReferenceAxisA", "userVisible", true );
    SetPropertyAttribute( "ObjectB_ReferenceAxisB", "userVisible", true );
    SetPropertyAttribute( "Angular", "userVisible", true );
    SetPropertyAttribute( "Angular_RestAngle", "userVisible", true );
    SetPropertyAttribute( "Angular_Stiffness", "userVisible", true );
    SetPropertyAttribute( "Angular_Damping", "userVisible", true );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
