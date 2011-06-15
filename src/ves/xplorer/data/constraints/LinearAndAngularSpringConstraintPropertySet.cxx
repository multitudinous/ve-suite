#include <ves/xplorer/data/constraints/LinearAndAngularSpringConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
LinearAndAngularSpringConstraintPropertySet::LinearAndAngularSpringConstraintPropertySet()
{
    mTableName = "LinearAndAngularSpring";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr LinearAndAngularSpringConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new LinearAndAngularSpringConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void LinearAndAngularSpringConstraintPropertySet::CreateSkeleton()
{

    SetPropertyAttribute( "ObjectA_ReferenceAxisA", "userVisible", true );
    SetPropertyAttribute( "ObjectB_ReferenceAxisB", "userVisible", true );

    SetPropertyAttribute( "Linear", "userVisible", true );
    SetPropertyAttribute( "Linear_RestLength", "userVisible", true );
    SetPropertyAttribute( "Linear_Stiffness", "userVisible", true );
    SetPropertyAttribute( "Linear_Damping", "userVisible", true );

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
