#include <ves/xplorer/data/constraints/LinearSpringConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
LinearSpringConstraintPropertySet::LinearSpringConstraintPropertySet()
{
    mTableName = "LinearSpring";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr LinearSpringConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new LinearSpringConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void LinearSpringConstraintPropertySet::CreateSkeleton()
{
    SetPropertyAttribute( "Linear", "userVisible", true );
    SetPropertyAttribute( "Linear_RestLength", "userVisible", true );
    SetPropertyAttribute( "Linear_Stiffness", "userVisible", true );
    SetPropertyAttribute( "Linear_Damping", "userVisible", true );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
