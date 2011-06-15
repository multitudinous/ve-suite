#include <ves/xplorer/data/constraints/BallAndSocketConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
BallAndSocketConstraintPropertySet::BallAndSocketConstraintPropertySet()
{
    mTableName = "FixedConstraint";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr BallAndSocketConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new BallAndSocketConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void BallAndSocketConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
