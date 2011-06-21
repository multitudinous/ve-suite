#include <ves/xplorer/data/constraints/ConstraintBasePropertySet.h>
#include <string>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
ConstraintBasePropertySet::ConstraintBasePropertySet()
{
    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
ConstraintBasePropertySet::~ConstraintBasePropertySet()
{

}
////////////////////////////////////////////////////////////////////////////////
void ConstraintBasePropertySet::RegisterPropertySet( std::string const& tableName )
{
    std::string prependTag( tableName );
    prependTag.append(" ");
    std::string tag = boost::any_cast<std::string>(GetPropertyValue("NameTag"));
    SetPropertyValue( "NameTag", tag.insert( 0, prependTag ) );
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintBasePropertySet::CreateSkeleton()
{
    AddProperty( "ObjectA", std::string( "" ), "Object A" );
    SetPropertyAttribute( "ObjectA", "isNodePath", true );
    AddProperty( "ObjectB", std::string( "" ), "Object B" );
    SetPropertyAttribute( "ObjectB", "isNodePath", true );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
