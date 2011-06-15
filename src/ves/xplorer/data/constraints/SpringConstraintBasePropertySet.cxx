#include <ves/xplorer/data/constraints/SpringConstraintBasePropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
SpringConstraintBasePropertySet::SpringConstraintBasePropertySet()
{
    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
SpringConstraintBasePropertySet::~SpringConstraintBasePropertySet()
{

}
////////////////////////////////////////////////////////////////////////////////
void SpringConstraintBasePropertySet::CreateSkeleton()
{
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

    AddProperty( "Linear", boost::any(), "Linear Spring" );
    SetPropertyAttribute( "Linear", "isUIGroupOnly", true );
    SetPropertyAttribute( "Linear", "userVisible", false );
    AddProperty( "Linear_RestLength", 100, "Rest Length" );
    SetPropertyAttribute( "Linear_RestLength", "minimumValue", 0 );
    SetPropertyAttribute( "Linear_RestLength", "userVisible", false );
    AddProperty( "Linear_Stiffness", 1, "Stiffness");
    SetPropertyAttribute( "Linear_Stiffness", "userVisible", false );
    SetPropertyAttribute( "Linear_Stiffness", "minimumValue", 0 );
    AddProperty( "Linear_Damping", 20, "Damping");
    SetPropertyAttribute( "Linear_Damping", "userVisible", false );
    SetPropertyAttribute( "Linear_Damping", "minimumValue", 0 );
    SetPropertyAttribute( "Linear_Damping", "maximumValue", 100 );


    AddProperty( "Angular", boost::any(), "Angular Spring" );
    SetPropertyAttribute( "Angular", "isUIGroupOnly", true );
    SetPropertyAttribute( "Angular", "userVisible", false );
    AddProperty( "Angular_RestAngle", 0, "Rest Angle" );
    SetPropertyAttribute( "Angular_RestAngle", "userVisible", false );
    AddProperty( "Angular_Stiffness", 1, "Stiffness");
    SetPropertyAttribute( "Angular_Stiffness", "userVisible", false );
    SetPropertyAttribute( "Angular_Stiffness", "minimumValue", 0 );
    AddProperty( "Angular_Damping", 20, "Damping");
    SetPropertyAttribute( "Angular_Damping", "userVisible", false );
    SetPropertyAttribute( "Angular_Damping", "minimumValue", 0 );
    SetPropertyAttribute( "Angular_Damping", "maximumValue", 100 );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
