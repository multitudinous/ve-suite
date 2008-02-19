// --- My Includes --- //
#include "GridEntity.h"
#include "Grid.h"

using namespace Construction;

////////////////////////////////////////////////////////////////////////////////
GridEntity::GridEntity( Construction::Grid* grid,
                        ves::xplorer::scenegraph::DCS* pluginDCS,
                        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
CADEntity( grid, pluginDCS, physicsSimulator ),
geometry( grid )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
GridEntity::~GridEntity()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void GridEntity::SetNameAndDescriptions()
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );

    GetDCS()->setName( "Grid" );
}
////////////////////////////////////////////////////////////////////////////////
