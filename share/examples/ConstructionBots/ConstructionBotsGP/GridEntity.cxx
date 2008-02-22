// --- My Includes --- //
#include "GridEntity.h"
#include "Grid.h"

using namespace Construction;

////////////////////////////////////////////////////////////////////////////////
GridEntity::GridEntity( osg::ref_ptr< Construction::Grid > grid,
                        ves::xplorer::scenegraph::DCS* pluginDCS,
                        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
CADEntity( grid.get(), pluginDCS, physicsSimulator ),
geometry( grid.get() )
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
    m_dcs->setDescriptions( descriptorsList );

    m_dcs->setName( "Grid" );
}
////////////////////////////////////////////////////////////////////////////////
