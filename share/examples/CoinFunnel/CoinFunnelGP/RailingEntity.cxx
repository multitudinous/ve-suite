// --- My Includes --- //
#include "RailingEntity.h"

// --- VE-Suite Includes --- //
//#include <ves/xplorer/scenegraph/PhysicsSimulator.h>

// --- OSG Includes --- //
#include <osg/Geometry>

#include <osgDB/ReadFile>
//#include <osgDB/WriteFile>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
RailingEntity::RailingEntity( std::string geomFile,
                              ves::xplorer::scenegraph::DCS* pluginDCS,
                              ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
CADEntity( geomFile, pluginDCS, false, false, physicsSimulator ),
m_nonPhysicsGeometry( 0 )
{
    m_nonPhysicsGeometry = osgDB::readNodeFile( "Models/IVEs/railing.ive" );
    GetDCS()->addChild( m_nonPhysicsGeometry.get() );
}
////////////////////////////////////////////////////////////////////////////////
RailingEntity::~RailingEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void RailingEntity::SetNameAndDescriptions( std::string geomFile )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( geomFile );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo