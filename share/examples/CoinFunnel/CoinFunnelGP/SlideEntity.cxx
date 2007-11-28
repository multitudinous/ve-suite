// --- My Includes --- //
#include "SlideEntity.h"

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
SlideEntity::SlideEntity( std::string geomFile,
                          ves::xplorer::scenegraph::DCS* pluginDCS,
                          ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
m_nonPhysicsGeometry( 0 ),
CADEntity( geomFile, pluginDCS, false, false, physicsSimulator )
{
    m_nonPhysicsGeometry = osgDB::readNodeFile( "Models/IVEs/slide.ive" );
    pluginDCS->addChild( m_nonPhysicsGeometry.get() );
}
////////////////////////////////////////////////////////////////////////////////
SlideEntity::~SlideEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SlideEntity::SetNameAndDescriptions( std::string geomFile )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( geomFile );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo