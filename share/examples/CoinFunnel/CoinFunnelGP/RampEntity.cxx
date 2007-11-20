// --- My Includes --- //
#include "RampEntity.h"

// --- VE-Suite Includes --- //
//#include <ves/xplorer/scenegraph/PhysicsSimulator.h>

// --- OSG Includes --- //
#include <osg/Geometry>

//#include <osgDB/ReadFile>
//#include <osgDB/WriteFile>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
RampEntity::RampEntity( std::string geomFile, ves::xplorer::scenegraph::DCS* pluginDCS )
:
CADEntity( geomFile, pluginDCS )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RampEntity::~RampEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void RampEntity::SetNameAndDescriptions( std::string geomFile )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( geomFile );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo