// --- My Includes --- //
#include "WaterEntity.h"

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
WaterEntity::WaterEntity( std::string geomFile, ves::xplorer::scenegraph::DCS* pluginDCS )
:
CADEntity( geomFile, pluginDCS )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
WaterEntity::~WaterEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void WaterEntity::SetNameAndDescriptions( std::string geomFile )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( geomFile );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo