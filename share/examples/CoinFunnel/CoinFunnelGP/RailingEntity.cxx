// --- My Includes --- //
#include "RailingEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

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
    m_nonPhysicsGeometryII = osgDB::readNodeFile( "Models/IVEs/railing_inlay.ive" );

    pluginDCS->addChild( m_nonPhysicsGeometry.get() );
    pluginDCS->addChild( m_nonPhysicsGeometryII.get() );
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
void RailingEntity::SetShaders( osg::TextureCubeMap* tcm )
{
    SetShaderOne( tcm );
}
////////////////////////////////////////////////////////////////////////////////
void RailingEntity::SetShaderOne( osg::TextureCubeMap* tcm )
{
    char vertexPass[]=
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
        "} \n";

    char fragmentPass[]=
        "uniform samplerCube Environment; \n"

        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "vec3 N = normalize( normal ); \n"
            "vec3 L = normalize( lightPos ); \n"
            "float NDotL = max( dot( N, L ), 0.0 ); \n"

            "vec3 V = normalize( eyePos ); \n"
            "vec3 R = reflect( V, N ); \n"
            "float RDotL = max( dot( R, L ), 0.0 ); \n"

            "vec3 color = vec3( 0.6, 0.6, 0.6 ); \n"
            "vec3 reflectedColor = textureCube( Environment, R ).rgb; \n"

            "vec3 mixColor = mix( color, reflectedColor, 0.4 ); \n"

            "vec3 TotalAmbient  = gl_LightSource[ 0 ].ambient.rgb  * mixColor; \n"
            "vec3 TotalDiffuse  = gl_LightSource[ 0 ].diffuse.rgb  * mixColor * NDotL; \n"
            "vec3 TotalSpecular = gl_LightSource[ 0 ].specular.rgb * mixColor * pow( RDotL, 15.0 ); \n"

            "gl_FragColor = vec4( TotalAmbient + TotalDiffuse + TotalSpecular, 1.0 ); \n"
        "} \n";

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );

    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    stateset->setAttribute( program.get(), osg::StateAttribute::ON );

    stateset->setTextureAttributeAndModes( 0, tcm, osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > Environment = new osg::Uniform( "Environment", 0 );
    stateset->addUniform( Environment.get() );
        
    GetNode()->GetNode()->setStateSet( stateset.get() );
    m_nonPhysicsGeometry->setStateSet( stateset.get() );
    m_nonPhysicsGeometryII->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo