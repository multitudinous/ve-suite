// --- My Includes --- //
#include "QuarterEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
QuarterEntity::QuarterEntity( std::string geomFile,
                              ves::xplorer::scenegraph::DCS* pluginDCS,
                              ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
CADEntity( geomFile, pluginDCS, false, false, physicsSimulator )
{
    m_nonPhysicsGeometry = osgDB::readNodeFile( "Models/IVEs/quarter_front.ive" );
    m_nonPhysicsGeometryII = osgDB::readNodeFile( "Models/IVEs/quarter_back.ive" );

    GetDCS()->addChild( m_nonPhysicsGeometry.get() );
    GetDCS()->addChild( m_nonPhysicsGeometryII.get() );
}
////////////////////////////////////////////////////////////////////////////////
QuarterEntity::~QuarterEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void QuarterEntity::SetNameAndDescriptions( std::string geomFile )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( geomFile );
}
////////////////////////////////////////////////////////////////////////////////
void QuarterEntity::SetShaders()
{
    osg::ref_ptr< osg::Texture2D > quarterEdge =
        new osg::Texture2D( osgDB::readImageFile( "Textures/QuarterEdge.jpg" ) );
    quarterEdge->setWrap( osg::Texture::WRAP_S, osg::Texture::REPEAT );
    quarterEdge->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_BORDER );

    osg::ref_ptr< osg::Texture2D > quarterFront =
        new osg::Texture2D( osgDB::readImageFile( "Textures/QuarterFront.jpg" ) );
    osg::ref_ptr< osg::Texture2D > quarterBack =
        new osg::Texture2D( osgDB::readImageFile( "Textures/QuarterBack.jpg" ) );

    SetShaderOne( GetNode()->GetNode(), quarterEdge.get() );
    SetShaderOne( m_nonPhysicsGeometry.get(), quarterFront.get() );
    SetShaderOne( m_nonPhysicsGeometryII.get(), quarterBack.get() );
}
////////////////////////////////////////////////////////////////////////////////
void QuarterEntity::SetShaderOne( osg::Node* node, osg::Texture2D* texture )
{
    char vertexPass[]=
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "gl_TexCoord[ 0 ].xy = gl_MultiTexCoord0.xy; \n"
            "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
        "} \n";

    char fragmentPass[]=
        "uniform sampler2D baseColor; \n"

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

            "vec3 color = vec3( texture2D( baseColor, gl_TexCoord[ 0 ].xy ) ); \n"

            "vec3 TotalAmbient  = gl_LightSource[ 0 ].ambient.rgb  * color; \n"
            "vec3 TotalDiffuse  = gl_LightSource[ 0 ].diffuse.rgb  * color * NDotL; \n"
            "vec3 TotalSpecular = gl_LightSource[ 0 ].specular.rgb * color * pow( RDotL, 15.0 ); \n"

            "gl_FragColor = vec4( TotalAmbient + TotalDiffuse + TotalSpecular, 1.0 ); \n"
        "} \n";

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );

    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    stateset->setAttribute( program.get(), osg::StateAttribute::ON );

    stateset->setTextureAttributeAndModes( 0, texture, osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > baseColor = new osg::Uniform( "baseColor", 0 );
    stateset->addUniform( baseColor.get() );
        
    node->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo