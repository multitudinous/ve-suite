// --- My Includes --- //
#include "RailingEntity.h"

// --- VE-Suite Includes --- //
//#include <ves/xplorer/scenegraph/PhysicsSimulator.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/TextureCubeMap>

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

    char fragmentPass[] =
        "uniform sampler2D Rainbow; \n"
        "uniform samplerCube Environment; \n"

        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "float ambient = 0.2; \n"
            "float rainbowScale = 0.6; \n"
            "float reflectionScale = 0.2; \n"
            "float refractionScale = 0.3; \n"
            "float indexOfRefractionRatio = 1.14; \n"
            "vec4 baseColor = vec4( 0.7, 0.7, 0.7, 1.0 ); \n"

            "vec3 N = normalize( normal ); \n"
            "vec3 V = normalize( eyePos ); \n"

            "vec3 R = reflect( V, N ); \n"
            "vec4 reflection = textureCube( Environment, R ); \n"

            "float cosine = dot( V, N ); \n"
            "float sine = sqrt( 1.0 - cosine * cosine ); \n"

            "float sine2 = clamp( indexOfRefractionRatio * sine, 0.0, 1.0 ); \n"
            "float cosine2 = sqrt( 1.0 - sine2 * sine2 ); \n"

            "vec3 x = N; \n"
            "vec3 y = normalize( cross( cross( V, N ), N ) ); \n"

            "vec3 refrVec = x * cosine2 + y * sine2; \n"
            "vec4 refraction = textureCube( Environment, -refrVec.xyz ); \n"

            "float v =  dot( normalize( V ), N ); \n"
            "vec4 rainbow = texture2D( Rainbow, vec2( v, 0.0 ) ); \n"

            //"vec4 rain = rainbowScale * rainbow * baseColor; \n"
            "vec4 rain = rainbowScale * rainbow; \n"
            "vec4 refl = reflectionScale * reflection; \n"
            "vec4 refr = refractionScale * refraction * baseColor; \n"

            "gl_FragColor = sine * refl + ( 1.0 - sine2 ) * refr + sine2 * rain + ambient; \n"
        "} \n";

    /*
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
    stateset->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );

    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setFunction( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    stateset->setAttribute( bf.get(), osg::StateAttribute::ON );

    stateset->setMode( GL_CULL_FACE, osg::StateAttribute::ON );
    osg::ref_ptr< osg::CullFace > cullFace = new osg::CullFace();
    cullFace->setMode( osg::CullFace::BACK );
    stateset->setAttribute( cullFace.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );

    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    stateset->setAttribute( program.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Texture2D > texture = new osg::Texture2D( osgDB::readImageFile( "Textures/Rainbow.tga" ) );
    stateset->setTextureAttributeAndModes( 0, tcm, osg::StateAttribute::ON );
    stateset->setTextureAttributeAndModes( 1, texture.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > Environment = new osg::Uniform( "Environment", 0 );
    stateset->addUniform( Environment.get() );

    osg::ref_ptr< osg::Uniform > Rainbow = new osg::Uniform( "Rainbow", 1 );
    stateset->addUniform( Rainbow.get() );
        
    GetNode()->GetNode()->setStateSet( stateset.get() );
    */
}
////////////////////////////////////////////////////////////////////////////////

} // end demo