// --- My Includes --- //
#include "SlideEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
//#include <ves/xplorer/scenegraph/PhysicsSimulator.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture3D>

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
void SlideEntity::SetShaders()
{
    SetShaderOne();
}
////////////////////////////////////////////////////////////////////////////////
void SlideEntity::SetShaderOne()
{
    char vertexPass[]=
        "varying vec3 scaledPosition; \n"
        "varying vec3 normal; \n"
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"
            
            "float scale = 0.47; \n"
            "scaledPosition = scale * gl_Vertex.xyz; \n"
            "normal = gl_NormalMatrix * gl_Normal; \n"
            "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex); \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
        "} \n";

    char fragmentPass[] =
        "uniform sampler3D Noise; \n"

        "varying vec3 scaledPosition; \n"
        "varying vec3 normal; \n"
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"

        "void main() \n"
        "{ \n"
            "float snoise = 2.0 * texture3D( Noise, scaledPosition ).x - 1.0; \n"
            "float frequency = 113.0; \n"
            "float noiseScale = 10.90; \n"
            
            "float r = fract( frequency * scaledPosition.z + noiseScale * snoise ); \n"

            "float ringSharpness = 0.6; \n"
            "float invMax = pow( ringSharpness, ringSharpness / ( ringSharpness - 1.0 ) ) / ( ringSharpness - 1.0 ); \n"
            "float ring = invMax * ( r - pow( r, ringSharpness ) ); \n"

            "vec4 darkWood = vec4( 0.439216, 0.210526, 0.0, 1.0 ); \n"
            "vec4 lightWood = vec4( 0.517647, 0.403759, 0.227820, 1.0 ); \n"
            "float lrp = ring + snoise; \n"
            "vec4  base = mix( darkWood, lightWood, lrp ); \n"

            "vec3 N = normalize( normal ); \n"
            "vec3 L = normalize( lightPos ); \n"
            "vec3 V = normalize( eyePos ); \n"
            "vec3 R = reflect( V, N ); \n"
            
            "float diffuse = 0.5 + 0.5 * dot( N, L ); \n"
            "float specular = pow( max( dot( R, L ), 0.0 ), 10.0 ); \n"

            "float Kd = 0.89; \n"
            "float Ks = 0.66; \n"
            "gl_FragColor = Kd * diffuse * base + Ks * specular; \n"
        "} \n";

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );

    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    stateset->setAttribute( program.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Texture3D > texture = new osg::Texture3D();
    texture->setFilter( osg::Texture3D::MIN_FILTER, osg::Texture3D::LINEAR );
    texture->setFilter( osg::Texture3D::MAG_FILTER, osg::Texture3D::LINEAR );
    texture->setWrap( osg::Texture3D::WRAP_S, osg::Texture3D::REPEAT );
    texture->setWrap( osg::Texture3D::WRAP_T, osg::Texture3D::REPEAT );
    texture->setWrap( osg::Texture3D::WRAP_R, osg::Texture3D::REPEAT );
    texture->setImage( osgDB::readImageFile( "Textures/NoiseVolume.dds" ) );
    stateset->setTextureAttributeAndModes( 0, texture.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > noise = new osg::Uniform( "Noise", 0 );
    stateset->addUniform( noise.get() );
        
    GetNode()->GetNode()->setStateSet( stateset.get() );
    m_nonPhysicsGeometry->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo