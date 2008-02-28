// --- My Includes --- //
#include "FunnelEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

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
FunnelEntity::FunnelEntity( std::string geomFile,
                            ves::xplorer::scenegraph::DCS* pluginDCS,
                            ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
m_nonPhysicsGeometry( new osg::Group() ),
m_nonPhysicsGeometryII( new osg::Group() ),
CADEntity( geomFile, pluginDCS, false, false, physicsSimulator )
{
    osg::ref_ptr< osg::Node > base = osgDB::readNodeFile( "Models/IVEs/base.ive" );
    osg::ref_ptr< osg::Node > column = osgDB::readNodeFile( "Models/IVEs/column.ive" );
    osg::ref_ptr< osg::Node > columnTop = osgDB::readNodeFile( "Models/IVEs/column_top.ive" );
    osg::ref_ptr< osg::Node > columnBase = osgDB::readNodeFile( "Models/IVEs/column_base.ive" );
    osg::ref_ptr< osg::Node > columnDetail = osgDB::readNodeFile( "Models/IVEs/column_detail.ive" );

    m_nonPhysicsGeometry->addChild( base.get() );
    m_nonPhysicsGeometryII->addChild( column.get() );
    m_nonPhysicsGeometryII->addChild( columnTop.get() );
    m_nonPhysicsGeometryII->addChild( columnBase.get() );
    m_nonPhysicsGeometryII->addChild( columnDetail.get() );
    GetDCS()->addChild( m_nonPhysicsGeometry.get() );
    GetDCS()->addChild( m_nonPhysicsGeometryII.get() );
}
////////////////////////////////////////////////////////////////////////////////
FunnelEntity::~FunnelEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FunnelEntity::SetNameAndDescriptions( std::string geomFile )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( geomFile );
}
////////////////////////////////////////////////////////////////////////////////
void FunnelEntity::SetShaders()
{
    SetShaderOne();
    SetShaderTwo();
    SetShaderThree();
}
////////////////////////////////////////////////////////////////////////////////
void FunnelEntity::SetShaderOne()
{
    char vertexPass[]=
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "gl_TexCoord[ 0 ].xyz = gl_Vertex.xyz * 0.5; \n"
            "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = gl_NormalMatrix * gl_Normal; \n"
        "} \n";

    char fragmentPass[]=
        "uniform sampler3D noise; \n"

        "varying vec3  lightPos; \n"
        "varying vec3  eyePos; \n"
        "varying vec3  normal; \n"

        ////////////////////////////////////////////////////////////////////////////////
        "vec3 Snoise( vec3 x ) \n"
        "{ \n"
            "return 2.0 * texture3D( noise, x ).xyz - 1.0; \n"
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "vec3 Noisy( vec3 x ) \n"
        "{ \n"
            "return texture3D( noise, x ).xyz; \n"
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "vec4 Ambient() \n"
        "{ \n"
            "return vec4( 0.51, 0.51, 0.51, 1.0 ); \n"
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "vec4 SoftDiffuse( vec3 Neye, vec3 Peye ) \n"
        "{ \n"
            "vec3 Leye = ( lightPos - Peye ) / length( lightPos - Peye ); \n"

            "float NdotL = dot( Neye, Leye ) * 0.5 + 0.5; \n"

            "return vec4( NdotL ); \n"
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "vec4 Specular( vec3 NNeye, vec3 Peye ) \n"
        "{ \n"
            "vec3 Leye = normalize( lightPos - Peye ); \n"
            "vec3 Veye = -( normalize( Peye ) ); \n"
            "vec3 Heye = normalize( Leye + Veye ); \n"

            "float NdotH = clamp( dot( NNeye, Heye ), 0.0, 1.0 ); \n"

            "return vec4( pow( NdotH, 64.0 ) ); \n"     
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "void main() \n"
        "{ \n"
            "float noiseAmplitude = 1.0; \n"
            "float sharpness = 50.0; \n"
            "vec3 PP = gl_TexCoord[ 0 ].xyz + noiseAmplitude * Noisy( gl_TexCoord[ 0 ].xyz ); \n"
            "float veinFrequency = 0.2; \n"
            "PP *= veinFrequency; \n"

            //Calculate the veining function for the lookup area
            "float turb, turbsum = 0.0; \n"
            "float freq = 1.0; \n"

            "for( int i = 0; i < 2; ++i ) \n"
            "{ \n"
                "turb = abs( Snoise( PP ).x ); \n"
                "turb = pow( smoothstep( 0.8, 1.0, 1.0 - turb ), sharpness ) / freq; \n"
                "turbsum += ( 1.0 - turbsum ) * turb; \n"
                "freq *= 3.0; \n"
                "PP *= 3.0; \n"
            "} \n"

            //Blend between the two colors
            "vec4 baseColor = vec4( 1.0, 1.0, 0.842105, 1.0 ); \n"
            "vec4 veinColor = vec4( 0.0, 0.0, 0.0, 1.0 ); \n"
            "vec4 Ct = mix( baseColor, veinColor, turbsum ); \n"

            "vec4 Ka = vec4( 0.00000, 0.00000, 0.00000, 1.0 ); \n"
            "vec4 Kd = vec4( 0.83333, 0.83333, 0.83333, 1.0 ); \n"
            "vec4 Ks = vec4( 0.19333, 0.19333, 0.19333, 1.0 ); \n"

            "gl_FragColor = ( Ct * ( Ka * Ambient() + Kd * SoftDiffuse( normal, eyePos ) ) +  \n"
            "                        Ks * Specular( normalize( normal ), eyePos ) ); \n"
        "} \n";
        ////////////////////////////////////////////////////////////////////////////////


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

    osg::ref_ptr< osg::Uniform > noise = new osg::Uniform( "noise", 0 );
    stateset->addUniform( noise.get() );
        
    GetNode()->GetNode()->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
void FunnelEntity::SetShaderTwo()
{
    char vertexPass[]=
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "gl_TexCoord[ 0 ].xyz = gl_Vertex.xyz * 0.5; \n"
            "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = gl_NormalMatrix * gl_Normal; \n"
        "} \n";

    char fragmentPass[]=
        "uniform sampler3D noise; \n"

        "varying vec3  lightPos; \n"
        "varying vec3  eyePos; \n"
        "varying vec3  normal; \n"

        ////////////////////////////////////////////////////////////////////////////////
        "vec3 Snoise( vec3 x ) \n"
        "{ \n"
            "return 2.0 * texture3D( noise, x ).xyz - 1.0; \n"
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "vec3 Noisy( vec3 x ) \n"
        "{ \n"
            "return texture3D( noise, x ).xyz; \n"
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "vec4 Ambient() \n"
        "{ \n"
            "return vec4( 0.51, 0.51, 0.51, 1.0 ); \n"
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "vec4 SoftDiffuse( vec3 Neye, vec3 Peye ) \n"
        "{ \n"
            "vec3 Leye = ( lightPos - Peye ) / length( lightPos - Peye ); \n"

            "float NdotL = dot( Neye, Leye ) * 0.5 + 0.5; \n"

            "return vec4( NdotL ); \n"
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "vec4 Specular( vec3 NNeye, vec3 Peye ) \n"
        "{ \n"
            "vec3 Leye = normalize( lightPos - Peye ); \n"
            "vec3 Veye = -( normalize( Peye ) ); \n"
            "vec3 Heye = normalize( Leye + Veye ); \n"

            "float NdotH = clamp( dot( NNeye, Heye ), 0.0, 1.0 ); \n"

            "return vec4( pow( NdotH, 64.0 ) ); \n"     
        "} \n"
        ////////////////////////////////////////////////////////////////////////////////
        "void main() \n"
        "{ \n"
            "float noiseAmplitude = 1.0; \n"
            "float sharpness = 50.0; \n"
            "vec3 PP = gl_TexCoord[ 0 ].xyz + noiseAmplitude * Noisy( gl_TexCoord[ 0 ].xyz ); \n"
            "float veinFrequency = 0.15; \n"
            "PP *= veinFrequency; \n"

            //Calculate the veining function for the lookup area
            "float turb, turbsum = 0.0; \n"
            "float freq = 1.0; \n"

            "for( int i = 0; i < 2; ++i ) \n"
            "{ \n"
                "turb = abs( Snoise( PP ).x ); \n"
                "turb = pow( smoothstep( 0.8, 1.0, 1.0 - turb ), sharpness ) / freq; \n"
                "turbsum += ( 1.0 - turbsum ) * turb; \n"
                "freq *= 3.0; \n"
                "PP *= 3.0; \n"
            "} \n"

            //Blend between the two colors
            "vec4 baseColor = vec4( 0.1, 0.1, 0.1, 1.0 ); \n"
            "vec4 veinColor = vec4( 0.95, 0.92, 0.84, 1.0 ); \n"
            "vec4 Ct = mix( baseColor, veinColor, turbsum ); \n"

            "vec4 Ka = vec4( 0.93333, 0.93333, 0.93333, 1.0 ); \n"
            "vec4 Kd = vec4( 0.83333, 0.83333, 0.83333, 1.0 ); \n"
            "vec4 Ks = vec4( 0.79333, 0.79333, 0.79333, 1.0 ); \n"

            "gl_FragColor = ( Ct * ( Ka * Ambient() + Kd * SoftDiffuse( normal, eyePos ) ) +  \n"
            "                        Ks * Specular( normalize( normal ), eyePos ) ); \n"
        "} \n";
        ////////////////////////////////////////////////////////////////////////////////


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

    osg::ref_ptr< osg::Uniform > noise = new osg::Uniform( "noise", 0 );
    stateset->addUniform( noise.get() );
        
    m_nonPhysicsGeometry->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
void FunnelEntity::SetShaderThree()
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
        
    m_nonPhysicsGeometryII->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo
