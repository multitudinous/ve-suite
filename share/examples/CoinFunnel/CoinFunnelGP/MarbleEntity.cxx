// --- My Includes --- //
#include "MarbleEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/Sound.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/CullFace>
#include <osg/BlendFunc>
#include <osg/Depth>
#include <osg/Texture2D>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
MarbleEntity::MarbleEntity( std::string geomFile,
                            ves::xplorer::scenegraph::DCS* pluginDCS,
                            ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
                            , osgAL::SoundManager* soundManager
#endif
                            )
:
CADEntity( geomFile, pluginDCS, false, false, physicsSimulator ),
#ifdef VE_SOUND
m_marbleOnWood( new ves::xplorer::scenegraph::Sound( "MarbleOnWood", GetDCS(), soundManager ) ),
m_marbleOnMetal( new ves::xplorer::scenegraph::Sound( "MarbleOnMetal", GetDCS(), soundManager ) ),
m_marbleOnMarble( new ves::xplorer::scenegraph::Sound( "MarbleOnMarble", GetDCS(), soundManager ) ),
#endif
m_nonPhysicsGeometry( 0 )
{
    m_nonPhysicsGeometry = osgDB::readNodeFile( "Models/IVEs/marble.ive" );
    GetDCS()->addChild( m_nonPhysicsGeometry.get() );
#ifdef VE_SOUND
    try
    {
        m_marbleOnWood->LoadFile( "Sounds/MarbleOnWood.wav" );
        m_marbleOnMetal->LoadFile( "Sounds/MarbleOnMetal.wav" );
        m_marbleOnMarble->LoadFile( "Sounds/MarbleOnMarble.wav" );
    }
    catch( ... )
    {
        std::cerr << "Could not load sounds" << std::endl;
    }
#endif
}
////////////////////////////////////////////////////////////////////////////////
MarbleEntity::~MarbleEntity()
{
#ifdef VE_SOUND
    delete m_marbleOnWood;
    delete m_marbleOnMetal;
    delete m_marbleOnMarble;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void MarbleEntity::SetNameAndDescriptions( std::string geomFile )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( geomFile );
}
////////////////////////////////////////////////////////////////////////////////
void MarbleEntity::SetShaders( osg::TextureCubeMap* tcm )
{
    SetShaderOne( tcm );
    SetShaderTwo();
}
////////////////////////////////////////////////////////////////////////////////
#ifdef VE_SOUND
ves::xplorer::scenegraph::Sound* MarbleEntity::GetMarbleOnWoodSound()
{
    return m_marbleOnWood;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Sound* MarbleEntity::GetMarbleOnMetalSound()
{
    return m_marbleOnMetal;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Sound* MarbleEntity::GetMarbleOnMarbleSound()
{
    return m_marbleOnMarble;
}
#endif
////////////////////////////////////////////////////////////////////////////////
void MarbleEntity::SetShaderOne( osg::TextureCubeMap* tcm )
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
            "float reflectionScale = 0.4; \n"
            "float refractionScale = 0.4; \n"
            "float indexOfRefractionRatio = 1.14; \n"
            "vec4 baseColor = vec4( 0.2, 0.6, 0.2, 1.0 ); \n"

            "vec3 N = normalize( normal ); \n"
            "vec3 L = normalize( lightPos ); \n"
            "vec3 V = normalize( eyePos ); \n"

            "vec3 R = reflect( V, N ); \n"
            "float RDotL = max( dot( R, L ), 0.0 ); \n"
            "vec4 reflection = textureCube( Environment, R ); \n"
            
            "float cosine = dot( V, N ); \n"
            "float sine = sqrt( 1.0 - cosine * cosine ); \n"

            "float sine2 = clamp( indexOfRefractionRatio * sine, 0.0, 1.0 ); \n"
            "float cosine2 = sqrt( 1.0 - sine2 * sine2 ); \n"

            "vec3 x = N; \n"
            "vec3 y = normalize( cross( cross( V, N ), N ) ); \n"

            "vec3 refrVec = x * cosine2 + y * sine2; \n"
            "vec4 refraction = textureCube( Environment, refrVec.xzy ); \n"

            "float v =  dot( normalize( V ), N ); \n"
            "vec4 rainbow = texture2D( Rainbow, vec2( v, 0.0 ) ); \n"

            "vec4 rain = rainbowScale * rainbow * baseColor; \n"
            "vec4 refl = reflectionScale * reflection * baseColor; \n"
            "vec4 refr = refractionScale * refraction * baseColor; \n"
            "vec4 TotalSpecular = gl_LightSource[ 0 ].specular * vec4( 0.8, 0.8, 0.8, 1.0 ) * pow( RDotL, 20.0 ); \n"

            "gl_FragColor = sine * refl + ( 1.0 - sine2 ) * refr + sine2 * rain + ambient + TotalSpecular; \n"
        "} \n";

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
}
////////////////////////////////////////////////////////////////////////////////
void MarbleEntity::SetShaderTwo()
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

            "vec3 color = vec3( 1.0, 0.0, 0.0 ); \n"

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
        
    m_nonPhysicsGeometry->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo
