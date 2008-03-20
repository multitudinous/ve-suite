// --- My Includes --- //
#include "CameraPlacementToolShaders.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>

// --- OSG Includes --- //

// --- C/C++ Libraries --- //

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolShaders::CameraPlacementToolShaders()
{
    CreateShaderSources();
    CreateShaderPrograms();
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolShaders::~CameraPlacementToolShaders()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolShaders::CreateShaderSources()
{
    mProjectionVertexSource = std::string(
    "varying vec4 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "eyePos = gl_ModelViewMatrix * gl_Vertex; \n"
        "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
        "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

        "gl_TexCoord[ 0 ].s = dot( eyePos, gl_EyePlaneS[ 0 ] ); \n"
        "gl_TexCoord[ 0 ].t = dot( eyePos, gl_EyePlaneT[ 0 ] ); \n"
        "gl_TexCoord[ 0 ].q = dot( eyePos, gl_EyePlaneQ[ 0 ] ); \n"
    "} \n" );

    mProjectionFragmentSource = std::string(
    "uniform float nearPlane; \n"
    "uniform float farPlane; \n"

    //"uniform sampler2D projectionMap; \n"

    "varying vec4 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "vec3 N = normalize( normal ); \n"
        "vec3 L = normalize( lightPos ); \n"
        "float NDotL = max( dot( N, L ), 0.0 ); \n"

        "vec3 V = normalize( eyePos.xyz ); \n"
        "vec3 R = reflect( V, N ); \n"
        "float RDotL = max( dot( R, L ), 0.0 ); \n"

        "vec3 totalAmbient = gl_LightSource[ 0 ].ambient.rgb * \n"
                            "vec3( 0.368627, 0.368421 , 0.368421 ); \n"
        "vec3 totalDiffuse = gl_LightSource[ 0 ].diffuse.rgb * \n"
                            "vec3( 0.886275, 0.885003 , 0.885003 ) * NDotL; \n"
        "vec3 totalSpecular = gl_LightSource[ 0 ].specular.rgb * \n"
                             "vec3( 0.490196, 0.488722 , 0.488722 ) * \n"
                             "pow( RDotL, 15.0 ); \n"

        "vec2 projectionUV = gl_TexCoord[ 0 ].st / gl_TexCoord[ 0 ].q; \n"
        //"vec4 projection = texture2D( projectionMap, projectionUV ); \n"
        "vec4 color = vec4( totalAmbient + totalDiffuse + totalSpecular, 1.0 ); \n"

        //Test that the fragment is in the frustum
        "if( projectionUV.s >= 0.0 && \n"
            "projectionUV.t >= 0.0 && \n"
            "projectionUV.s <= 1.0 && \n"
            "projectionUV.t <= 1.0 && \n"
            "gl_TexCoord[ 0 ].q >= nearPlane && \n"
            "gl_TexCoord[ 0 ].q <= farPlane ) \n"
        "{ \n"
            //If in the frustum
            "gl_FragColor = vec4( 1.0, 1.0, 0.0, 1.0 ); \n"
        "} \n"
        "else \n"
        "{ \n"
            //If not in the frustum
            "gl_FragColor = color; \n"
        "} \n"
    "} \n" );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolShaders::CreateShaderPrograms()
{
    osg::ref_ptr< osg::Shader > projectionVertexShader = new osg::Shader();
    projectionVertexShader->setType( osg::Shader::VERTEX );
    projectionVertexShader->setShaderSource( mProjectionVertexSource );

    osg::ref_ptr< osg::Shader > projectionFragmentShader = new osg::Shader();
    projectionFragmentShader->setType( osg::Shader::FRAGMENT );
    projectionFragmentShader->setShaderSource( mProjectionFragmentSource );

    osg::ref_ptr< osg::Program > projectionProgram = new osg::Program();
    projectionProgram->addShader( projectionVertexShader.get() );
    projectionProgram->addShader( projectionFragmentShader.get() );
    boost::any anyVal = projectionProgram;
    ves::xplorer::scenegraph::ResourceManager::instance()->add(
        std::string( "ProjectionProgram" ), anyVal );
}
////////////////////////////////////////////////////////////////////////////////
