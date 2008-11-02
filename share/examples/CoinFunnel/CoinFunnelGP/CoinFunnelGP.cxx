/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- My Includes --- //
#include "CoinFunnelGP.h"
#include "CoinFunnelWorld.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#include <ves/open/xml/model/Model.h>

// --- osgAL Includes --- //
#ifdef VE_SOUND
#include <osgAL/SoundRoot>
#include <osgAL/SoundManager>
#include <osgAL/SoundNode>
#include <osgAL/SoundState>
#endif

// --- OSG Includes --- //
#include <osg/Texture2D>
#include <osg/Texture3D>
#include <osg/TextureCubeMap>

using namespace funnel;

////////////////////////////////////////////////////////////////////////////////
CoinFunnelGP::CoinFunnelGP()
:
PluginBase()
{
    mObjectName = "CoinFunnelUI";
}
////////////////////////////////////////////////////////////////////////////////
CoinFunnelGP::~CoinFunnelGP()
{
    if( !mResourceManager )
    {
        return;
    }
    mResourceManager->remove( "NoiseVolume" );

    mResourceManager->remove( "CubeMap" );

    mResourceManager->remove( "Rainbow" );

    mResourceManager->remove( "RoomProgram" );

    mResourceManager->remove( "FunnelProgram" );

    mResourceManager->remove( "MarbleProgram" );

    mResourceManager->remove( "CatsEyeProgram" );

    mResourceManager->remove( "RailingProgram" );

    mResourceManager->remove( "SlideProgram" );

    mResourceManager->remove( "WaterProgram" );
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    InitializeResources();

    mCoinFunnelWorld = funnel::CoinFunnelWorldPtr(
        new funnel::CoinFunnelWorld( mDCS.get(),
                                     mPhysicsSimulator,
                                     mResourceManager
#ifdef VE_SOUND
                                     ,
                                     mSoundManager
#endif
                                     ) );
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::PreFrameUpdate()
{
    mCoinFunnelWorld->PreFrameUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::InitializeResources()
{
    //Texture Unit 0
    osg::ref_ptr< osg::Texture3D > noiseVolumeTexture = new osg::Texture3D();
    noiseVolumeTexture->setFilter(
        osg::Texture3D::MIN_FILTER, osg::Texture3D::LINEAR );
    noiseVolumeTexture->setFilter(
        osg::Texture3D::MAG_FILTER, osg::Texture3D::LINEAR );
    noiseVolumeTexture->setWrap(
        osg::Texture3D::WRAP_S, osg::Texture3D::REPEAT );
    noiseVolumeTexture->setWrap(
        osg::Texture3D::WRAP_T, osg::Texture3D::REPEAT );
    noiseVolumeTexture->setWrap(
        osg::Texture3D::WRAP_R, osg::Texture3D::REPEAT );
    noiseVolumeTexture->setImage(
        osgDB::readImageFile( "Textures/NoiseVolume.dds" ) );

    boost::any noiseVal = noiseVolumeTexture;
    mResourceManager->add(
        std::string( "NoiseVolume" ), noiseVal );

    //Texture Unit 1
    osg::ref_ptr< osg::TextureCubeMap > textureCubeMap =
        new osg::TextureCubeMap();
    textureCubeMap->setWrap(
        osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    textureCubeMap->setWrap(
        osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );
    textureCubeMap->setWrap(
        osg::Texture::WRAP_R, osg::Texture::CLAMP_TO_EDGE );
    textureCubeMap->setFilter(
        osg::Texture::MIN_FILTER, osg::Texture::LINEAR );

    textureCubeMap->setImage(
        osg::TextureCubeMap::POSITIVE_X,
        osgDB::readImageFile(
            "Textures/CubeMaps/NvidiaLobby/nvlobby_new_positive_x.tga" ) );
    textureCubeMap->setImage(
        osg::TextureCubeMap::NEGATIVE_X,
        osgDB::readImageFile(
            "Textures/CubeMaps/NvidiaLobby/nvlobby_new_negative_x.tga" ) );
    textureCubeMap->setImage(
        osg::TextureCubeMap::POSITIVE_Y,
        osgDB::readImageFile(
            "Textures/CubeMaps/NvidiaLobby/nvlobby_new_positive_y.tga" ) );
    textureCubeMap->setImage(
        osg::TextureCubeMap::NEGATIVE_Y,
        osgDB::readImageFile(
            "Textures/CubeMaps/NvidiaLobby/nvlobby_new_negative_y.tga" ) );
    textureCubeMap->setImage(
        osg::TextureCubeMap::POSITIVE_Z,
        osgDB::readImageFile(
            "Textures/CubeMaps/NvidiaLobby/nvlobby_new_positive_z.tga" ) );
    textureCubeMap->setImage(
        osg::TextureCubeMap::NEGATIVE_Z,
        osgDB::readImageFile(
            "Textures/CubeMaps/NvidiaLobby/nvlobby_new_negative_z.tga" ) );

    boost::any cubeMapVal = textureCubeMap;
    mResourceManager->add(
        std::string( "CubeMap" ), cubeMapVal );

    //Texture Unit 2
    osg::ref_ptr< osg::Texture2D > rainbowTexture = new osg::Texture2D();
    rainbowTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    rainbowTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    rainbowTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    rainbowTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
    rainbowTexture->setImage(
        osgDB::readImageFile( "Textures/Rainbow.tga" ) );

    boost::any rainbowVal = rainbowTexture;
    mResourceManager->add(
        std::string( "Rainbow" ), rainbowVal );


    //Initialize room program
    std::string roomVertexSource = std::string(
    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "gl_TexCoord[ 0 ].xyz = gl_Vertex.xyz; \n"
    "} \n" );

    std::string roomFragmentSource = std::string(
	"uniform samplerCube envMap; \n"

	"void main()  \n"
	"{  \n"
	    "gl_FragColor = textureCube( envMap, gl_TexCoord[ 0 ].xyz ); \n"
	"} \n" );

    osg::ref_ptr< osg::Shader > roomVertexShader = new osg::Shader();
    roomVertexShader->setType( osg::Shader::VERTEX );
    roomVertexShader->setShaderSource( roomVertexSource );

    osg::ref_ptr< osg::Shader > roomFragmentShader = new osg::Shader();
    roomFragmentShader->setType( osg::Shader::FRAGMENT );
    roomFragmentShader->setShaderSource( roomFragmentSource );

    osg::ref_ptr< osg::Program > roomProgram = new osg::Program();
    roomProgram->addShader( roomVertexShader.get() );
    roomProgram->addShader( roomFragmentShader.get() );
    boost::any roomVal = roomProgram;
    mResourceManager->add(
        std::string( "RoomProgram" ), roomVal );

    //Initialize funnel program
    std::string funnelVertexSource = std::string(
    "uniform float scale; \n"

    "varying vec3 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "gl_TexCoord[ 0 ].xyz = gl_Vertex.xyz * scale; \n"
        "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
        "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
        "normal = gl_NormalMatrix * gl_Normal; \n"
    "} \n" );

    std::string funnelFragmentSource = std::string(
    "uniform sampler3D noise; \n"

    "uniform vec4 baseColor; \n"
    "uniform vec4 veinColor; \n"

    "varying vec3  lightPos; \n"
    "varying vec3  eyePos; \n"
    "varying vec3  normal; \n"
    
    "vec3 Snoise( vec3 x ) \n"
    "{ \n"
        "return 2.0 * texture3D( noise, x ).xyz - 1.0; \n"
    "} \n"
    
    "vec3 Noisy( vec3 x ) \n"
    "{ \n"
        "return texture3D( noise, x ).xyz; \n"
    "} \n"
    
    "vec4 Ambient() \n"
    "{ \n"
        "return vec4( 0.51, 0.51, 0.51, 1.0 ); \n"
    "} \n"
    
    "vec4 SoftDiffuse( vec3 Neye, vec3 Peye ) \n"
    "{ \n"
        "vec3 Leye = ( lightPos - Peye ) / length( lightPos - Peye ); \n"

        "float NdotL = dot( Neye, Leye ) * 0.5 + 0.5; \n"

        "return vec4( NdotL ); \n"
    "} \n"
    
    "vec4 Specular( vec3 NNeye, vec3 Peye ) \n"
    "{ \n"
        "vec3 Leye = normalize( lightPos - Peye ); \n"
        "vec3 Veye = -( normalize( Peye ) ); \n"
        "vec3 Heye = normalize( Leye + Veye ); \n"

        "float NdotH = clamp( dot( NNeye, Heye ), 0.0, 1.0 ); \n"

        "return vec4( pow( NdotH, 64.0 ) ); \n"
    "} \n"
    
    "void main() \n"
    "{ \n"
        "float noiseAmplitude = 1.0; \n"
        "float sharpness = 50.0; \n"
        "vec3 PP = gl_TexCoord[ 0 ].xyz + noiseAmplitude * Noisy( gl_TexCoord[ 0 ].xyz ); \n"
        "float veinFrequency = 0.2; \n"
        "PP *= veinFrequency; \n"

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

        "vec4 Ct = mix( baseColor, veinColor, turbsum ); \n"

        "vec4 Ka = vec4( 0.00000, 0.00000, 0.00000, 1.0 ); \n"
        "vec4 Kd = vec4( 0.83333, 0.83333, 0.83333, 1.0 ); \n"
        "vec4 Ks = vec4( 0.19333, 0.19333, 0.19333, 1.0 ); \n"

        "gl_FragColor = ( Ct * ( Ka * Ambient() + Kd * SoftDiffuse( normal, eyePos ) ) +  \n"
        "                        Ks * Specular( normalize( normal ), eyePos ) ); \n"
    "} \n" );
    
    
    osg::ref_ptr< osg::Shader > funnelVertexShader = new osg::Shader();
    funnelVertexShader->setType( osg::Shader::VERTEX );
    funnelVertexShader->setShaderSource( funnelVertexSource );

    osg::ref_ptr< osg::Shader > funnelFragmentShader = new osg::Shader();
    funnelFragmentShader->setType( osg::Shader::FRAGMENT );
    funnelFragmentShader->setShaderSource( funnelFragmentSource );

    osg::ref_ptr< osg::Program > funnelProgram = new osg::Program();
    funnelProgram->addShader( funnelVertexShader.get() );
    funnelProgram->addShader( funnelFragmentShader.get() );
    boost::any funnelVal = funnelProgram;
    mResourceManager->add(
        std::string( "FunnelProgram" ), funnelVal );

    //Initialize marble program
    std::string marbleVertexSource = std::string(
    "varying vec3 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
        "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
        "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
    "} \n" );

    std::string marbleFragmentSource = std::string(
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
    "} \n" );

    osg::ref_ptr< osg::Shader > marbleVertexShader = new osg::Shader();
    marbleVertexShader->setType( osg::Shader::VERTEX );
    marbleVertexShader->setShaderSource( marbleVertexSource );

    osg::ref_ptr< osg::Shader > marbleFragmentShader = new osg::Shader();
    marbleFragmentShader->setType( osg::Shader::FRAGMENT );
    marbleFragmentShader->setShaderSource( marbleFragmentSource );

    osg::ref_ptr< osg::Program > marbleProgram = new osg::Program();
    marbleProgram->addShader( marbleVertexShader.get() );
    marbleProgram->addShader( marbleFragmentShader.get() );
    boost::any marbleVal = marbleProgram;
    mResourceManager->add(
        std::string( "MarbleProgram" ), marbleVal );

    //Initialize cats eye program
    std::string catsEyeVertexSource = std::string(
    "varying vec3 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
        "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
        "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
    "} \n" );

    std::string catsEyeFragmentSource = std::string(
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
    "} \n" );

    osg::ref_ptr< osg::Shader > catsEyeVertexShader = new osg::Shader();
    catsEyeVertexShader->setType( osg::Shader::VERTEX );
    catsEyeVertexShader->setShaderSource( catsEyeVertexSource );

    osg::ref_ptr< osg::Shader > catsEyeFragmentShader = new osg::Shader();
    catsEyeFragmentShader->setType( osg::Shader::FRAGMENT );
    catsEyeFragmentShader->setShaderSource( catsEyeFragmentSource );

    osg::ref_ptr< osg::Program > catsEyeProgram = new osg::Program();
    catsEyeProgram->addShader( catsEyeVertexShader.get() );
    catsEyeProgram->addShader( catsEyeFragmentShader.get() );
    boost::any catsEyeVal = catsEyeProgram;
    mResourceManager->add(
        std::string( "CatsEyeProgram" ), catsEyeVal );

    //Initialize railing program
    std::string railingVertexSource = std::string(
    "varying vec3 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
        "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
        "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
    "} \n" );

    std::string railingFragmentSource = std::string(
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

        "vec3 color = vec3( 0.7, 0.7, 0.7 ); \n"
        "vec3 reflectedColor = textureCube( Environment, R ).rgb; \n"

        "vec3 mixColor = mix( color, reflectedColor, 0.15 ); \n"

        "vec3 TotalAmbient  = gl_LightSource[ 0 ].ambient.rgb  * mixColor; \n"
        "vec3 TotalDiffuse  = gl_LightSource[ 0 ].diffuse.rgb  * mixColor * NDotL; \n"
        "vec3 TotalSpecular = gl_LightSource[ 0 ].specular.rgb * mixColor * pow( RDotL, 15.0 ); \n"

        "gl_FragColor = vec4( TotalAmbient + TotalDiffuse + TotalSpecular, 1.0 ); \n"
    "} \n" );

    osg::ref_ptr< osg::Shader > railingVertexShader = new osg::Shader();
    railingVertexShader->setType( osg::Shader::VERTEX );
    railingVertexShader->setShaderSource( railingVertexSource );

    osg::ref_ptr< osg::Shader > railingFragmentShader = new osg::Shader();
    railingFragmentShader->setType( osg::Shader::FRAGMENT );
    railingFragmentShader->setShaderSource( railingFragmentSource );

    osg::ref_ptr< osg::Program > railingEyeProgram = new osg::Program();
    railingEyeProgram->addShader( railingVertexShader.get() );
    railingEyeProgram->addShader( railingFragmentShader.get() );
    boost::any railingVal = railingEyeProgram;
    mResourceManager->add(
        std::string( "RailingProgram" ), railingVal );

    //Initialize slide program
    std::string slideVertexSource = std::string(
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
    "} \n" );

    std::string slideFragmentSource = std::string(
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
    "} \n" );

    osg::ref_ptr< osg::Shader > slideVertexShader = new osg::Shader();
    slideVertexShader->setType( osg::Shader::VERTEX );
    slideVertexShader->setShaderSource( slideVertexSource );

    osg::ref_ptr< osg::Shader > slideFragmentShader = new osg::Shader();
    slideFragmentShader->setType( osg::Shader::FRAGMENT );
    slideFragmentShader->setShaderSource( slideFragmentSource );

    osg::ref_ptr< osg::Program > slideProgram = new osg::Program();
    slideProgram->addShader( slideVertexShader.get() );
    slideProgram->addShader( slideFragmentShader.get() );
    boost::any slideVal = slideProgram;
    mResourceManager->add(
        std::string( "SlideProgram" ), slideVal );

    //Initialize water program
    std::string waterVertexSource = std::string(
    "varying vec3 eyePos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "gl_TexCoord[ 0 ].xyz =vec3( gl_ModelViewMatrix * gl_Vertex ) * 0.8; \n"
        "eyePos = gl_TexCoord[ 0 ].xyz; \n"
        "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
    "} \n" );

    std::string waterFragmentSource = std::string(
    "uniform float time; \n"

    "uniform sampler3D noise; \n"
    "uniform samplerCube skyBox; \n"

    "varying vec3 eyePos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "float noiseSpeed = 0.18; \n"
        "float waveSpeed = 0.14; \n"

        "vec3 texCoord = gl_TexCoord[ 0 ].xyz; \n"
        "texCoord.x += waveSpeed * time; \n"
        "texCoord.z += noiseSpeed * time; \n"

        "vec4 noisy = texture3D( noise, texCoord ); \n"

        //Signed noise 
        "vec3 bump = 2.0 * noisy.xyz - 1.0; \n"
        "bump.xy *= 0.15; \n"

        //Make sure the normal always points upwards
        "bump.z = 0.8 * abs( bump.z ) + 0.2; \n"

        //Offset the surface normal with the bump
        "bump = normalize( normal + bump ); \n"

        //Find the reflection vector
        "vec3 V = normalize( eyePos ); \n"
        "vec3 R = reflect( V, bump ); \n"
        "vec4 refl = textureCube( skyBox, R ); \n"

        "float lrp = 1.0 - dot( V, bump ); \n"

        "vec4 waterColor = vec4( 0.0, 0.0, 0.0, 1.0 ); \n"
        "float fadeExp = 30.0; \n"
        "float fadeBias = 0.30; \n"
        "vec4 color = mix( waterColor, refl, clamp( fadeBias + pow( lrp, fadeExp ), 0.0, 1.0 ) ); \n"
        "color.a = 0.4; \n"

        //Interpolate between the water color and reflection
        "gl_FragColor = color; \n"
    "} \n" );

    osg::ref_ptr< osg::Shader > waterVertexShader = new osg::Shader();
    waterVertexShader->setType( osg::Shader::VERTEX );
    waterVertexShader->setShaderSource( waterVertexSource );

    osg::ref_ptr< osg::Shader > waterFragmentShader = new osg::Shader();
    waterFragmentShader->setType( osg::Shader::FRAGMENT );
    waterFragmentShader->setShaderSource( waterFragmentSource );

    osg::ref_ptr< osg::Program > waterProgram = new osg::Program();
    waterProgram->addShader( waterVertexShader.get() );
    waterProgram->addShader( waterFragmentShader.get() );
    boost::any waterVal = waterProgram;
    mResourceManager->add(
        std::string( "WaterProgram" ), waterVal );
}
////////////////////////////////////////////////////////////////////////////////