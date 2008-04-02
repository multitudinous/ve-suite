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
#include "CameraEntity.h"
#include "CameraEntityCallback.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/ResourceManager.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/TexGenNode>

#include <osgDB/ReadFile>

//C/C++ Libraries
#include <iostream>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity()
:
osg::Camera(),
mCameraPerspective( false ),
mCameraEntityCallback( 0 ),
mQuadTexture( 0 ),
mTexGenNode( 0 ),
mNearPlaneUniform( 0 ),
mFarPlaneUniform( 0 ),
mCameraDCS( 0 ),
mQuadDCS( 0 ),
mCameraNode( 0 ),
mFrustumGeode( 0 ),
mFrustumGeometry( 0 ),
mFrustumVertices( 0 ),
mQuadGeode( 0 ),
mQuadGeometry( 0 ),
mQuadVertices( 0 ),
mSceneManager( 0 )
{
    //Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity(
    ves::xplorer::scenegraph::SceneManager* sceneManager )
:
osg::Camera(),
mCameraPerspective( false ),
mCameraEntityCallback( 0 ),
mQuadTexture( 0 ),
mTexGenNode( 0 ),
mNearPlaneUniform( 0 ),
mFarPlaneUniform( 0 ),
mCameraDCS( 0 ),
mQuadDCS( 0 ),
mCameraNode( 0 ),
mFrustumGeode( 0 ),
mFrustumGeometry( 0 ),
mFrustumVertices( 0 ),
mQuadGeode( 0 ),
mQuadGeometry( 0 ),
mQuadVertices( 0 ),
mSceneManager( sceneManager )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity( const CameraEntity& cameraEntity,
                            const osg::CopyOp& copyop )
:
osg::Camera( cameraEntity, copyop ),
mCameraEntityCallback( 0 ),
mQuadTexture( 0 ),
mTexGenNode( 0 ),
mNearPlaneUniform( 0 ),
mFarPlaneUniform( 0 ),
mCameraDCS( 0 ),
mQuadDCS( 0 ),
mCameraNode( 0 ),
mFrustumGeode( 0 ),
mFrustumGeometry( 0 ),
mFrustumVertices( 0 ),
mQuadGeode( 0 ),
mQuadGeometry( 0 ),
mQuadVertices( 0 )
{
    if( &cameraEntity != this )
    {
        mCameraPerspective = cameraEntity.mCameraPerspective;
    }
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::~CameraEntity()
{
    mSceneManager->GetRootNode()->removeChild( this );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::Initialize()
{   
    osg::ref_ptr< osg::Group > rootNode =
        mSceneManager->GetRootNode();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > worldDCS =
        mSceneManager->GetWorldDCS();
    
    //Initialize the resources
    InitializeResources();

    //Initialize this
    setRenderOrder( osg::Camera::PRE_RENDER );
    setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    setClearColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    setComputeNearFarMode( osg::Camera::DO_NOT_COMPUTE_NEAR_FAR );
    setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );
    setReferenceFrame( osg::Camera::ABSOLUTE_RF );

    mInitialViewMatrix.makeLookAt( osg::Vec3( 0, 0, 0 ),
                                   osg::Vec3( 0, 1, 0 ),
                                   osg::Vec3( 0, 0, 1 ) );
    setViewMatrix( mInitialViewMatrix );
    setProjectionMatrixAsPerspective( 30.0, 1.0, 5.0, 10.0 );

    rootNode->addChild( this );

    //Initialize mCameraEntityCallback
    mCameraEntityCallback = new cpt::CameraEntityCallback();
    setUpdateCallback( mCameraEntityCallback.get() );

    //Initialize mMVPT
    mMVPT = osg::Matrix::identity();

    //Initialize mQuadTexture
    CreateCameraViewTexture();

    //Initialize mTexGenNode
    mTexGenNode = new osg::TexGenNode();
    mTexGenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
    mTexGenNode->setTextureUnit( 0 );
    rootNode->addChild( mTexGenNode.get() );

    //Initialize mNearPlaneUniform, mFarPlaneUniform
    mNearPlaneUniform = new osg::Uniform(
        "nearPlane", static_cast< float >( 5.0 ) );
    mFarPlaneUniform = new osg::Uniform(
        "farPlane", static_cast< float >( 10.0 ) );

    //Initialize mCameraDCS & mQuadDCS
    mCameraDCS = new ves::xplorer::scenegraph::DCS();
    worldDCS->addChild( mCameraDCS.get() );

    mQuadDCS = new ves::xplorer::scenegraph::DCS();
    worldDCS->addChild( mQuadDCS.get() );

    SetNamesAndDescriptions();

    //Initialize mCameraNode
    mCameraNode = osgDB::readNodeFile( std::string( "Models/camera.ive" ) );
    mCameraDCS->addChild( mCameraNode.get() );
    
    //Initialize mFrustumGeode
    CreateViewFrustumGeode();

    //Initialize mQuadGeode
    CreateScreenAlignedQuadGeode();






        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
    stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );
    stateset->setAttribute(
        ( ves::xplorer::scenegraph::ResourceManager::instance()->get
        < osg::Program, osg::ref_ptr >( "ProjectionProgram" ) ).get(),
        osg::StateAttribute::ON );

    stateset->addUniform( mNearPlaneUniform.get() );
    stateset->addUniform( mFarPlaneUniform.get() );



    //worldDCS->setStateSet( stateset.get() );

    //Add the subgraph to render
    addChild( worldDCS.get() );








    Update();
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::InitializeResources()
{
    std::string mQuadVertexSource = std::string(
        "varying vec4 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "eyePos = gl_ModelViewMatrix * gl_Vertex; \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

            "gl_FrontColor = gl_Color; \n"

            "gl_TexCoord[ 0 ].s = dot( eyePos, gl_EyePlaneS[ 0 ] ); \n"
            "gl_TexCoord[ 0 ].t = dot( eyePos, gl_EyePlaneT[ 0 ] ); \n"
            "gl_TexCoord[ 0 ].q = dot( eyePos, gl_EyePlaneQ[ 0 ] ); \n"
        "} \n" );

    std::string mQuadFragmentSource = std::string(
        "uniform float nearPlane; \n"
        "uniform float farPlane; \n"

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
                                "gl_Color.rgb; \n"
            "vec3 totalDiffuse = gl_LightSource[ 0 ].diffuse.rgb * \n"
                                "gl_Color.rgb * NDotL; \n"
            "vec3 totalSpecular = gl_LightSource[ 0 ].specular.rgb * \n"
                                 "gl_Color.rgb * pow( RDotL, 15.0 ); \n"

            "vec2 projectionUV = gl_TexCoord[ 0 ].st / gl_TexCoord[ 0 ].q; \n"
            "vec4 color = \n"
                "vec4( totalAmbient + totalDiffuse + totalSpecular, 0.3 ); \n"

            //If in frustum
            "if( projectionUV.s >= 0.0 && \n"
                "projectionUV.t >= 0.0 && \n"
                "projectionUV.s <= 1.0 && \n"
                "projectionUV.t <= 1.0 && \n"
                "gl_TexCoord[ 0 ].q >= nearPlane && \n"
                "gl_TexCoord[ 0 ].q <= farPlane ) \n"
            "{ \n"
                "color.a = 1.0; \n"
            "} \n"

            "gl_FragColor = color; \n"
        "} \n" );

    std::string mProjectionVertexSource = std::string(
        "varying vec4 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "eyePos = gl_ModelViewMatrix * gl_Vertex; \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

            "gl_FrontColor = gl_Color; \n"

            "gl_TexCoord[ 0 ].s = dot( eyePos, gl_EyePlaneS[ 0 ] ); \n"
            "gl_TexCoord[ 0 ].t = dot( eyePos, gl_EyePlaneT[ 0 ] ); \n"
            "gl_TexCoord[ 0 ].q = dot( eyePos, gl_EyePlaneQ[ 0 ] ); \n"
        "} \n" );

    std::string mProjectionFragmentSource = std::string(
        "uniform float nearPlane; \n"
        "uniform float farPlane; \n"

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
                                "gl_Color.rgb; \n"
            "vec3 totalDiffuse = gl_LightSource[ 0 ].diffuse.rgb * \n"
                                "gl_Color.rgb * NDotL; \n"
            "vec3 totalSpecular = gl_LightSource[ 0 ].specular.rgb * \n"
                                 "gl_Color.rgb * pow( RDotL, 15.0 ); \n"

            "vec2 projectionUV = gl_TexCoord[ 0 ].st / gl_TexCoord[ 0 ].q; \n"
            "vec4 color = \n"
                "vec4( totalAmbient + totalDiffuse + totalSpecular, 0.3 ); \n"

            //If in frustum
            "if( projectionUV.s >= 0.0 && \n"
                "projectionUV.t >= 0.0 && \n"
                "projectionUV.s <= 1.0 && \n"
                "projectionUV.t <= 1.0 && \n"
                "gl_TexCoord[ 0 ].q >= nearPlane && \n"
                "gl_TexCoord[ 0 ].q <= farPlane ) \n"
            "{ \n"
                "color.a = 1.0; \n"
            "} \n"

            "gl_FragColor = color; \n"
        "} \n" );

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
ves::xplorer::scenegraph::SceneManager* CameraEntity::GetSceneManager()
{
    return mSceneManager;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CalculateMatrixMVPT()
{
    //Compute the matrix which takes a vertex from local coords into tex coords
    //Multiply the ModelView(MV) by the Projection(P) by the Texture(T) matrix
    mMVPT = getViewMatrix() *
            getProjectionMatrix() *
            osg::Matrix::translate( 1.0f, 1.0f, 1.0f ) *
            osg::Matrix::scale( 0.5f, 0.5f, 0.5f );

    mTexGenNode->getTexGen()->setPlanesFromMatrix( mMVPT );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CreateViewFrustumGeode()
{
    mFrustumGeode = new osg::Geode();
    mFrustumGeometry = new osg::Geometry();
    mFrustumVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > frustumColors = new osg::Vec4Array();

    //Only need 9 vertices:
    //The origin, and the eight corners of the near and far planes.
    mFrustumVertices->resize( 9 );
    mFrustumGeometry->setVertexArray( mFrustumVertices.get() );

    frustumColors->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    mFrustumGeometry->setColorArray( frustumColors.get() );
    mFrustumGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    GLushort idxLines[ 8 ]  = { 0, 5, 0, 6, 0, 7, 0, 8 };
    GLushort idxLoops0[ 4 ] = { 1, 2, 3, 4 };
    GLushort idxLoops1[ 4 ] = { 5, 6, 7, 8 };
    mFrustumGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINES, 8, idxLines ) );
    mFrustumGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINE_LOOP, 4, idxLoops0 ) );
    mFrustumGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINE_LOOP, 4, idxLoops1 ) );
    mFrustumGeode->addDrawable( mFrustumGeometry.get() );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    mFrustumGeode->setStateSet( stateset.get() );

    mCameraDCS->addChild( mFrustumGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CreateScreenAlignedQuadGeode()
{
    mQuadGeode = new osg::Geode();
    mQuadGeometry = new osg::Geometry();
    mQuadVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();

    //Only need 4 vertices for a quad:
    mQuadVertices->resize( 4 );
    mQuadGeometry->setVertexArray( mQuadVertices.get() );

    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0, 0 );
    (*quadTexCoords)[ 1 ].set( 1, 0 );
    (*quadTexCoords)[ 2 ].set( 1, 1 );
    (*quadTexCoords)[ 3 ].set( 0, 1 );
    mQuadGeometry->setTexCoordArray( 1, quadTexCoords.get() );

    mQuadGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );
    mQuadGeode->addDrawable( mQuadGeometry.get() );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setTextureAttributeAndModes( 1, mQuadTexture.get(), osg::StateAttribute::ON );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    mQuadGeode->setStateSet( stateset.get() );

    mQuadDCS->addChild( mQuadGeode.get() );

    DisplayScreenAlignedQuad( false );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CreateCameraViewTexture()
{
    mQuadTexture = new osg::Texture2D();

    //Create the texture
    mQuadTexture->setTextureSize( 1024, 1024 );
    mQuadTexture->setInternalFormat( GL_RGBA );
    mQuadTexture->setSourceFormat( GL_RGBA );
    mQuadTexture->setSourceType( GL_UNSIGNED_BYTE );

    mQuadTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    mQuadTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    mQuadTexture->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    mQuadTexture->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );

    setViewport( 0, 0, 1024, 1024 );

    //Attach the texture and use it as the color buffer
    attach( osg::Camera::COLOR_BUFFER, mQuadTexture.get() );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::SetNamesAndDescriptions()
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );

    mCameraDCS->setDescriptions( descriptorsList );
    mCameraDCS->setName( std::string( "Camera" ) );

    mQuadDCS->setDescriptions( descriptorsList );
    mQuadDCS->setName( std::string( "Screen Aligned Quad" ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::Update()
{
    //Update the MVPT matrix
    CalculateMatrixMVPT();

    //Update the frustum geode
    osg::Matrixd projectionMatrix = getProjectionMatrix();

    const double nearPlane = projectionMatrix( 3, 2 ) /
                           ( projectionMatrix( 2, 2 ) - 1.0 );
    const double farPlane = projectionMatrix( 3, 2 ) /
                          ( projectionMatrix( 2, 2 ) + 1.0 );

    const double nLeft =   nearPlane * ( projectionMatrix( 2, 0 ) - 1.0 ) /
                                         projectionMatrix( 0, 0 );
    const double nRight =  nearPlane * ( projectionMatrix( 2, 0 ) + 1.0 ) /
                                         projectionMatrix( 0, 0 );
    const double nTop =    nearPlane * ( projectionMatrix( 2, 1 ) + 1.0 ) /
                                         projectionMatrix( 1, 1 );
    const double nBottom = nearPlane * ( projectionMatrix( 2, 1 ) - 1.0 ) /
                                         projectionMatrix( 1, 1 );

    const double fLeft =   farPlane * ( projectionMatrix( 2, 0 ) - 1.0 ) /
                                        projectionMatrix( 0, 0 );
    const double fRight =  farPlane * ( projectionMatrix( 2, 0 ) + 1.0 ) /
                                        projectionMatrix( 0, 0 );
    const double fTop =    farPlane * ( projectionMatrix( 2, 1 ) + 1.0 ) /
                                        projectionMatrix( 1, 1 );
    const double fBottom = farPlane * ( projectionMatrix( 2, 1 ) - 1.0 ) /
                                        projectionMatrix( 1, 1 );

    (*mFrustumVertices)[ 0 ].set( 0.0, 0.0, 0.0 );
    (*mFrustumVertices)[ 1 ].set( nLeft, nearPlane, nBottom );
    (*mFrustumVertices)[ 2 ].set( nRight, nearPlane, nBottom );
    (*mFrustumVertices)[ 3 ].set( nRight, nearPlane, nTop );
    (*mFrustumVertices)[ 4 ].set( nLeft, nearPlane, nTop );
    (*mFrustumVertices)[ 5 ].set( fLeft, farPlane, fBottom );
    (*mFrustumVertices)[ 6 ].set( fRight, farPlane, fBottom );
    (*mFrustumVertices)[ 7 ].set( fRight, farPlane, fTop );
    (*mFrustumVertices)[ 8 ].set( fLeft, farPlane, fTop );
    
    mFrustumGeometry->dirtyDisplayList();
    mFrustumGeometry->dirtyBound();

    (*mQuadVertices)[ 0 ].set( -0.5, 5.0, -0.5 );
    (*mQuadVertices)[ 1 ].set(  0.5, 5.0, -0.5 );
    (*mQuadVertices)[ 2 ].set(  0.5, 5.0,  0.5 );
    (*mQuadVertices)[ 3 ].set( -0.5, 5.0,  0.5 );

    mQuadGeometry->dirtyDisplayList();
    mQuadGeometry->dirtyBound();

    //Update the uniforms
    mNearPlaneUniform->set( static_cast< float >( nearPlane ) );
    mFarPlaneUniform->set( static_cast< float >( farPlane ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::DisplayCamera( bool onOff )
{
    mCameraNode->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::DisplayViewFrustum( bool onOff )
{
    mFrustumGeode->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::DisplayScreenAlignedQuad( bool onOff )
{
    mQuadDCS->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* CameraEntity::GetDCS()
{
    return mCameraDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& CameraEntity::GetInitialViewMatrix()
{
    return mInitialViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
