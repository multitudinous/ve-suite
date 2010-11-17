/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/xplorer/scenegraph/camera/CameraObject.h>
#include <ves/xplorer/scenegraph/camera/CameraObjectCallback.h>
#include <ves/xplorer/scenegraph/camera/HeadCameraObjectCallback.h>
#include <ves/xplorer/scenegraph/camera/CameraCullVisitorCallback.h>

#include <ves/xplorer/scenegraph/util/CallbackSupport.h>

//#include "DepthOfFieldTechnique.h"
//#include "DepthHelperTechnique.h"
#include <ves/xplorer/scenegraph/technique/ProjectionTechnique.h>

// --- VE-Suite Includes --- //
//#include <ves/xplorer/environment/HeadsUpDisplay.h>

#include <ves/xplorer/scenegraph/Masks.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/CameraImageCaptureCallback.h>
#include <ves/xplorer/scenegraph/RTTCameraImageCaptureCallback.h>

// --- vrJuggler Includes --- //
//#include <gmtl/Xforms.h>

// --- OSG Includes --- //
#include <osg/Camera>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/TexGenNode>
//Needed for FBO GL extensions
#include <osg/FrameBufferObject>

#include <osg/Light>
#include <osg/LightSource>
#include <osg/LightModel>

//#include <osgText/Text>

#include <osgUtil/CullVisitor>
#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

#include <osgDB/WriteFile>
#include <osgDB/ReaderWriter>
#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

// --- STL Includes --- //
#include <iostream>

#include <boost/lexical_cast.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>

//#define VES_USE_MULTISAMPLING

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace camera
{

////////////////////////////////////////////////////////////////////////////////
CameraObject::CameraObject(
    technique::ProjectionTechnique* const projectionTechnique,
    osg::TexGenNode* const texGenNode )
    :
    osg::Group(),
    //osg::PositionAttitudeTransform(),
    m_initialViewMatrix(),
    m_mvpt(),
    m_texGenNode( texGenNode ),
    //mDepthOfFieldTechnique( NULL ),
    //mDepthHelperTechnique( NULL ),
    m_projectionTechnique( projectionTechnique ),
    //mHeadsUpDisplay( NULL ),
    m_camera( NULL ),
    m_dcs( NULL ),
    m_cameraNode( NULL ),
    m_frustumGeode( NULL ),
    m_frustumGeometry( NULL ),
    m_frustumVertices( NULL ),
    //mCameraViewQuadDCS( NULL ),
    //mCameraViewQuadGeode( NULL ),
    //mCameraViewQuadGeometry( NULL ),
    //mCameraViewQuadVertices( NULL ),
    //mDistanceText( NULL ),
    //mDepthHelperQuadDCS( NULL ),
    //mDepthHelperQuadGeode( NULL ),
    //mDepthHelperQuadGeometry( NULL ),
    //mDepthHelperQuadVertices( NULL ),
    //m_light( NULL ),
    m_imageCounter( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CameraObject::CameraObject(
    const CameraObject& cameraObject,
    const osg::CopyOp& copyop )
    :
    osg::Group( cameraObject, copyop ),
    //osg::PositionAttitudeTransform( camera, copyop ),
    m_initialViewMatrix( cameraObject.m_initialViewMatrix ),
    m_mvpt( cameraObject.m_mvpt ),
    m_texGenNode( cameraObject.m_texGenNode.get() ),
    //mDepthOfFieldTechnique( camera.mDepthOfFieldTechnique ),
    //mDepthHelperTechnique( camera.mDepthHelperTechnique ),
    m_projectionTechnique( cameraObject.m_projectionTechnique ),
    //mHeadsUpDisplay( camera.mHeadsUpDisplay ),
    m_camera( cameraObject.m_camera.get() ),
    m_dcs( cameraObject.m_dcs.get() ),
    m_cameraNode( cameraObject.m_cameraNode.get() ),
    m_frustumGeode( cameraObject.m_frustumGeode.get() ),
    m_frustumGeometry( cameraObject.m_frustumGeometry.get() ),
    m_frustumVertices( cameraObject.m_frustumVertices.get() )//,
    //mCameraViewQuadDCS( camera.mCameraViewQuadDCS.get() ),
    //mCameraViewQuadGeode( camera.mCameraViewQuadGeode.get() ),
    //mCameraViewQuadGeometry( camera.mCameraViewQuadGeometry.get() ),
    //mCameraViewQuadVertices( camera.mCameraViewQuadVertices.get() ),
    //mDistanceText( camera.mDistanceText.get() ),
    //mDepthHelperQuadDCS( camera.mDepthHelperQuadDCS.get() ),
    //mDepthHelperQuadGeode( camera.mDepthHelperQuadGeode.get() ),
    //mDepthHelperQuadGeometry( camera.mDepthHelperQuadGeometry.get() ),
    //mDepthHelperQuadVertices( camera.mDepthHelperQuadVertices.get() )
{
    if( &cameraObject != this )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
CameraObject::~CameraObject()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::ComputeNearFarPlanes( bool const& enable )
{
    if( !m_camera.valid() )
    {
        return;
    }

    if( enable )
    {
        m_camera->setComputeNearFarMode(
            osgUtil::CullVisitor::COMPUTE_NEAR_FAR_USING_BOUNDING_VOLUMES );
        m_screenCapCamera->setComputeNearFarMode(
            osgUtil::CullVisitor::COMPUTE_NEAR_FAR_USING_BOUNDING_VOLUMES );
    }
    else
    {
        m_camera->setComputeNearFarMode(
            osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
        m_screenCapCamera->setComputeNearFarMode(
            osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::InitializeCamera( osg::Camera& camera )
{
    camera.setName( "Camera Object Camera" );
    camera.setReferenceFrame( osg::Camera::ABSOLUTE_RF );
    camera.setRenderOrder( osg::Camera::PRE_RENDER, 0 );
    camera.setRenderTargetImplementation(
        osg::Camera::FRAME_BUFFER_OBJECT, osg::Camera::FRAME_BUFFER );
    camera.setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    //Keep visibility within user defined near and far planes
    camera.setComputeNearFarMode(
        osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
    //Don't set transparency to zero because of image save as .png
    //If we do post processing on this camera we can get rid of that requirement
    camera.setClearColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    m_texWidth = 1280;
    m_texHeight = 720;
    camera.setViewport( 0, 0, m_texWidth, m_texHeight );
    
    std::pair< int, int > textureRes = 
    std::make_pair< int, int >( m_texWidth, m_texHeight );
    if( !m_colorMap.valid() )
    {
        m_colorMap = CreateViewportTexture(
           GL_RGBA8, GL_BGRA, GL_UNSIGNED_BYTE,
           osg::Texture2D::LINEAR, osg::Texture2D::CLAMP_TO_EDGE,
           textureRes );
    }
    
    //Attach a texture and use it as the render target
    int maxSamples( 0 );
#ifdef VES_USE_MULTISAMPLING
    glGetIntegerv( GL_MAX_SAMPLES_EXT, &maxSamples );
    if( maxSamples > 4 )
    {
        maxSamples = 4;
    }
#endif
    
    camera.attach( osg::Camera::COLOR_BUFFER0, m_colorMap.get(),
        0, 0, false, maxSamples, maxSamples );
    
    m_initialViewMatrix.makeLookAt(
                                   osg::Vec3d( 0.0, 0.0, 0.0 ),
                                   osg::Vec3d( 0.0, 1.0, 0.0 ),
                                   osg::Vec3d( 0.0, 0.0, 1.0 ) );
    camera.setViewMatrix( m_initialViewMatrix );
    camera.setProjectionMatrixAsPerspective( 40.0, 1.0, 0.1, 5.0 );
    
    //Add the subgraph to render
    camera.addChild( &SceneManager::instance()->GetGraphicalPluginManager() );
    camera.addChild( &SceneManager::instance()->GetHighlightManager() );
    camera.addChild( &SceneManager::instance()->GetDeviceHandlerGroup() );
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::Initialize()
{
    //Create osg camera for rendering
    m_camera = new osg::Camera();
    InitializeCamera( *(m_camera.get()) );
    //Add the subgraph to render
    addChild( m_camera.get() );

    //Do this for easy image capture capability
    m_screenCapCamera = new osg::Camera();
    InitializeCamera( *(m_screenCapCamera.get()) );

    m_colorImage = new osg::Image();
    m_colorImage->
        allocateImage( m_texWidth, m_texHeight, 1, GL_RGB, GL_UNSIGNED_BYTE );
    //m_colorMap->setImage( m_colorImage.get() );
    m_screenCapCamera->
        attach( osg::Camera::COLOR_BUFFER0, m_colorImage.get(), 0, 0 );

    /*m_light = new osg::Light();
    m_light->setLightNum( 0 );
    m_light->setAmbient( osg::Vec4( 0.36862, 0.36842, 0.36842, 1.0 ) );
    m_light->setDiffuse( osg::Vec4( 0.88627, 0.88500, 0.88500, 1.0 ) );
    m_light->setSpecular( osg::Vec4( 0.49019, 0.48872, 0.48872, 1.0 ) );
    //We are in openGL space
    m_light->setPosition( osg::Vec4( 0.0, 10000.0, 10000.0, 0.0 ) );

    osg::ref_ptr< osg::LightSource > lightSource = new osg::LightSource();
    lightSource->setLight( m_light.get() );
    lightSource->setReferenceFrame( osg::LightSource::ABSOLUTE_RF );

    osg::ref_ptr< osg::LightModel > lightModel = new osg::LightModel();
    lightModel->setAmbientIntensity( osg::Vec4( 0.1, 0.1, 0.1, 1.0 ) );
    //Get correct specular lighting across pipes
    //See http://www.ds.arch.tue.nl/General/Staff/Joran/osg/osg_specular_problem.htm
    lightModel->setLocalViewer( true );
     */
    //Setup the light
    //osg::ref_ptr< osg::StateSet > stateset = m_camera->getOrCreateStateSet();
    //stateset->setAssociatedModes(
        //m_light.get(),
        //osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    //stateset->setMode(
        //GL_LIGHTING,
        //osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    //stateset->setAttributeAndModes(
        //lightModel.get(),
        //osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    //m_camera->addChild( lightSource.get() );

    //Create DCS
    m_dcs = new DCS();
    addChild( m_dcs.get() );

    //Initialize update callback
    setUpdateCallback( new CameraObjectCallback() );

    //Initialize m_mvpt
    m_mvpt = osg::Matrix::identity();

    //Initialize m_texGenNode
    //m_texGenNode = new osg::TexGenNode();
    //m_texGenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
    //m_texGenNode->setTextureUnit( 0 );
    //addChild( m_texGenNode.get() );

    //Create visual representation of this camera
    CreateGeometry();

    //Initialize mHitQuadGeode
    //CreateHitQuadGeode();

    //Initialize mCameraViewQuadDCS
    //CreateCameraViewQuad();

    //Initialize mDepthHelperQuadDCS
    //CreateDepthHelperQuad();

    //Initialize mDepthOfFieldTechnique
    //mDepthOfFieldTechnique = new cpt::DepthOfFieldTechnique();
    //mDepthOfFieldTechnique->GetTextureDimensionsUniform()->set(
        //static_cast< int >( 1024 ), static_cast< int >( 1024 ) );
    //mDepthOfFieldTechnique->GetMaxCircleOfConfusionUniform()->set(
        //static_cast< float >( 6.0 ) );
    //mCameraViewQuadDCS->AddTechnique( "DepthOfField", mDepthOfFieldTechnique );
    //mCameraViewQuadDCS->SetTechnique( "DepthOfField" );

    //mDepthHelperTechnique = new cpt::DepthHelperTechnique();
    //mDepthHelperQuadDCS->AddTechnique( "DepthHelper", mDepthHelperTechnique );
    //mDepthHelperQuadDCS->SetTechnique( "DepthHelper" );

    //Initialize mProjectionTechnique
    //mProjectionTechnique = new cpt::ProjectionTechnique();
    //mProjectionTechnique->GetAlpha()->set(
        //static_cast< float >( 0.3 ) );
    //mProjectionTechnique->GetNearPlaneUniform()->set(
        //static_cast< float >( 0.1 ) );
    //mProjectionTechnique->GetFarPlaneUniform()->set(
        //static_cast< float >( 2.0 ) );
    //mProjectionTechnique->GetFocalDistanceUniform()->set(
        //static_cast< float >( 1.5 ) );
    //mProjectionTechnique->GetFocalRangeUniform()->set(
        //static_cast< float >( 5.0 ) );
    //mPluginDCS->AddTechnique( "Projection", mProjectionTechnique );
    //mPluginDCS->SetTechnique( "Projection" );

    SetNamesAndDescriptions();

    Update();
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::CalculateMatrixMVPT()
{
    //Compute the matrix which takes a vertex from local coords into tex coords
    //Multiply the ModelView(MV) by the Projection(P) by the Texture(T) matrix
    m_mvpt = m_camera->getViewMatrix() *
             m_camera->getProjectionMatrix() *
             osg::Matrixd::translate( 1.0, 1.0, 1.0 ) *
             osg::Matrixd::scale( 0.5, 0.5, 0.5 );

    m_texGenNode->getTexGen()->setPlanesFromMatrix( m_mvpt );
    
    //Now update the screen capture camera since this happens after the 
    //update callback
    m_screenCapCamera->setViewMatrix( m_camera->getViewMatrix() );
    m_screenCapCamera->setProjectionMatrix( m_camera->getProjectionMatrix() );
}
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::CustomKeyboardMouseSelection(
    std::pair< unsigned int, unsigned int > mousePosition,
    gmtl::Matrix44d localToWorldMatrix )
{
    std::pair< unsigned int, unsigned int > windowResolution =
        mHeadsUpDisplay->GetWindowResolution();
    mousePosition.second = windowResolution.second - mousePosition.second;

    std::pair< unsigned int, unsigned int > quadResolution( 0, 0 );
    quadResolution.first = (*mCameraViewQuadVertices)[ 2 ].x() *
                           mCameraViewQuadDCS->getScale().x();
    quadResolution.second = (*mCameraViewQuadVertices)[ 2 ].y() *
                            mCameraViewQuadDCS->getScale().y();

    if( mousePosition.first <= quadResolution.first &&
        mousePosition.second <= quadResolution.second )
    {
        std::pair< double, double > frustumQuadRatio( 0, 0 );
        frustumQuadRatio.first =
            ( (*m_frustumVertices)[ 3 ].x() - (*m_frustumVertices)[ 4 ].x() ) /
            static_cast< double >( quadResolution.first );
        frustumQuadRatio.second =
            ( (*m_frustumVertices)[ 3 ].z() - (*m_frustumVertices)[ 2 ].z() ) /
            static_cast< double >( quadResolution.second );

        osg::Vec3d cameraLensPoint( 0, 0, 0 );
        osg::Vec3d startPoint( 0, 0, 0 );
        osg::Vec3d endPoint( 0, 0, 0 );

        startPoint.x() = (*m_frustumVertices)[ 4 ].x() +
                         ( mousePosition.first * frustumQuadRatio.first );
        startPoint.y() = (*m_frustumVertices)[ 3 ].y();
        startPoint.z() = (*m_frustumVertices)[ 2 ].z() +
                         ( mousePosition.second * frustumQuadRatio.second );

        double yRatio = (*m_frustumVertices)[ 7 ].y() /
                        (*m_frustumVertices)[ 3 ].y();

        endPoint.x() = yRatio * startPoint.x();
        endPoint.y() = (*m_frustumVertices)[ 7 ].y();
        endPoint.z() = yRatio * startPoint.z();

        osg::Matrixd tempMatrix;
        tempMatrix.set( localToWorldMatrix.getData() );
        startPoint = startPoint * tempMatrix;
        endPoint = endPoint * tempMatrix;
        cameraLensPoint = cameraLensPoint * tempMatrix;

        osg::ref_ptr< osgUtil::LineSegmentIntersector > intersector =
            new osgUtil::LineSegmentIntersector( startPoint, endPoint );
        osgUtil::IntersectionVisitor intersectionVisitor( intersector.get() );

        mPluginDCS->accept( intersectionVisitor );

        if( !intersector->containsIntersections() )
        {
            mDistanceText->setText( "" );

            return;
        }

        osg::Vec3d intersectPoint =
            intersector->getFirstIntersection().getWorldIntersectPoint();

        //Calculate the distance hit point is from the camera lens
        osg::Vec3d hitToCameraVector = intersectPoint - cameraLensPoint;
        double hitDistanceFromCamera = hitToCameraVector.length();

        //Get the center vector of the frustum in world coordinates
        osg::Vec3d centerVector( 0, 1, 0 );
        centerVector = centerVector * tempMatrix;
        centerVector = centerVector - cameraLensPoint;

        //Project hitToCameraVector onto centerVector
        //This will give y length w.r.t the center of the frustum
        osg::Vec3d centerFrustumProjection( 0, 0, 0 );
        centerFrustumProjection = centerVector *
            ( ( hitToCameraVector * centerVector ) / centerVector.length2() );

        double yCenterLength = centerFrustumProjection.length();
        yRatio = yCenterLength / (*m_frustumVertices)[ 3 ].y();

        double x = yRatio * (*m_frustumVertices)[ 3 ].x();
        double z = yRatio * (*m_frustumVertices)[ 3 ].z();

        //(*mHitQuadVertices)[ 0 ].set( -x, yCenterLength, -z );
        //(*mHitQuadVertices)[ 1 ].set(  x, yCenterLength, -z );
        //(*mHitQuadVertices)[ 2 ].set(  x, yCenterLength,  z );
        //(*mHitQuadVertices)[ 3 ].set( -x, yCenterLength,  z );
        //mHitQuadGeometry->computeFastPathsUsed();
        //mHitQuadGeometry->dirtyDisplayList();
        //mHitQuadGeometry->dirtyBound();

        std::stringstream ss;
        ss << "Distance: ";
        ss << yCenterLength;
        mDistanceText->setText( ss.str() );
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
void CameraObject::CreateGeometry()
{
    /*
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    //Set bin number to 11 so camera does not occlude geometry from scene
    stateset->setRenderBinDetails( 11, "RenderBin" );
    stateset->setAttribute(
        ( mResourceManager->get
        < osg::Program, osg::ref_ptr >( "CameraProgram" ) ).get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    mCameraDCS->setStateSet( stateset.get() );

    mCameraDCS->addChild( m_cameraNode.get() );
    */

    //Add the geometric model for the camera
    m_cameraNode = osgDB::readNodeFile( "osg-data/camera.ive" );
    {
        std::string vertexSource =
        "varying vec4 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "eyePos = gl_ModelViewMatrix * gl_Vertex; \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
        "} \n";

        std::string fragmentSource =
        "varying vec4 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"
        "uniform vec3 glowColor; \n"

        "const vec3 ambMat  = vec3( 0.368627, 0.368421, 0.368421 ); \n"
        "const vec3 diffMat = vec3( 0.886275, 0.885003, 0.885003 ); \n"
        "const vec3 specMat = vec3( 0.490196, 0.488722, 0.488722 ); \n"

        "void main() \n"
        "{ \n"
            "vec3 N = normalize( normal ); \n"
            "vec3 L = normalize( lightPos ); \n"
            "float NDotL = max( dot( N, L ), 0.0 ); \n"

            "vec3 V = normalize( eyePos.xyz ); \n"
            "vec3 R = reflect( V, N ); \n"
            "float RDotL = max( dot( R, L ), 0.0 ); \n"

            "vec3 totalAmbient = gl_LightSource[ 0 ].ambient.rgb * ambMat; \n"
            "vec3 totalDiffuse = gl_LightSource[ 0 ].diffuse.rgb * diffMat * NDotL; \n"
            "vec3 totalSpecular = \n"
                "gl_LightSource[ 0 ].specular.rgb * specMat * pow( RDotL, 15.0 ); \n"

            "gl_FragData[ 0 ] = vec4( totalAmbient + totalDiffuse + totalSpecular, 1.0 ); \n"
            "gl_FragData[ 1 ] = vec4( glowColor, gl_FragData[ 0 ].a ); \n"
        "} \n";

        osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
        vertexShader->setType( osg::Shader::VERTEX );
        vertexShader->setShaderSource( vertexSource );
        vertexShader->setName( "Camera Vertex Shader" );

        osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
        fragmentShader->setType( osg::Shader::FRAGMENT );
        fragmentShader->setShaderSource( fragmentSource );
        fragmentShader->setName( "Camera Fragment Shader" );

        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( vertexShader.get() );
        program->addShader( fragmentShader.get() );
        program->setName( "Camera Program" );

        osg::ref_ptr< osg::StateSet > stateset =
            m_cameraNode->getOrCreateStateSet();
        stateset->setAttributeAndModes(
            program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }
    m_dcs->addChild( m_cameraNode.get() );

    //Create the geometric lines for the frustum
    m_frustumGeode = new osg::Geode();
    m_frustumGeometry = new osg::Geometry();
    m_frustumVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > frustumColors = new osg::Vec4Array();

    //Only need 9 vertices:
    //The origin, and the eight corners of the near and far planes.
    m_frustumVertices->resize( 9 );
    m_frustumGeometry->setVertexArray( m_frustumVertices.get() );

    frustumColors->push_back( osg::Vec4( 0.33, 0.87, 0.56, 1.0 ) );
    m_frustumGeometry->setColorArray( frustumColors.get() );
    m_frustumGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    GLushort idxLines[ 8 ]  = { 0, 5, 0, 6, 0, 7, 0, 8 };
    GLushort idxLoops0[ 4 ] = { 1, 2, 3, 4 };
    GLushort idxLoops1[ 4 ] = { 5, 6, 7, 8 };
    m_frustumGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINES, 8, idxLines ) );
    m_frustumGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINE_LOOP, 4, idxLoops0 ) );
    m_frustumGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINE_LOOP, 4, idxLoops1 ) );
    m_frustumGeode->addDrawable( m_frustumGeometry.get() );

    {
        osg::ref_ptr< osg::StateSet > stateset =
            m_frustumGeode->getOrCreateStateSet();
        //stateset->setRenderBinDetails( 0, "RenderBin" );
        stateset->setMode(
            GL_LIGHTING,
            osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
        //stateset->setAttribute(
            //( mResourceManager->get
            //< osg::Program, osg::ref_ptr >( "FrustumProgram" ) ).get(),
            //osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }
    m_dcs->addChild( m_frustumGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::CreateHitQuadGeode()
{
    mHitQuadGeode = new osg::Geode();
    mHitQuadGeometry = new osg::Geometry();
    mHitQuadVertices = new osg::Vec3Array();

    //Only need 4 vertices for a quad:
    mHitQuadVertices->resize( 4 );
    mHitQuadGeometry->setVertexArray( mHitQuadVertices.get() );

    mHitQuadGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );
    mHitQuadGeode->addDrawable( mHitQuadGeometry.get() );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setRenderBinDetails( 10, "DepthSortedBin" );
    stateset->setAttribute(
        ( mResourceManager->get
        < osg::Program, osg::ref_ptr >( "HitQuadProgram" ) ).get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    osg::ref_ptr< osg::Uniform > baseMapUniform =
        new osg::Uniform( "baseMap", 1 );
    stateset->addUniform( baseMapUniform.get() );
    mHitQuadGeode->setStateSet( stateset.get() );

    mCameraDCS->addChild( mHitQuadGeode.get() );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::CreateCameraViewQuad()
{
    mCameraViewQuadDCS = new ves::xplorer::scenegraph::DCS();
    mCameraViewQuadDCS->setScale( osg::Vec3( 300, 300, 1 ) );
    mHeadsUpDisplay->GetCamera()->addChild( mCameraViewQuadDCS.get() );

    mCameraViewQuadGeode = new osg::Geode();
    mCameraViewQuadGeometry = new osg::Geometry();
    mCameraViewQuadVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();

    //Only need 4 vertices for a quad:
    mCameraViewQuadVertices->resize( 4 );
    mCameraViewQuadGeometry->setVertexArray( mCameraViewQuadVertices.get() );

    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0, 0 );
    (*quadTexCoords)[ 1 ].set( 1, 0 );
    (*quadTexCoords)[ 2 ].set( 1, 1 );
    (*quadTexCoords)[ 3 ].set( 0, 1 );
    mCameraViewQuadGeometry->setTexCoordArray( 0, quadTexCoords.get() );

    mCameraViewQuadGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );

    mCameraViewQuadGeode->addDrawable( mCameraViewQuadGeometry.get() );

    mDistanceText = new osgText::Text();
    std::string textFont( "fonts/arial.ttf" );
    mDistanceText->setFont( textFont );
    mDistanceText->setCharacterSize( 20 );
    mDistanceText->setAxisAlignment( osgText::Text::SCREEN );
    mDistanceText->setAlignment( osgText::Text::LEFT_TOP );
    mDistanceText->setPosition( osg::Vec3(  0, 1, 0 ) );
    mDistanceText->setColor( osg::Vec4( 1, 1, 1, 1 ) );
    mDistanceText->setText( "" );
    mCameraViewQuadGeode->addDrawable( mDistanceText.get() );

    mCameraViewQuadDCS->addChild( mCameraViewQuadGeode.get() );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::CreateDepthHelperQuad()
{
    mDepthHelperQuadDCS = new ves::xplorer::scenegraph::DCS();
    mDepthHelperQuadDCS->setScale( osg::Vec3( 200, 200, 1 ) );
    mDepthHelperQuadDCS->setPosition(
        osg::Vec3( 0, mCameraViewQuadDCS->getScale().y(), 0 ) );
    mHeadsUpDisplay->GetCamera()->addChild( mDepthHelperQuadDCS.get() );

    mDepthHelperQuadGeode = new osg::Geode();
    mDepthHelperQuadGeometry = new osg::Geometry();
    mDepthHelperQuadVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();

    //Only need 4 vertices for a quad:
    mDepthHelperQuadVertices->resize( 4 );
    mDepthHelperQuadGeometry->setVertexArray( mDepthHelperQuadVertices.get() );

    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0, 0 );
    (*quadTexCoords)[ 1 ].set( 1, 0 );
    (*quadTexCoords)[ 2 ].set( 1, 1 );
    (*quadTexCoords)[ 3 ].set( 0, 1 );
    mDepthHelperQuadGeometry->setTexCoordArray( 0, quadTexCoords.get() );

    mDepthHelperQuadGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );

    mDepthHelperQuadGeode->addDrawable( mDepthHelperQuadGeometry.get() );

    mDepthHelperQuadDCS->addChild( mDepthHelperQuadGeode.get() );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::DisplayProjectionEffect( bool onOff )
{
    if( onOff )
    {
        mPluginDCS->SetTechnique( "Projection" );
    }
    else
    {
        mPluginDCS->SetTechnique( "Default" );
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::DisplayDepthOfFieldEffect( bool onOff )
{
    if( onOff )
    {
        mCameraViewQuadDCS->SetTechnique( "DepthOfField" );
    }
    else
    {
        mCameraViewQuadDCS->SetTechnique( "Default" );
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::DisplayCameraViewQuad( bool onOff )
{
    mCameraViewQuadDCS->setNodeMask( onOff );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::DisplayDepthHelperQuad( bool onOff )
{
    mDepthHelperQuadDCS->setNodeMask( onOff );
}
*/
////////////////////////////////////////////////////////////////////////////////
void CameraObject::EnableCamera( bool const& enable )
{
    if( !m_camera.valid() )
    {
        return;
    }

    if( enable )
    {
        m_camera->setNodeMask( NodeMask::CAMERA );
    }
    else
    {
        m_camera->setNodeMask( 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera& CameraObject::GetCamera()
{
    return *m_camera.get();
}
////////////////////////////////////////////////////////////////////////////////
DCS& CameraObject::GetDCS()
{
    return *m_dcs.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& CameraObject::GetInitialViewMatrix()
{
    return m_initialViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
/*osg::Light& CameraObject::GetLight() const
{
    return *m_light.get();
}*/
////////////////////////////////////////////////////////////////////////////////
/*
osg::TexGenNode* CameraObject::GetTexGenNode()
{
    return m_texGenNode.get();
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::setAttitude( const osg::Quat& quat )
{
    osg::PositionAttitudeTransform::setAttitude( quat );

    osg::Matrixd& viewMatrix = m_camera->getViewMatrix();
}
*/
////////////////////////////////////////////////////////////////////////////////
void CameraObject::SetNamesAndDescriptions()
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );

    m_dcs->setDescriptions( descriptorsList );
    m_dcs->setName( "CameraDCS" );
}
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::SetProjectionEffectOpacity( double value )
{
    mProjectionTechnique->GetAlpha()->set(
        static_cast< float >( value ) );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::SetFocalDistance( double value )
{
    mProjectionTechnique->GetFocalDistanceUniform()->set(
        static_cast< float >( value ) );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::SetFocalRange( double value )
{
    mProjectionTechnique->GetFocalRangeUniform()->set(
        static_cast< float >( value ) );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::SetMaxCircleOfConfusion( double value )
{
    mDepthOfFieldTechnique->GetMaxCircleOfConfusionUniform()->set(
        static_cast< float >( value ) );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::SetCameraViewQuadResolution( unsigned int value )
{
    mCameraViewQuadDCS->setScale( osg::Vec3( value, value, 1 ) );

    mDepthHelperQuadDCS->setPosition( osg::Vec3( 0, value, 0 ) );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::SetDepthHelperQuadResolution( unsigned int value )
{
    mDepthHelperQuadDCS->setScale( osg::Vec3( value, value, 1 ) );
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void CameraObject::setPosition( const osg::Vec3d& pos )
{
    osg::PositionAttitudeTransform::setPosition( pos );

    osg::Matrixd& viewMatrix = m_camera->getViewMatrix();
    viewMatrix.preMultTranslate( pos );
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::setScale( const osg::Vec3d& scale )
{
    osg::PositionAttitudeTransform::setScale( scale );

    osg::Matrixd& viewMatrix = m_camera->getViewMatrix();
}
*/
////////////////////////////////////////////////////////////////////////////////
void CameraObject::ShowCameraGeometry( bool const& show )
{
    m_cameraNode->setNodeMask( show );
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::ShowFrustumGeometry( bool const& show )
{
    m_frustumGeode->setNodeMask( show );
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::SetPictureFrameProjection( bool const& show )
{
    m_projectionTechnique->SetPictureFrame( show );
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::Update()
{
    //Update the MVPT matrix
    CalculateMatrixMVPT();

    //Update the frustum geode
    osg::Matrixd projectionMatrix = m_camera->getProjectionMatrix();

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

    (*m_frustumVertices)[ 0 ].set( 0.0, 0.0, 0.0 );
    (*m_frustumVertices)[ 1 ].set( nLeft, nearPlane, nBottom );
    (*m_frustumVertices)[ 2 ].set( nRight, nearPlane, nBottom );
    (*m_frustumVertices)[ 3 ].set( nRight, nearPlane, nTop );
    (*m_frustumVertices)[ 4 ].set( nLeft, nearPlane, nTop );
    (*m_frustumVertices)[ 5 ].set( fLeft, farPlane, fBottom );
    (*m_frustumVertices)[ 6 ].set( fRight, farPlane, fBottom );
    (*m_frustumVertices)[ 7 ].set( fRight, farPlane, fTop );
    (*m_frustumVertices)[ 8 ].set( fLeft, farPlane, fTop );
    m_frustumGeometry->dirtyDisplayList();
    m_frustumGeometry->dirtyBound();

    //const double aspectRatio = fabs( fRight - fLeft ) / fabs( fTop - fBottom );

    /*
    (*mCameraViewQuadVertices)[ 0 ].set( 0.0,         0.0, -1.0 );
    (*mCameraViewQuadVertices)[ 1 ].set( aspectRatio, 0.0, -1.0 );
    (*mCameraViewQuadVertices)[ 2 ].set( aspectRatio, 1.0, -1.0 );
    (*mCameraViewQuadVertices)[ 3 ].set( 0.0,         1.0, -1.0 );
    mCameraViewQuadGeometry->dirtyDisplayList();
    mCameraViewQuadGeometry->dirtyBound();

    (*mDepthHelperQuadVertices)[ 0 ].set( 0.0,         0.0, -1.0 );
    (*mDepthHelperQuadVertices)[ 1 ].set( aspectRatio, 0.0, -1.0 );
    (*mDepthHelperQuadVertices)[ 2 ].set( aspectRatio, 1.0, -1.0 );
    (*mDepthHelperQuadVertices)[ 3 ].set( 0.0,         1.0, -1.0 );
    mDepthHelperQuadGeometry->dirtyDisplayList();
    mDepthHelperQuadGeometry->dirtyBound();
    */
    //Update the uniforms
    m_projectionTechnique->SetNearPlane( static_cast< float >( nearPlane ) );
    m_projectionTechnique->SetFarPlane( static_cast< float >( farPlane ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::SetRenderQuadTexture( osg::Geode& geode )
{
    geode.getStateSet()->setTextureAttributeAndModes(
        0, m_colorMap.get(), osg::StateAttribute::ON );
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture2D* CameraObject::CreateViewportTexture(
    GLenum internalFormat,
    GLenum sourceFormat,
    GLenum sourceType,
    osg::Texture2D::FilterMode filterMode,
    osg::Texture2D::WrapMode wrapMode,
    std::pair< int, int >& viewportDimensions )
{
    //GL_RGBA8/GL_UNSIGNED_INT - GL_RGBA16F_ARB/GL_FLOAT
    osg::Texture2D* tempTexture = new osg::Texture2D();
    tempTexture->setInternalFormat( internalFormat );
    tempTexture->setSourceFormat( sourceFormat );
    tempTexture->setSourceType( sourceType );
    tempTexture->setFilter( osg::Texture2D::MIN_FILTER, filterMode );
    tempTexture->setFilter( osg::Texture2D::MAG_FILTER, filterMode );
    tempTexture->setWrap( osg::Texture2D::WRAP_S, wrapMode );
    tempTexture->setWrap( osg::Texture2D::WRAP_T, wrapMode );
    tempTexture->setTextureSize(
        viewportDimensions.first, viewportDimensions.second );

    return tempTexture;
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::WriteImageFile( std::string const& saveImageDir )
{
    std::string imageNumber = 
        boost::lexical_cast< std::string >( m_imageCounter );
    const std::string baseFilename = saveImageDir + "/" + getName() + "_";
    const std::string extension = ".png";
    m_filename = baseFilename + imageNumber + extension;

    while( boost::filesystem::exists( m_filename ) )
    {
        imageNumber = boost::lexical_cast< std::string >( ++m_imageCounter );

        m_filename = baseFilename + imageNumber + extension;
    }
    
    removeChild( m_camera.get() );
    addChild( m_screenCapCamera.get() );
    /*osg::ref_ptr< ves::xplorer::scenegraph::RTTCameraImageCaptureCallback > 
        tempCB = 
        new ves::xplorer::scenegraph::RTTCameraImageCaptureCallback( m_filename, m_colorMap.get() );
    
    osg::Camera::DrawCallback* pdCB = m_camera->getPostDrawCallback();
    if( pdCB )
    {
        osg::ref_ptr< osgwTools::CompositeDrawCallback > cdc = new osgwTools::CompositeDrawCallback();
        cdc->getDrawCallbackList().push_back( pdCB );
        cdc->getDrawCallbackList().push_back( tempCB.get() );
        m_camera->setPostDrawCallback( cdc.get() );
    }
    else
    {
        m_camera->setPostDrawCallback( tempCB.get() );
    }*/

    m_captureImage = true;
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::MakeHeadTrackedCamera()
{
    setUpdateCallback( new HeadCameraObjectCallback() );
    ShowCameraGeometry( false );
    ShowFrustumGeometry( false );
    ComputeNearFarPlanes( true );
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::PostWriteImageFile()
{
    osgDB::writeImageFile( *(m_colorImage.get()), m_filename );
    m_captureImage = false;
    removeChild( m_screenCapCamera.get() );
    addChild( m_camera.get() );
    /*osg::Camera::DrawCallback* pdCB = m_camera->getPostDrawCallback();
    osgwTools::CompositeDrawCallback* cdc = dynamic_cast< osgwTools::CompositeDrawCallback* >( pdCB );
    
    if( cdc )
    {
        pdCB = cdc->getDrawCallbackList().at( 0 );
        m_camera->setPostDrawCallback( pdCB );
    }
    else
    {
        m_camera->setPostDrawCallback( 0 );
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::SetTextureResolution( std::pair< unsigned int, unsigned int >& viewportDimensions )
{
    m_texWidth = viewportDimensions.first;
    m_texHeight = viewportDimensions.second;
    m_colorMap->setTextureSize( m_texWidth, m_texHeight );
    m_colorMap->dirty();
    //m_colorImage->allocateImage( m_texWidth, m_texHeight, 1, GL_RGB, GL_UNSIGNED_BYTE );
    m_colorImage->scaleImage( m_texWidth, m_texHeight, 1 );
    m_colorImage->dirty();
}
////////////////////////////////////////////////////////////////////////////////
} //end camera
} //end scenegraph
} //end xplorer
} //end ves
