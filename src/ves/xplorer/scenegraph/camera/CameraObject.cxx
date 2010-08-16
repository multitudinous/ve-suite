/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
//#include "DepthOfFieldTechnique.h"
//#include "DepthHelperTechnique.h"
//#include "ProjectionTechnique.h"

// --- VE-Suite Includes --- //
//#include <ves/xplorer/environment/HeadsUpDisplay.h>

#include <ves/xplorer/scenegraph/Masks.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>

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

// --- STL Includes --- //
#include <iostream>

#define VES_USE_MULTISAMPLING

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace camera
{

////////////////////////////////////////////////////////////////////////////////
CameraObject::CameraObject()
    :
    osg::Group(),
    //osg::PositionAttitudeTransform(),
    m_initialViewMatrix(),
    m_mvpt(),
    m_texGenNode( NULL ),
    //mDepthOfFieldTechnique( NULL ),
    //mDepthHelperTechnique( NULL ),
    //mProjectionTechnique( NULL ),
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
    m_light( NULL )
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
    //mProjectionTechnique( camera.mProjectionTechnique ),
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
    /*
    mHeadsUpDisplay->GetCamera()->removeChild( mCameraViewQuadDCS.get() );
    mHeadsUpDisplay->GetCamera()->removeChild( mDepthHelperQuadDCS.get() );

    mCameraViewQuadDCS->SetTechnique( "Default" );
    mCameraViewQuadDCS->RemoveTechnique( "DepthOfField" );
    delete mDepthOfFieldTechnique;

    mDepthHelperQuadDCS->SetTechnique( "Default" );
    mDepthHelperQuadDCS->RemoveTechnique( "DepthHelper" );
    delete mDepthHelperTechnique;

    mPluginDCS->SetTechnique( "Default" );
    mPluginDCS->RemoveTechnique( "Projection" );
    delete mProjectionTechnique;
    */
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
    }
    else
    {
        m_camera->setComputeNearFarMode(
            osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraObject::Initialize()
{
    //Create osg camera for rendering
    m_camera = new osg::Camera();
    m_camera->setName( "Camera Object Camera" );
    m_camera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
    m_camera->setRenderOrder( osg::Camera::PRE_RENDER, 0 );
    m_camera->setRenderTargetImplementation(
        osg::Camera::FRAME_BUFFER_OBJECT, osg::Camera::FRAME_BUFFER_OBJECT );
    m_camera->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    //Keep visibility within user defined near and far planes
    m_camera->setComputeNearFarMode(
        osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
    m_camera->setClearColor( osg::Vec4( 0.0, 0.0, 0.0, 0.0 ) );
    m_camera->setViewport( 0, 0, 1024, 1024 );

    std::pair< int, int > textureRes = std::make_pair< int, int >( 1024, 1024 );
    m_colorMap = CreateViewportTexture(
        GL_RGBA8, GL_BGRA, GL_UNSIGNED_BYTE,
        osg::Texture2D::LINEAR, osg::Texture2D::CLAMP_TO_EDGE,
        textureRes );

    //Attach a texture and use it as the render target
    int maxSamples( 0 );
#ifdef VES_USE_MULTISAMPLING
    glGetIntegerv( GL_MAX_SAMPLES_EXT, &maxSamples );
    if( maxSamples > 4 )
    {
        maxSamples = 4;
    }
#endif

    m_camera->attach(
        osg::Camera::COLOR_BUFFER0, m_colorMap.get(),
        0, 0, false, 4, 4 );
    //Attach the depth texture and use it as the second render target
    //attach( osg::Camera::COLOR_BUFFER1,
            //( ves::xplorer::scenegraph::ResourceManager::instance()->get
            //< osg::Texture2D, osg::ref_ptr >( "DepthTexture" ) ).get() );

    //Do this for easy image capture capability
    m_colorImage = new osg::Image();
    m_camera->attach( osg::Camera::COLOR_BUFFER0, m_colorImage.get(), 4, 4 );

    m_initialViewMatrix.makeLookAt(
        osg::Vec3d( 0.0, 0.0, 0.0 ),
        osg::Vec3d( 0.0, 1.0, 0.0 ),
        osg::Vec3d( 0.0, 0.0, 1.0 ) );
    m_camera->setViewMatrix( m_initialViewMatrix );
    m_camera->setProjectionMatrixAsPerspective( 40.0, 1.0, 0.1, 5.0 );

    m_light = new osg::Light();
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

    //Setup the light
    osg::ref_ptr< osg::StateSet > stateset = m_camera->getOrCreateStateSet();
    stateset->setAssociatedModes(
        m_light.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes(
        lightModel.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    m_camera->addChild( lightSource.get() );

    //Add the subgraph to render
    m_camera->addChild( &SceneManager::instance()->GetGraphicalPluginManager() );
    addChild( m_camera.get() );

    //Create DCS
    m_dcs = new DCS();
    addChild( m_dcs.get() );

    //Initialize update callback
    setUpdateCallback( new CameraObjectCallback() );

    //Initialize m_mvpt
    m_mvpt = osg::Matrix::identity();

    //Initialize m_texGenNode
    m_texGenNode = new osg::TexGenNode();
    m_texGenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
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

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    //stateset->setRenderBinDetails( 0, "RenderBin" );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    //stateset->setAttribute(
        //( mResourceManager->get
        //< osg::Program, osg::ref_ptr >( "FrustumProgram" ) ).get(),
        //osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    m_frustumGeode->setStateSet( stateset.get() );

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
osg::Light& CameraObject::GetLight() const
{
    return *m_light.get();
}
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

    const double aspectRatio = fabs( fRight - fLeft ) / fabs( fTop - fBottom );

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

    //Update the uniforms
    mProjectionTechnique->GetNearPlaneUniform()->set(
        static_cast< float >( nearPlane ) );
    mProjectionTechnique->GetFarPlaneUniform()->set(
        static_cast< float >( farPlane ) );
    */
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
    std::string filename = saveImageDir + "\\" + getName() + ".png";
    osgDB::writeImageFile( *(m_colorImage.get()), filename );
}
////////////////////////////////////////////////////////////////////////////////

} //end camera
} //end scenegraph
} //end xplorer
} //end ves