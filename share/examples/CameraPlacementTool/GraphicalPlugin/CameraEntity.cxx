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
#include "ProjectionTechnique.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/environment/HeadsUpDisplay.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/LocalToWorldTransform.h>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/TexGenNode>

#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

#include <osgDB/ReadFile>

//C/C++ Libraries
#include <iostream>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity()
:
osg::Camera(),
mTexGenNode( 0 ),
mProjectionTechnique( 0 ),
mCameraEntityCallback( 0 ),
mHeadsUpDisplay( 0 ),
mResourceManager( 0 ),
mRootNode( 0 ),
mWorldDCS( 0 ),
mPluginDCS( 0 ),
mCameraDCS( 0 ),
mCameraNode( 0 ),
mFrustumGeode( 0 ),
mFrustumGeometry( 0 ),
mFrustumVertices( 0 ),
mCameraViewQuadDCS( 0 ),
mCameraViewQuadGeode( 0 ),
mCameraViewQuadGeometry( 0 ),
mCameraViewQuadVertices( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity(
    ves::xplorer::scenegraph::Group* rootNode,
    ves::xplorer::scenegraph::DCS* worldDCS,
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::ResourceManager* resourceManager,
    ves::xplorer::HeadsUpDisplay* headsUpDisplay )
:
osg::Camera(),
mTexGenNode( 0 ),
mProjectionTechnique( 0 ),
mCameraEntityCallback( 0 ),
mHeadsUpDisplay( headsUpDisplay ),
mResourceManager( resourceManager ),
mRootNode( rootNode ),
mWorldDCS( worldDCS ),
mPluginDCS( pluginDCS ),
mCameraDCS( 0 ),
mCameraNode( 0 ),
mFrustumGeode( 0 ),
mFrustumGeometry( 0 ),
mFrustumVertices( 0 ),
mCameraViewQuadDCS( 0 ),
mCameraViewQuadGeode( 0 ),
mCameraViewQuadGeometry( 0 ),
mCameraViewQuadVertices( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity( const CameraEntity& cameraEntity,
                            const osg::CopyOp& copyop )
:
osg::Camera( cameraEntity, copyop ),
mTexGenNode( 0 ),
mProjectionTechnique( 0 ),
mCameraEntityCallback( 0 ),
mHeadsUpDisplay( 0 ),
mResourceManager( 0 ),
mRootNode( 0 ),
mWorldDCS( 0 ),
mPluginDCS( 0 ),
mCameraDCS( 0 ),
mCameraNode( 0 ),
mFrustumGeode( 0 ),
mFrustumGeometry( 0 ),
mFrustumVertices( 0 ),
mCameraViewQuadDCS( 0 ),
mCameraViewQuadGeode( 0 ),
mCameraViewQuadGeometry( 0 ),
mCameraViewQuadVertices( 0 )
{
    if( &cameraEntity != this )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::~CameraEntity()
{
    mHeadsUpDisplay->GetCamera()->removeChild( mCameraViewQuadDCS.get() );

    mPluginDCS->SetTechnique( std::string( "Default" ) );
    mPluginDCS->RemoveTechnique( std::string( "Projection" ) );
    delete mProjectionTechnique;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::Initialize()
{
    //Initialize this
    setRenderOrder( osg::Camera::PRE_RENDER );
    setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    setClearColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    setComputeNearFarMode( osg::Camera::DO_NOT_COMPUTE_NEAR_FAR );
    setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );
    setReferenceFrame( osg::Camera::ABSOLUTE_RF );
    setViewport( 0, 0, 1024, 1024 );
    //Attach the texture and use it as the color buffer
    attach( osg::Camera::COLOR_BUFFER,
          ( mResourceManager->get< osg::Texture2D, osg::ref_ptr >
          ( "CameraViewTexture" ) ).get() );
    //Add the subgraph to render
    addChild( mWorldDCS.get() );

    //Initialize mInitialViewMatrix
    mInitialViewMatrix.makeLookAt( osg::Vec3( 0, 0, 0 ),
                                   osg::Vec3( 0, 1, 0 ),
                                   osg::Vec3( 0, 0, 1 ) );
    setViewMatrix( mInitialViewMatrix );
    setProjectionMatrixAsPerspective( 30.0, 1.0, 5.0, 10.0 );

    //Initialize mMVPT
    mMVPT = osg::Matrix::identity();

    //Initialize mTexGenNode
    mTexGenNode = new osg::TexGenNode();
    mTexGenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
    mTexGenNode->setTextureUnit( 0 );
    mRootNode->addChild( mTexGenNode.get() );

    //Initialize mProjectionTechnique
    mProjectionTechnique = new cpt::ProjectionTechnique();
    mProjectionTechnique->GetAlpha()->set(
        static_cast< float >( 0.3 ) );
    mProjectionTechnique->GetNearPlaneUniform()->set(
        static_cast< float >( 5.0 ) );
    mProjectionTechnique->GetFarPlaneUniform()->set(
        static_cast< float >( 10.0 ) );
    mPluginDCS->AddTechnique( std::string( "Projection" ), mProjectionTechnique );
    mPluginDCS->SetTechnique( std::string( "Projection" ) );

    //Initialize mCameraEntityCallback
    mCameraEntityCallback = new cpt::CameraEntityCallback();
    setUpdateCallback( mCameraEntityCallback.get() );

    //Initialize mCameraDCS
    mCameraDCS = new ves::xplorer::scenegraph::DCS();
    mPluginDCS->addChild( mCameraDCS.get() );
    //Initialize mCameraNode and stateset of mCameraDCS 
    CreateCameraNode();
    //Initialize mFrustumGeode
    CreateViewFrustumGeode();
    //Initialize mHitQuadGeode
    CreateHitQuadGeode();

    //Initialize mCameraViewQuadDCS
    mCameraViewQuadDCS = new ves::xplorer::scenegraph::DCS();
    mCameraViewQuadDCS->setScale( osg::Vec3( 200, 200, 0 ) );
    mHeadsUpDisplay->GetCamera()->addChild( mCameraViewQuadDCS.get() );
    //Initialize mCameraViewQuadGeode
    CreateCameraViewQuadGeode();

    SetNamesAndDescriptions();

    Update();
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
void CameraEntity::CustomKeyboardMouseSelection(
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
            ( (*mFrustumVertices)[ 3 ].x() - (*mFrustumVertices)[ 4 ].x() ) /
            static_cast< double >( quadResolution.first );
        frustumQuadRatio.second =
            ( (*mFrustumVertices)[ 3 ].z() - (*mFrustumVertices)[ 2 ].z() ) /
            static_cast< double >( quadResolution.second );

        osg::Vec3d cameraLensPoint( 0, 0, 0 );
        osg::Vec3d startPoint( 0, 0, 0 );
        osg::Vec3d endPoint( 0, 0, 0 );

        startPoint.x() = (*mFrustumVertices)[ 4 ].x() +
                         ( mousePosition.first * frustumQuadRatio.first );
        startPoint.y() = (*mFrustumVertices)[ 3 ].y();
        startPoint.z() = (*mFrustumVertices)[ 2 ].z() +
                         ( mousePosition.second * frustumQuadRatio.second );

        double yRatio = (*mFrustumVertices)[ 7 ].y() /
                        (*mFrustumVertices)[ 3 ].y();

        endPoint.x() = yRatio * startPoint.x();
        endPoint.y() = (*mFrustumVertices)[ 7 ].y();
        endPoint.z() = yRatio * startPoint.z();

        osg::Matrixd tempMatrix;
        tempMatrix.set( localToWorldMatrix.getData() );
        startPoint = startPoint * tempMatrix;
        endPoint = endPoint * tempMatrix;
        cameraLensPoint = cameraLensPoint * tempMatrix;

        osg::ref_ptr< osgUtil::LineSegmentIntersector > intersector =
            new osgUtil::LineSegmentIntersector( startPoint, endPoint );
        osgUtil::IntersectionVisitor intersectionVisitor( intersector.get() );

        mWorldDCS->accept( intersectionVisitor );

        if( !intersector->containsIntersections() )
        {
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
        yRatio = yCenterLength / (*mFrustumVertices)[ 3 ].y();

        double x = yRatio * (*mFrustumVertices)[ 3 ].x();
        double z = yRatio * (*mFrustumVertices)[ 3 ].z();

        (*mHitQuadVertices)[ 0 ].set( -x, yCenterLength, -z );
        (*mHitQuadVertices)[ 1 ].set(  x, yCenterLength, -z );
        (*mHitQuadVertices)[ 2 ].set(  x, yCenterLength,  z );
        (*mHitQuadVertices)[ 3 ].set( -x, yCenterLength,  z );
        //mHitQuadGeometry->computeFastPathsUsed();
        mHitQuadGeometry->dirtyDisplayList();
        mHitQuadGeometry->dirtyBound();
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CreateCameraNode()
{
    mCameraNode = osgDB::readNodeFile( std::string( "Models/camera.ive" ) );
    
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    //Set bin number to 11 so camera does not occlude geometry from scene
    stateset->setRenderBinDetails( 11, std::string( "RenderBin" ) );
    stateset->setAttribute(
        ( mResourceManager->get
        < osg::Program, osg::ref_ptr >( "CameraProgram" ) ).get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    mCameraDCS->setStateSet( stateset.get() );

    mCameraDCS->addChild( mCameraNode.get() );
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
    //Set bin number to 11 so camera does not occlude geometry from scene
    stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    stateset->setAttribute(
        ( mResourceManager->get
        < osg::Program, osg::ref_ptr >( "FrustumProgram" ) ).get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    mFrustumGeode->setStateSet( stateset.get() );

    mCameraDCS->addChild( mFrustumGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CreateHitQuadGeode()
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
    stateset->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
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
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CreateCameraViewQuadGeode()
{
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
    mCameraViewQuadGeometry->setTexCoordArray( 1, quadTexCoords.get() );

    mCameraViewQuadGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );
    mCameraViewQuadGeode->addDrawable( mCameraViewQuadGeometry.get() );
    mCameraViewQuadDCS->addChild( mCameraViewQuadGeode.get() );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    stateset->setTextureAttributeAndModes( 1,
        ( mResourceManager->get< osg::Texture2D, osg::ref_ptr >
        ( "CameraViewTexture" ) ).get(), osg::StateAttribute::ON );
    stateset->setAttribute(
        ( mResourceManager->get
        < osg::Program, osg::ref_ptr >( "CameraViewProgram" ) ).get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    osg::ref_ptr< osg::Uniform > baseMapUniform =
        new osg::Uniform( "baseMap", 1 );
    stateset->addUniform( baseMapUniform.get() );
    mCameraViewQuadDCS->setStateSet( stateset.get() );
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
    //mFrustumGeometry->computeFastPathsUsed(); 
    mFrustumGeometry->dirtyDisplayList();
    mFrustumGeometry->dirtyBound();

    const double aspectRatio = fabs( fRight - fLeft ) / fabs( fTop - fBottom );
    (*mCameraViewQuadVertices)[ 0 ].set( 0.0,         0.0, 0.0 );
    (*mCameraViewQuadVertices)[ 1 ].set( aspectRatio, 0.0, 0.0 );
    (*mCameraViewQuadVertices)[ 2 ].set( aspectRatio, 1.0, 0.0 );
    (*mCameraViewQuadVertices)[ 3 ].set( 0.0,         1.0, 0.0 );
    //mCameraViewQuadGeometry->computeFastPathsUsed();
    mCameraViewQuadGeometry->dirtyDisplayList();
    mCameraViewQuadGeometry->dirtyBound();

    //Update the uniforms
    mProjectionTechnique->GetNearPlaneUniform()->set(
        static_cast< float >( nearPlane ) );
    mProjectionTechnique->GetFarPlaneUniform()->set(
        static_cast< float >( farPlane ) );
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
void CameraEntity::DisplayProjectionEffect( bool onOff )
{
    if( onOff )
    {
        mPluginDCS->SetTechnique( std::string( "Projection" ) );
    }
    else
    {
        mPluginDCS->SetTechnique( std::string( "Default" ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::DisplayScreenAlignedQuad( bool onOff )
{
    mCameraViewQuadDCS->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Group* CameraEntity::GetRootNode()
{
    return mRootNode.get();
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* CameraEntity::GetWorldDCS()
{
    return mWorldDCS.get();
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
osg::TexGenNode* CameraEntity::GetTexGenNode()
{
    return mTexGenNode.get();
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::SetNamesAndDescriptions()
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );

    mCameraDCS->setDescriptions( descriptorsList );
    mCameraDCS->setName( std::string( "CameraDCS" ) );

    mCameraViewQuadDCS->setDescriptions( descriptorsList );
    mCameraViewQuadDCS->setName( std::string( "CameraViewQuadDCS" ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::SetProjectionEffectOpacity( double value )
{
    mProjectionTechnique->GetAlpha()->set(
        static_cast< float >( value ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::SetQuadResolution( unsigned int value )
{
    mCameraViewQuadDCS->setScale( osg::Vec3( value, value, 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
