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

// --- VE-Suite Includes --- //
#include <ves/xplorer/communication/CommunicationHandler.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>
#include <ves/xplorer/scenegraph/camera/CameraObject.h>

#include <ves/xplorer/scenegraph/Masks.h>
#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>
#include <ves/xplorer/scenegraph/HeadPositionCallback.h>

#include <ves/xplorer/scenegraph/technique/ProjectionTechnique.h>

#include <ves/xplorer/Debug.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/Transform.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/TexGenNode>

#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

#include <gmtl/gmtl.h>

using namespace ves::xplorer::scenegraph::camera;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CameraManager::CameraManager()
    :
    osg::Group(),
    m_enabled( false ),
    m_cptEnabled( false ),
    m_projectEffect( false ),
    m_activeCameraObject( NULL ),
    m_rttQuad( NULL ),
    m_rttQuadTransform( new osg::PositionAttitudeTransform() ),
    m_projectionTechnique( new technique::ProjectionTechnique() ),
    m_texGenNode( new osg::TexGenNode() ),
    m_isPictureMode( false ),
    m_isTakingScreenCap( false ),
    m_imageDir( "." )
{
    Enable();

    if( !SceneManager::instance()->IsDesktopMode() )
    {
        m_rttQuadTransform->setUpdateCallback( new HeadPositionCallback() );
    }

    m_rttQuad = CreateMasterCameraQuad();
    m_rttQuadTransform->addChild( m_rttQuad.get() );

    //Initialize m_projectionTechnique
    scenegraph::Group& gpm =
        SceneManager::instance()->GetGraphicalPluginManager();
    gpm.AddTechnique( "Projection", m_projectionTechnique );
    m_projectionTechnique->SetAlpha( 0.3 );

    //Initialize m_texGenNode
    m_texGenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
    m_texGenNode->setTextureUnit( 6 );
}
////////////////////////////////////////////////////////////////////////////////
CameraManager::CameraManager(
    const CameraManager& cameraManager, const osg::CopyOp& copyop )
    :
    osg::Group( cameraManager, copyop ),
    m_enabled( cameraManager.m_enabled ),
    m_cptEnabled( cameraManager.m_cptEnabled ),
    m_activeCameraObject( cameraManager.m_activeCameraObject ),
    m_rttQuad( cameraManager.m_rttQuad.get() ),
    m_rttQuadTransform( cameraManager.m_rttQuadTransform.get() ),
    m_projectionTechnique( cameraManager.m_projectionTechnique )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraManager::~CameraManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::addChild( std::string const& name )
{
    osg::ref_ptr< CameraObject > cameraObject =
        new CameraObject( m_projectionTechnique, m_texGenNode.get() );
    cameraObject->setName( name );
    DCS& dcs = cameraObject->GetDCS();
    const gmtl::AxisAngled myAxisAngle(
        osg::DegreesToRadians( double( -90 ) ), 1, 0, 0 );
    gmtl::Matrix44d myMat = gmtl::make< gmtl::Matrix44d >( myAxisAngle );
    ///We need to rotate the camera geometry 90 initially so that the geometry
    ///is in VR Juggler space (y up) so that when the view matrix is multiplied
    ///in the 90 is taken back out.
    myMat = ves::xplorer::scenegraph::SceneManager::instance()->
        GetGlobalViewMatrix() * myMat;
    dcs.SetMat( myMat );

    return osg::Group::addChild( cameraObject.get() );
}
////////////////////////////////////////////////////////////////////////////////
CameraObject* const CameraManager::ConvertNodeToCameraObject(
    osg::Node* const node )
{
    return dynamic_cast< CameraObject* >( node );
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::DisplayProjectionEffect(
    bool const& onOff,
    bool const& calledByGUI )
{
    scenegraph::Group& gpm =
        SceneManager::instance()->GetGraphicalPluginManager();
    if( calledByGUI )
    {
        m_projectEffect = onOff;
        if( m_projectEffect )
        {
            gpm.addChild( m_texGenNode.get() );
            if( m_activeCameraObject )
            {
                gpm.SetTechnique( "Projection" );
            }
        }
        else
        {
            gpm.removeChild( m_texGenNode.get() );
            gpm.SetTechnique( "Default" );
        }
    }
    else
    {
        if( onOff )
        {
            gpm.SetTechnique( "Projection" );
        }
        else
        {
            gpm.SetTechnique( "Default" );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::Enable( const bool& enable )
{
    m_enabled = enable;

    if( m_enabled )
    {
        setNodeMask( NodeMask::CAMERA_MANAGER );
    }
    else
    {
        setNodeMask( NodeMask::NONE );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::EnableCPT( const bool& enable )
{
    m_cptEnabled = enable;
}
////////////////////////////////////////////////////////////////////////////////
CameraObject* const CameraManager::GetActiveCameraObject() const
{
    return m_activeCameraObject;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::Handle(
    Event::Enum event,
    osgUtil::LineSegmentIntersector& deviceInput )
{
    osgUtil::LineSegmentIntersector::Intersections& intersections =
        scenegraph::TestForIntersections(
            deviceInput, *this, TraversalMask::CAMERA );

    CameraObject* cameraObject( NULL );
    if( !intersections.empty() )
    {
        osg::NodePath nodePath = intersections.begin()->nodePath;
        cameraObject =
            ConvertNodeToCameraObject(
                scenegraph::FindVESObject( nodePath )->getParent( 0 ) );
    }

    switch( event )
    {
    case Event::FOCUS:
    {
        if( cameraObject )
        {
            //camera->DoSomething(); like change color or something
        }

        break;
    }
    case Event::RELEASE:
    {
        SetActiveCameraObject( cameraObject, true );

        break;
    }
    default:
    {
        SetActiveCameraObject( NULL, true );

        break;
    }
    } //end switch( event )

    return cameraObject;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::insertChild( unsigned int index, CameraObject* child )
{
    return osg::Group::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
const bool CameraManager::IsEnabled() const
{
    return m_enabled;
}
////////////////////////////////////////////////////////////////////////////////
bool const& CameraManager::IsCPTEnabled() const
{
    return m_cptEnabled;
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::removeChildren()
{
    m_rttQuad->getStateSet()->setTextureAttributeAndModes(
        0, NULL, osg::StateAttribute::OFF );

    SetActiveCameraObject( NULL );

    _children.clear();
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::replaceChild( CameraObject* origChild, CameraObject* newChild )
{
    return osg::Group::replaceChild( origChild, newChild );
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::SetActiveCameraObject(
    CameraObject* cameraObject,
    const bool& sendDataToConductor )
{
    if( cameraObject == m_activeCameraObject )
    {
        return;
    }

    if( m_isPictureMode && !cameraObject )
    {
        return;
    }

    //Turn off rendering for previously active camera
    if( m_activeCameraObject )
    {
        m_activeCameraObject->EnableCamera( false );
    }

    if( cameraObject )
    {
        cameraObject->SetRenderQuadTexture( *(m_rttQuad.get()) );
        m_rttQuadTransform->setNodeMask( 1 );
        cameraObject->EnableCamera();
        cameraObject->Update();
    }
    else
    {
        m_rttQuadTransform->setNodeMask( 0 );
    }

    //Set the active camera
    m_activeCameraObject = cameraObject;

    if( m_projectEffect )
    {
        bool enableGlobalProjectionEffects(cameraObject);
        //If we are setting the active camera to null AND we have selected that
        //the projection effect should be used for all cameras then we need to 
        //disable all projection effects across all cameras. We use the 
        //cameraObject pointer in conjuction with the m_projectEffect as the 
        //mechanism to determine if this is the case. 
        DisplayProjectionEffect( enableGlobalProjectionEffects, false );
    }

    //Send active object to conductor if we need to
    if( sendDataToConductor )
    {
        UpdateConductorData();
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::SetCameraViewQuadResolution( unsigned int const& scale )
{
    if( SceneManager::instance()->IsDesktopMode() )
    {
        m_rttQuadTransform->setScale( osg::Vec3( scale, scale, 1.0 ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::setChild( unsigned int i, CameraObject* node )
{
    return osg::Group::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::SetProjectionEffectOpacity( double const& value )
{
    m_projectionTechnique->SetAlpha( static_cast< float >( value ) );
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* CameraManager::CreateMasterCameraQuad()
{
    ///These are in pixel space
    osg::ref_ptr< osg::Vec3Array > cameraViewQuadVertices =
        new osg::Vec3Array();
    cameraViewQuadVertices->resize( 4 );
    if( SceneManager::instance()->IsDesktopMode() )
    {
        (*cameraViewQuadVertices)[ 0 ].set( 0.0, 0.0, 0.0 );
        (*cameraViewQuadVertices)[ 1 ].set( 1.0, 0.0, 0.0 );
        (*cameraViewQuadVertices)[ 2 ].set( 1.0, 1.0, 0.0 );
        (*cameraViewQuadVertices)[ 3 ].set( 0.0, 1.0, 0.0 );

        //Set initial scale to match quad size in UI
        m_rttQuadTransform->setScale( osg::Vec3( 300.0, 300.0, 1.0 ) );
    }
    else
    {
        (*cameraViewQuadVertices)[ 0 ].set( -1.5,  0.01, -1.5 );
        (*cameraViewQuadVertices)[ 1 ].set(  1.5,  0.01, -1.5 );
        (*cameraViewQuadVertices)[ 2 ].set(  1.5,  0.01,  1.5 );
        (*cameraViewQuadVertices)[ 3 ].set( -1.5,  0.01,  1.5 );
    }

    //Get the texture coordinates for the quad
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0.0, 0.0 );
    (*quadTexCoords)[ 1 ].set( 1.0, 0.0 );
    (*quadTexCoords)[ 2 ].set( 1.0, 1.0 );
    (*quadTexCoords)[ 3 ].set( 0.0, 1.0 );

    //Create the quad geometry
    osg::ref_ptr< osg::Geometry > quadGeometry = new osg::Geometry();
    quadGeometry->setVertexArray( cameraViewQuadVertices.get() );
    quadGeometry->addPrimitiveSet( new osg::DrawArrays( 
        osg::PrimitiveSet::QUADS, 0, cameraViewQuadVertices->size() ) );
    quadGeometry->setTexCoordArray( 0, quadTexCoords.get() );

    //
    {
        osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
        std::string fragmentSource =
        "uniform sampler2D baseMap; \n"

        "void main() \n"
        "{ \n"
            "gl_FragData[ 0 ] = texture2D( baseMap, gl_TexCoord[ 0 ].xy ); \n"
        "} \n";

        fragmentShader->setType( osg::Shader::FRAGMENT );
        fragmentShader->setShaderSource( fragmentSource );
        fragmentShader->setName( "Camera Quad Fragment Shader" );

        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( fragmentShader.get() );
        program->setName( "Camera Quad Program" );

        osg::ref_ptr< osg::StateSet > stateset =
            quadGeometry->getOrCreateStateSet();
        stateset->setAttributeAndModes(
            program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        //Here we attach the texture for the text
        stateset->addUniform( new osg::Uniform( "baseMap", 0 ) );
    }

    //Create the outline geometry
    GLushort idxLoops[ 4 ] = { 0, 1, 2, 3 };
    osg::ref_ptr< osg::Geometry > lineGeometry = new osg::Geometry();
    lineGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINE_LOOP, 4, idxLoops ) );

    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();
    colorArray->push_back( osg::Vec4( 0.33, 0.87, 0.56, 1.0 ) );
    lineGeometry->setColorArray( colorArray.get() );
    lineGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    //
    {
        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( 2.0 );

        osg::ref_ptr< osg::StateSet > stateset =
            lineGeometry->getOrCreateStateSet();
        stateset->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );
    }

    //
    osg::Geode* quadGeode = new osg::Geode();
    quadGeode->setCullingActive( false );
    quadGeode->addDrawable( quadGeometry.get() );
    quadGeode->addDrawable( lineGeometry.get() );

    //
    {
        osg::ref_ptr< osg::StateSet > stateset =
            quadGeode->getOrCreateStateSet();
        stateset->setMode(
            GL_LIGHTING,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    return quadGeode;
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* CameraManager::GetCameraManagerQuad()
{
    if( !m_rttQuadTransform.valid() )
    {
        return NULL;
    }

    return m_rttQuadTransform.get();
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::UpdateConductorData( ves::open::xml::DataValuePairPtr inDvp )
{
    if( !ves::xplorer::scenegraph::SceneManager::instance()->IsMasterNode() )
    {
        return;
    }

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "UPDATE_ACTIVE_CAMERA_OBJECT" );

    if( inDvp )
    {
        command->AddDataValuePair( inDvp );
    }
    else
    {
        unsigned int position = getChildIndex( m_activeCameraObject );
        open::xml::DataValuePairPtr dvp( new ves::open::xml::DataValuePair() );
        dvp->SetData( "ActiveCameraObject", position );
        command->AddDataValuePair( dvp );
    }

    communication::CommunicationHandler::instance()->SetXMLCommand( command );
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::WriteAllImageFiles( std::string const& saveImageDir )
{
    if( !saveImageDir.empty() )
    {
        m_imageDir = saveImageDir;
    }

    CameraObject* cameraObject( NULL );
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        cameraObject = ConvertNodeToCameraObject( getChild( i ) );
        if( cameraObject )
        {
            cameraObject->WriteImageFile( m_imageDir );
        }
        else
        {
            //error output!
        }
    }
    m_isTakingScreenCap = true;
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::WriteActiveCameraImageFile( std::string const& saveImageDir )
{
    if( !saveImageDir.empty() )
    {
        m_imageDir = saveImageDir;
    }

    scenegraph::camera::CameraObject* const activeCameraObject =
        GetActiveCameraObject();
    if( activeCameraObject )
    {
        activeCameraObject->WriteImageFile( m_imageDir );
        m_isTakingScreenCap = true;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::SetPictureMode( bool isPictureMode )
{
    m_isPictureMode = isPictureMode;
    if( m_isPictureMode )
    {
        addChild( "HeadShot" );
        m_headShotCamera = 
            ConvertNodeToCameraObject( getChild( getNumChildren() - 1 ) );
        SetActiveCameraObject( m_headShotCamera.get() );
        m_headShotCamera->MakeHeadTrackedCamera();
    }
    else
    {
        removeChild( m_headShotCamera.get() );
        m_headShotCamera = 0;
        SetActiveCameraObject( NULL );
    }
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::IsPictureMode()
{
    return m_isPictureMode;
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::LatePreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::PostFrameUpdate()
{
    if( m_isTakingScreenCap )
    {
        CameraObject* cameraObject( NULL );
        for( unsigned int i = 0; i < getNumChildren(); ++i )
        {
            cameraObject = ConvertNodeToCameraObject( getChild( i ) );
            if( cameraObject )
            {
                cameraObject->PostWriteImageFile();
            }
        }
        m_isTakingScreenCap = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::SetImageStoreDirectory( const std::string& imageDir )
{
    m_imageDir = imageDir;
}
////////////////////////////////////////////////////////////////////////////////
