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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/camera/CameraManager.h>
#include <ves/xplorer/scenegraph/camera/CameraObject.h>

#include <ves/xplorer/scenegraph/Masks.h>
#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/HeadPositionCallback.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

using namespace ves::xplorer::scenegraph::camera;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CameraManager::CameraManager()
    :
    osg::Group(),
    m_enabled( false ),
    m_activeCameraObject( NULL ),
    m_rttQuad( NULL ),
    m_rttQuadTransform( new osg::PositionAttitudeTransform() )
{
    Enable();

    if( !SceneManager::instance()->IsDesktopMode() )
    {
        m_rttQuadTransform->setUpdateCallback( new HeadPositionCallback() );
    }

    m_rttQuad = CreateMasterCameraQuad();
    m_rttQuadTransform->addChild( m_rttQuad.get() );
}
////////////////////////////////////////////////////////////////////////////////
CameraManager::CameraManager(
    const CameraManager& cameraManager, const osg::CopyOp& copyop )
    :
    osg::Group( cameraManager, copyop ),
    m_enabled( cameraManager.m_enabled ),
    m_activeCameraObject( cameraManager.m_activeCameraObject )
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
    DCS* worldDCS = SceneManager::instance()->GetWorldDCS();
    osg::ref_ptr< CameraObject > cameraObject = new CameraObject();
    cameraObject->setName( name );
    DCS& dcs = cameraObject->GetDCS();
    dcs.SetMat( gmtl::invert( worldDCS->GetMat() ) );

    return osg::Group::addChild( cameraObject.get() );
}
////////////////////////////////////////////////////////////////////////////////
CameraObject* const CameraManager::ConvertNodeToCameraObject(
    osg::Node* const node )
{
    return static_cast< CameraObject* >( node );
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
        SetActiveCameraObject( cameraObject );

        break;
    }
    default:
    {
        SetActiveCameraObject( NULL );

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
void CameraManager::SetActiveCameraObject( CameraObject* cameraObject )
{
    if( cameraObject == m_activeCameraObject )
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
        cameraObject->EnableCamera();
        m_rttQuadTransform->setNodeMask( 1 );
    }
    else
    {
        m_rttQuadTransform->setNodeMask( 0 );
    }

    //Set the active camera
    m_activeCameraObject = cameraObject;
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::SetCameraViewQuadResolution( unsigned int const& scale )
{
    //if( SceneManager::instance()->IsDesktopMode() )
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
        (*cameraViewQuadVertices)[ 0 ].set( -1.0,  0.01, -1.0 );
        (*cameraViewQuadVertices)[ 1 ].set(  1.0,  0.01, -1.0 );
        (*cameraViewQuadVertices)[ 2 ].set(  1.0,  0.01,  1.0 );
        (*cameraViewQuadVertices)[ 3 ].set( -1.0,  0.01,  1.0 );
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
void CameraManager::WriteAllImageFiles( std::string const& filename )
{
    CameraObject* cameraObject( NULL );
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        cameraObject = ConvertNodeToCameraObject( getChild( i ) );
        if( cameraObject )
        {
            cameraObject->WriteImageFile( filename );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
