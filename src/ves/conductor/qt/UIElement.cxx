/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
////////////////////////////////////////////////////////////////////////////////

// --- VES Includes --- //
#include <ves/conductor/qt/UIElement.h>

#include <ves/xplorer/eventmanager/EventManager.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

// --- OSG Includes --- //
#include <osg/Vec4f>
#include <osg/Matrix>
#include <osg/MatrixTransform>
#include <osg/AnimationPath>
#include <osg/Switch>
#include <osg/Geometry>
#include <osg/Texture2D>

#include <osg/io_utils>

// --- C++ Includes --- //
#include <iostream>

#include <boost/concept_check.hpp>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
UIElement::UIElement():
    mIsMinimized( false ),
    mUIMatrixDirty( false ),
    //mUITransform( 0 ),
    //mElementTransform( 0 ),
    //mElementMatrixDirty( false ),
    mAnimationOn( false ),
    //mGeode( 0 ),
    m_mouseInsideUI( true ),
    m_pixelUIRatio( 0 ),
    m_osgImage( new osg::Image )
{
    m_desktopSize = std::make_pair< int, int >( 0, 0 );
    //Request connection to UIManager.EnterLeaveUI signal
    CONNECTSIGNAL_1( "UIManager.EnterLeaveUI", void( bool ), &UIElement::UIEnterLeave,
                    m_connections, highest_Priority );
}
////////////////////////////////////////////////////////////////////////////////
UIElement::~UIElement()
{

}
////////////////////////////////////////////////////////////////////////////////
void UIElement::PostConstructor()
{
    m_vertices = new osg::Vec3Array();
    if( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        m_vertices->push_back( osg::Vec3( -1.0f, -1.0f, -1.0f ) );
        m_vertices->push_back( osg::Vec3(  1.0f, -1.0f, -1.0f ) );
        m_vertices->push_back( osg::Vec3(  1.0f,  1.0f, -1.0f ) );
        m_vertices->push_back( osg::Vec3( -1.0f,  1.0f, -1.0f ) );
    }
    else
    {
        m_pixelUIRatio = double( 967 )/double( 6 );
        m_uiSize.first = 600;
        m_uiSize.second = 967;

        double tempWidth = 0.5f * double( 600 ) * (double( 6 )/double( 967 ));
        //600 x 967 - 3/967 = 0.00310237
        // 1.8614 x 3 
        m_vertices->push_back( osg::Vec3( -tempWidth, 0.1f, -3.0 ) ); //ll
        m_vertices->push_back( osg::Vec3(  tempWidth, 0.1f, -3.0 ) ); //lr
        m_vertices->push_back( osg::Vec3(  tempWidth, 0.1f,  3.0 ) ); //ur
        m_vertices->push_back( osg::Vec3( -tempWidth, 0.1f,  3.0 ) ); //ul
    }
    
    //
    osg::Vec4f coordinates = GetTextureCoordinates();
    float m_left = coordinates.w();
    float m_right = coordinates.x();
    float m_bottom = coordinates.y();
    float m_top = coordinates.z();

    osg::ref_ptr< osg::Vec2Array > texture_coordinates = new osg::Vec2Array();
    texture_coordinates->push_back( osg::Vec2( m_left, m_bottom ) );
    texture_coordinates->push_back( osg::Vec2( m_right, m_bottom ) );
    texture_coordinates->push_back( osg::Vec2( m_right, m_top ) );
    texture_coordinates->push_back( osg::Vec2( m_left, m_top ) );

    //
    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
    geometry->setVertexArray( m_vertices.get() );
    geometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );
    geometry->setTexCoordArray( 0, texture_coordinates.get() );
    /*osg::ref_ptr<osg::Vec4Array> colors = new osg::Vec4Array( 1 );
    ( *colors )[0].set( 1.0f, 1.0f, 0.0f, 0.2f );
    geometry->setColorArray( colors.get() );
    geometry->setColorBinding( osg::Geometry::BIND_OVERALL );*/

    //
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    geode->setCullingActive( false );
    geode->setDataVariance( osg::Object::DYNAMIC );
    geode->addDrawable( geometry.get() );
    mGeode = geode.get();
    mGeode->setName( "Qt UI" );
    
    //Create an empty image
    osg::ref_ptr< osg::Image > image = new osg::Image();

    //Attach the image in a Texture2D object
    osg::ref_ptr< osg::Texture2D > texture = new osg::Texture2D();
    texture->setResizeNonPowerOfTwoHint( false );
    texture->setImage( image.get() );
    texture->setDataVariance( osg::Object::DYNAMIC );

    //Create stateset for adding texture
    osg::ref_ptr< osg::StateSet > stateset = mGeode->getOrCreateStateSet();
    stateset->setTextureAttributeAndModes(
        0, texture.get(), osg::StateAttribute::ON );

    PushUIMatrix( osg::Matrix::identity() );
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* UIElement::GetGeode()
{
    return mGeode.get();
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SetInitialImageWidthAndHeight( int width, int height )
{
    m_initialImageSize = std::make_pair< int, int >( width, height );
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SetScreenDimensions( int width, int height )
{
    m_desktopSize = std::make_pair< int, int >( width, height );
}
////////////////////////////////////////////////////////////////////////////////
int UIElement::GetImageWidth()
{
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
int UIElement::GetImageHeight()
{
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
int UIElement::GetElementWidth()
{
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
int UIElement::GetElementHeight()
{
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec4f UIElement::GetTextureCoordinates()
{
    return osg::Vec4f( 0.f, 1.f, 0.f, 1.f );
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SendInteractionEvent( xplorer::eventmanager::InteractionEvent& event )
{
    boost::ignore_unused_variable_warning( event );
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SendButtonPressEvent( gadget::Keys button, int x, int y, int state )
{
    boost::ignore_unused_variable_warning( button );
    boost::ignore_unused_variable_warning( x );
    boost::ignore_unused_variable_warning( y );
    boost::ignore_unused_variable_warning( state );
    std::cerr << "UIElement::SendButtonPressEvent If you see this we have problems." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SendButtonReleaseEvent( gadget::Keys button, int x, int y, int state )
{
    boost::ignore_unused_variable_warning( button );
    boost::ignore_unused_variable_warning( x );
    boost::ignore_unused_variable_warning( y );
    boost::ignore_unused_variable_warning( state );
    std::cerr << "UIElement::SendButtonReleaseEvent If you see this we have problems." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SendDoubleClickEvent( gadget::Keys button, int x, int y, int state )
{
    boost::ignore_unused_variable_warning( button );
    boost::ignore_unused_variable_warning( x );
    boost::ignore_unused_variable_warning( y );
    boost::ignore_unused_variable_warning( state );
    std::cerr << "UIElement::SendDoubleClickEvent If you see this we have problems." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SendMouseMoveEvent( int x, int y, int z, int state )
{
    boost::ignore_unused_variable_warning( x );
    boost::ignore_unused_variable_warning( y );
    boost::ignore_unused_variable_warning( z );
    boost::ignore_unused_variable_warning( state );
    std::cerr << "UIElement::SendMouseMoveEvent If you see this we have problems." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SendKeyPressEvent( gadget::Keys key, int modifierMask, char unicode )
{
    boost::ignore_unused_variable_warning( key );
    boost::ignore_unused_variable_warning( modifierMask );
    boost::ignore_unused_variable_warning( unicode );
    std::cerr << "UIElement::SendKeyPressEvent If you see this we have problems." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SendKeyReleaseEvent( gadget::Keys key, int modifierMask, char unicode )
{
    boost::ignore_unused_variable_warning( key );
    boost::ignore_unused_variable_warning( modifierMask );
    boost::ignore_unused_variable_warning( unicode );
    std::cerr << "UIElement::SendKeyReleaseEvent If you see this we have problems." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SendScrollEvent( int deltaX, int deltaY, int x, int y, int state )
{
    boost::ignore_unused_variable_warning( deltaX );
    boost::ignore_unused_variable_warning( deltaY );
    boost::ignore_unused_variable_warning( x );
    boost::ignore_unused_variable_warning( y );
    boost::ignore_unused_variable_warning( state );
    std::cerr << "UIElement::SendScrollEvent If you see this we have problems." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
osg::Image* UIElement::RenderElementToImage()
{
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
bool UIElement::IsDirty()
{
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool UIElement::SizeDirty()
{
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::Initialize()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SetMinimized( bool state )
{
    mIsMinimized = state;
}
////////////////////////////////////////////////////////////////////////////////
bool UIElement::IsMinimized()
{
    return mIsMinimized;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixf& UIElement::GetUIMatrix()
{
    return mUIMatrices.at( mUIMatrices.size() - 1 );
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::PushUIMatrix( osg::Matrixf const& matrix )
{
    mUIMatrices.push_back( matrix );
    mUIMatrixDirty = true;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixf UIElement::PopUIMatrix()
{
    mUIMatrices.pop_back();
    osg::Matrixf last = GetUIMatrix();
    mUIMatrixDirty = true;
    return last;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::MoveCanvas( float dx, float dy, float dz )
{
    osg::Matrixf trans;
    trans.setTrans( dx, dy, dz );
    mUIMatrices[ mUIMatrices.size() - 1 ] = GetUIMatrix() * trans;
    mUIMatrixDirty = true;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::ResizeCanvas( int width, int height )
{
    boost::ignore_unused_variable_warning( width );
    boost::ignore_unused_variable_warning( height );
    std::cerr << "UIElement::ResizeCanvas If you see this we have problems." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::Update()
{
    if( mAnimationOn )
    {
        // Check whether animation has ended and remove the callback if so
        /*osg::AnimationPathCallback* cb = static_cast < osg::AnimationPathCallback* > ( mUITransform->getUpdateCallback() );
        if( cb->getAnimationTime() >= cb->getAnimationPath()->getLastTime() )
        {
            mUITransform->setUpdateCallback( 0 );
            mAnimationOn = false;
        }*/
        
        //m_animationPath->get
    }

    if( !ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        return;
    }

    if( !ves::xplorer::scenegraph::SceneManager::instance()->
       GetCurrentGLTransformInfo() )
    {
        return;
    }

    if( mUIMatrixDirty )
    {
        osg::Matrixf& tempUIMatrix = GetUIMatrix();
        mUIMatrixDirty = false;

        osg::Vec3d trans = tempUIMatrix.getTrans();
        osg::Vec3d scale = tempUIMatrix.getScale();

        osg::Matrixd const& windowMat = 
            ves::xplorer::scenegraph::SceneManager::instance()->
            GetCurrentGLTransformInfo()->GetInvertedWindowMatrixOSG();
        osg::Vec3 min = trans * windowMat;
        osg::Vec3 max = trans + scale;

        max[ 2 ] = 1.0;
        max = max * windowMat;

        //This assumes that we are spanning the whole ui the height of the screen
        m_vertices->at( 0 ) = osg::Vec3( min.x(), min.y(), -1.0 ); //ll
        m_vertices->at( 1 ) = osg::Vec3( max.x(), min.y(), -1.0 ); //lr
        m_vertices->at( 2 ) = osg::Vec3( max.x(), max.y(), -1.0 ); //ur
        m_vertices->at( 3 ) = osg::Vec3( min.x(), max.y(), -1.0 ); //ul
        
        mGeode->getDrawable( 0 )->dirtyDisplayList();
        mGeode->dirtyBound();
        
        //Now update the stored corners for the UI
        ComputeMouseBoundsForElement();
    }

    /*if( mElementMatrixDirty )
    {
        //mElementTransform->setMatrix( mElementMatrix );
        mElementMatrixDirty = false;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
/*osg::MatrixTransform* UIElement::GetUITransform()
{
    return mUITransform.get();
}*/
////////////////////////////////////////////////////////////////////////////////
/*osg::MatrixTransform* UIElement::GetElementTransform()
{
    return mElementTransform.get();
}*/
////////////////////////////////////////////////////////////////////////////////
bool UIElement::IsVisible()
{
    return static_cast< bool >( mGeode->getNodeMask() );
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SetVisible( bool visible )
{
    if( visible )
    {
        mGeode->setNodeMask( 0x1 );
    }
    else
    {
        mGeode->setNodeMask( 0x0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SetAnimationPath( osg::AnimationPath* path )
{
    mAnimationOn = true;

    //osg::ref_ptr<osg::AnimationPathCallback> aniCallback = new osg::AnimationPathCallback( path );
    //mUITransform->setUpdateCallback( aniCallback.get() );
    m_animationPath = path;
}
////////////////////////////////////////////////////////////////////////////////
//
//    virtual osg::ref_ptr< osg::Geometry > GetCanvasGeometry()
//    {
//        return mCanvasGeometry;
//    }
//
//    virtual void SetCanvasOrigin( float x, float y, float z = 0.0f )
//    {
//        mCanvasOriginX = x;
//        mCanvasOriginY = y;
//        mCanvasOriginZ = z;
//
//        _UpdateCanvasTransform();
//    }
//
//    virtual void SetCanvasSize( float width, float height )
//    {
//        mCanvasWidth = width;
//        mCanvasHeight = height;
//
//        _UpdateCanvasTransform();
//    }
//
//    virtual void SetCanvasWidth( float width )
//    {
//        SetCanvasSize( width, mCanvasHeight );
//    }
//
//    virtual void SetCanvasHeight( float height )
//    {
//        SetCanvasSize( mCanvasWidth, height );
//    }
//
//    virtual void MoveCanvas( float dx, float dy, float dz = 0.0f )
//    {
//        mCanvasOriginX += dx;
//        mCanvasOriginY += dy;
//        mCanvasOriginZ += dz;
//
//        _UpdateCanvasTransform();
//    }
////////////////////////////////////////////////////////////////////////////////
void UIElement::UIEnterLeave( bool uiEnter )
{
    m_mouseInsideUI = uiEnter;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::GetPointIntersectionInPixels( int& x, int& y, osg::Vec3d& point )
{
    if( !ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        x = (point.x() - (*m_vertices)[0].x()) * m_pixelUIRatio;
        //Z is up in OSG and VE-Suite land
        y = (point.z() - (*m_vertices)[0].z()) * m_pixelUIRatio;
    }
    else
    {
        x = x - int( m_uiCorners[ 0 ] );
        y = y - int( m_uiCorners[ 2 ] );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::ComputeMouseBoundsForElement()
{
    //This function basically is creating the screen coordinates to do
    //mouse testing against to see if the mouse is over the ui
    //osg::ref_ptr< osg::Geode > geode = element->GetGeode();
    osg::Vec3Array* vertexArray = 
    static_cast< osg::Vec3Array* >( mGeode->getDrawable( 0 )->asGeometry()->getVertexArray() );
    osg::Vec3& ll = vertexArray->at( 0 );
    osg::Vec3& ur = vertexArray->at( 2 );
    
    osg::Matrixd const& windowMat = 
    ves::xplorer::scenegraph::SceneManager::instance()->
    GetCurrentGLTransformInfo()->GetWindowMatrixOSG();
    osg::Vec3 min = ll * windowMat;
    osg::Vec3 max = ur * windowMat;
    
    // Return in the form (left, right, bottom, top)
    m_uiCorners = osg::Vec4( min.x(), max.x(), min.y(), max.y() );
}
////////////////////////////////////////////////////////////////////////////////
bool UIElement::TestQuadIntersection( int x, int y )
{
    /*std::cout << "Testing (" << x << ", " << y << ") against ("
     << quadPos.x() << ", " << quadPos.y() << ", " << quadPos.z()
     << ", " << quadPos.w() << ")\n";*/
    if( ( x >= m_uiCorners.x() ) && ( x <= m_uiCorners.y() ) &&
       ( y >= m_uiCorners.z() ) && ( y <= m_uiCorners.w() ) )
    {
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec4d& UIElement::GetUICorners()
{
    return m_uiCorners;
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec2d& UIElement::GetTextureCoords( int x, int y )
{
    if( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        m_texCoords[ 0 ] = -1.0;//double( x ) / double( GetImageWidth() );
        m_texCoords[ 1 ] = -1.0;//double( y ) / double( GetImageHeight() );
    }
    else
    {
        m_texCoords[ 0 ] = double( x ) / double( m_uiSize.first );
        m_texCoords[ 1 ] = double( y ) / double( m_uiSize.second );
    }
    return m_texCoords;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::pair< osg::Image*, std::pair< int, int > > > const& UIElement::GetDamagedAreas()
{
    return m_damagedAreas;
}

} // namepsace conductor
} // namespace ves

