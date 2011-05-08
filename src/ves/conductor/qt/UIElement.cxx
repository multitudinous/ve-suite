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
    mElementMatrixDirty( false ),
    mAnimationOn( false ),
    //mGeode( 0 ),
    m_mouseInsideUI( true )
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
    //
    GetElementWidth();
    GetElementHeight();

    m_vertices = new osg::Vec3Array();
    /*vertices->push_back( osg::Vec3( 0.0f, 0.0f, 0.0 ) );
    vertices->push_back( osg::Vec3( 1.0f, 0.0f, 0.0 ) );
    vertices->push_back( osg::Vec3( 1.0f, 1.0f, 0.0 ) );
    vertices->push_back( osg::Vec3( 0.0f, 1.0f, 0.0 ) );*/
    m_vertices->push_back( osg::Vec3( -1.0f, -1.0f, 1.0 ) );
    m_vertices->push_back( osg::Vec3(  1.0f, -1.0f, 1.0 ) );
    m_vertices->push_back( osg::Vec3(  1.0f,  1.0f, 1.0 ) );
    m_vertices->push_back( osg::Vec3( -1.0f,  1.0f, 1.0 ) );
    
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

    //
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    geode->setCullingActive( false );
    geode->setDataVariance( osg::Object::DYNAMIC );
    geode->addDrawable( geometry.get() );
    mGeode = geode.get();

    //Create an empty image
    osg::ref_ptr< osg::Image > image = new osg::Image();

    //Attach the image in a Texture2D object
    osg::ref_ptr< osg::Texture2D > texture = new osg::Texture2D();
    texture->setResizeNonPowerOfTwoHint( false );
    texture->setImage( image.get() );
    texture->setDataVariance( osg::Object::DYNAMIC );

    //Create stateset for adding texture
    osg::ref_ptr< osg::StateSet > stateset = geode->getOrCreateStateSet();
    stateset->setTextureAttributeAndModes(
        0, texture.get(), osg::StateAttribute::ON );

    // Transform to make unit square appear with same dimensions as underlying
    // element dimensions
    osg::ref_ptr<osg::MatrixTransform> elementTransform = new osg::MatrixTransform();
    elementTransform->setMatrix( osg::Matrix::scale( GetElementWidth(),
                                                     GetElementHeight(),
                                                     1.0f ) );
    //elementTransform->setMatrix( osg::Matrix::identity() );

    PushElementMatrix( elementTransform->getMatrix() );
    //elementTransform->addChild( mGeode.get() );
    mElementTransform = elementTransform.get();

    //
    osg::ref_ptr<osg::MatrixTransform> uiTransform = new osg::MatrixTransform();
    uiTransform->setMatrix( osg::Matrix::identity() );
    PushUIMatrix( uiTransform->getMatrix() );
    //uiTransform->addChild( elementTransform.get() );
    uiTransform->addChild( mGeode.get() );
    mUITransform = uiTransform.get();

    //
    //mVisibilitySwitch = new osg::Switch();
    //mVisibilitySwitch->addChild( uiTransform.get() );
}
////////////////////////////////////////////////////////////////////////////////
/*osg::Switch* UIElement::GetVisibilitySwitch()
{
    return mVisibilitySwitch.get();
}*/
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
unsigned char* UIElement::RenderElementToImage()
{
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
bool UIElement::IsDirty()
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
    osg::Matrixf last = GetUIMatrix();
    mUIMatrices.pop_back();
    mUIMatrixDirty = true;
    return last;
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::PushElementMatrix( osg::Matrixf const& matrix )
{
    mElementMatrix = matrix;
    mElementMatrixDirty = true;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixf& UIElement::GetElementMatrix()
{
    return mElementMatrix;
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
        osg::AnimationPathCallback* cb = static_cast < osg::AnimationPathCallback* > ( mUITransform->getUpdateCallback() );
        if( cb->getAnimationTime() >= cb->getAnimationPath()->getLastTime() )
        {
            mUITransform->setUpdateCallback( 0 );
            mAnimationOn = false;
        }
        
        //m_animationPath->get
    }

    if( mUIMatrixDirty )
    {
        osg::Matrixf& tempUIMatrix = GetUIMatrix();
        //mUITransform->setMatrix( tempUIMatrix );
        mUIMatrixDirty = false;
        
        osg::Vec3d trans = tempUIMatrix.getTrans();

        osg::Matrixd const& windowMat = 
            ves::xplorer::scenegraph::SceneManager::instance()->
            GetCurrentGLTransformInfo()->GetInvertedWindowMatrixOSG();
        osg::Vec3 min = trans * windowMat;
        osg::Vec3 max = trans + osg::Vec3( GetElementWidth(), GetElementHeight(), 1.0 );
        max[ 2 ] = 1.0;
        max = max * windowMat;

        //This assumes that we are spanning the whole ui the height of the screen
        m_vertices->at( 0 ) = osg::Vec3( min.x(), min.y(), 1.0 ); //ll
        m_vertices->at( 1 ) = osg::Vec3( max.x(), min.y(), 1.0 ); //lr
        m_vertices->at( 2 ) = osg::Vec3( max.x(), max.y(), 1.0 ); //ur
        m_vertices->at( 3 ) = osg::Vec3( min.x(), max.y(), 1.0 ); //ul
        
        mGeode->getDrawable( 0 )->dirtyDisplayList();
        mGeode->dirtyBound();
    }

    if( mElementMatrixDirty )
    {
        //mElementTransform->setMatrix( mElementMatrix );
        mElementMatrixDirty = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::MatrixTransform* UIElement::GetUITransform()
{
    return mUITransform.get();
}
////////////////////////////////////////////////////////////////////////////////
/*osg::MatrixTransform* UIElement::GetElementTransform()
{
    return mElementTransform.get();
}*/
////////////////////////////////////////////////////////////////////////////////
bool UIElement::IsVisible()
{
    return static_cast< bool >( mUITransform->getNodeMask() );
    //mVisibilitySwitch->getValue( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void UIElement::SetVisible( bool visible )
{
    if( visible )
    {
        mUITransform->setNodeMask( 0x1 );
        //mVisibilitySwitch->setAllChildrenOn();
    }
    else
    {
        mUITransform->setNodeMask( 0x0 );
        //mVisibilitySwitch->setAllChildrenOff();
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
} // namepsace conductor
} // namespace ves

