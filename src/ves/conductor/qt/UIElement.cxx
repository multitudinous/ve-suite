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
////////////////////////////////////////////////////////////////////////////////

// --- VES Includes --- //
#include <ves/conductor/qt/UIElement.h>

// --- OSG Includes --- //
#include <osg/Vec4f>
#include <osg/Matrix>
#include <osg/MatrixTransform>
#include <osg/AnimationPath>
#include <osg/Switch>
#include <osg/Geometry>
#include <osg/Texture2D>

// --- C++ Includes --- //
#include <iostream>

namespace ves
{
namespace conductor
{

UIElement::UIElement( )
{

}

UIElement::~UIElement( )
{

}

void UIElement::PostConstructor()
{
    //
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array;
    vertices->push_back( osg::Vec3( 0.0f, 0.0f, 0.0 ) );
    vertices->push_back( osg::Vec3( 1.0f, 0.0f, 0.0 ) );
    vertices->push_back( osg::Vec3( 1.0f, 1.0f, 0.0 ) );
    vertices->push_back( osg::Vec3( 0.0f, 1.0f, 0.0 ) );

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
    geometry->setVertexArray( vertices.get() );
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
    osg::ref_ptr<osg::MatrixTransform> elementTransform = new osg::MatrixTransform( );
    elementTransform->setMatrix( osg::Matrix::scale( GetElementWidth( ),
                                                     GetElementHeight( ),
                                                     1.0f ) );
    PushElementMatrix( elementTransform->getMatrix() );
    elementTransform->addChild( geode.get() );
    mElementTransform = elementTransform.get();

    //
    osg::ref_ptr<osg::MatrixTransform> uiTransform = new osg::MatrixTransform( );
    uiTransform->setMatrix( osg::Matrix::identity() );
    PushUIMatrix( uiTransform->getMatrix() );
    uiTransform->addChild( elementTransform.get() );
    mUITransform = uiTransform.get();

    //
    mVisibilitySwitch = new osg::Switch();
    mVisibilitySwitch->addChild( uiTransform.get() );
}

osg::Switch* UIElement::GetVisibilitySwitch()
{
    return mVisibilitySwitch.get();
}

osg::Geode* UIElement::GetGeode()
{
    return mGeode;
}

int UIElement::GetImageWidth( )
{
    return 0;
}

int UIElement::GetImageHeight( )
{
    return 0;
}

int UIElement::GetElementWidth( )
{
    return 0;
}

int UIElement::GetElementHeight( )
{
    return 0;
}

const osg::Vec4f UIElement::GetTextureCoordinates( )
{
    return osg::Vec4f( 0.f, 1.f, 0.f, 1.f );
}

void UIElement::SendInteractionEvent( xplorer::eventmanager::InteractionEvent &event )
{

}

unsigned char* UIElement::RenderElementToImage( )
{
    return 0;
}

bool UIElement::IsDirty( )
{
    return false;
}

void UIElement::Initialize( )
{
    
}

void UIElement::SetMinimized( bool state )
{
    mIsMinimized = state;
}

bool UIElement::IsMinimized( )
{
    return mIsMinimized;
}

osg::Matrixf UIElement::GetUIMatrix( )
{
    return mUIMatrices.at( mUIMatrices.size( ) - 1 );
}

void UIElement::PushUIMatrix( osg::Matrixf matrix )
{
    mUIMatrices.push_back( matrix );
    mUIMatrixDirty = true;
}

osg::Matrixf UIElement::PopUIMatrix( )
{
    osg::Matrixf last = GetUIMatrix( );
    mUIMatrices.pop_back( );
    mUIMatrixDirty = true;
    return last;
}

void UIElement::PushElementMatrix( osg::Matrixf matrix )
{
    mElementMatrix = matrix;
    mElementMatrixDirty = true;
}

osg::Matrixf UIElement::GetElementMatrix( )
{
    return mElementMatrix;
}

void UIElement::MoveCanvas( float dx, float dy, float dz )
{
    osg::Matrixf trans;
    trans.setTrans( dx, dy, dz );
    mUIMatrices[ mUIMatrices.size( ) - 1 ] = GetUIMatrix( ) * trans;
    mUIMatrixDirty = true;
}

void UIElement::Update( )
{
    if( mAnimationOn )
    {
        // Check whether animation has ended and remove the callback if so
        osg::AnimationPathCallback* cb = static_cast < osg::AnimationPathCallback* > ( mUITransform->getUpdateCallback( ) );
        if( cb->getAnimationTime( ) >= cb->getAnimationPath( )->getLastTime( ) )
        {
            mUITransform->setUpdateCallback( 0 );
            mAnimationOn = false;
        }
    }

    if( mUIMatrixDirty )
    {
        mUITransform->setMatrix( GetUIMatrix( ) );
        mUIMatrixDirty = false;
    }

    if( mElementMatrixDirty )
    {
        mElementTransform->setMatrix( mElementMatrix );
        mElementMatrixDirty = false;
    }
}

osg::MatrixTransform* UIElement::GetUITransform( )
{
    return mUITransform;
}

osg::MatrixTransform* UIElement::GetElementTransform( )
{
    return mElementTransform;
}

bool UIElement::IsVisible( )
{
    return mVisibilitySwitch->getValue( 0 );
}

void UIElement::SetVisible( bool visible )
{
    if( visible )
    {
        mVisibilitySwitch->setAllChildrenOn( );
    }
    else
    {
        mVisibilitySwitch->setAllChildrenOff( );
    }
}

void UIElement::SetAnimationPath( osg::AnimationPath* path )
{
    mAnimationOn = true;

    osg::ref_ptr<osg::AnimationPathCallback> aniCallback = new osg::AnimationPathCallback( path );
    mUITransform->setUpdateCallback( aniCallback.get( ) );
}

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
//        _UpdateCanvasTransform( );
//    }
//
//    virtual void SetCanvasSize( float width, float height )
//    {
//        mCanvasWidth = width;
//        mCanvasHeight = height;
//
//        _UpdateCanvasTransform( );
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
//        _UpdateCanvasTransform( );
//    }


} // namepsace conductor
} // namespace ves

