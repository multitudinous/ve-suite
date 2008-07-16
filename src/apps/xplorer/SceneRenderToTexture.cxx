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

// --- VE-Suite Includes --- //
#include "SceneRenderToTexture.h"

// --- OSG Includes --- //
#include <osg/Camera>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Texture2D>
//Needed for FBO GL extensions
#include <osg/FrameBufferObject>

using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
SceneRenderToTexture::SceneRenderToTexture()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SceneRenderToTexture::~SceneRenderToTexture()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::CreateTexture()
{
    mTexture = new osg::Texture2D();
    mTexture->setInternalFormat( GL_RGB16F_ARB );
    mTexture->setTextureSize( 512, 512 );
    mTexture->setSourceFormat( GL_RGBA );
    mTexture->setSourceType( GL_FLOAT );
    mTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    mTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    mTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    mTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::CreateQuad()
{
    mQuadGeode = new osg::Geode();
    mQuadGeometry = new osg::Geometry();
    mQuadVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
    
    mQuadVertices->resize( 4 );
    (*mQuadVertices)[ 0 ].set( 0, 0, 0 );
    (*mQuadVertices)[ 0 ].set( 1, 0, 0 );
    (*mQuadVertices)[ 0 ].set( 1, 0, 1 );
    (*mQuadVertices)[ 0 ].set( 0, 0, 1 );
    mQuadGeometry->setVertexArray( mQuadVertices.get() );
    
    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0, 0 );
    (*quadTexCoords)[ 0 ].set( 1, 0 );
    (*quadTexCoords)[ 0 ].set( 1, 1 );
    (*quadTexCoords)[ 0 ].set( 0, 1 );
    mQuadGeometry->setTexCoordArray( 0, quadTexCoords.get() );
    
    mQuadGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );
    
    mQuadGeode->addDrawable( mQuadGeometry.get() );
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::CreateCamera()
{
    mCamera = new osg::Camera();
    mCamera->setRenderOrder( osg::Camera::PRE_RENDER );
    mCamera->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    mCamera->setClearColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    mCamera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );
    
    mCamera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
    mCamera->setViewport( 0, 0, 512, 512 );

    //Set the internal format for the render target
    mCamera->attach( osg::Camera::BufferComponent( osg::Camera::COLOR_BUFFER0 ),
                     GL_DEPTH_COMPONENT24 );
    //Attach a texture and use it as the render target
    mCamera->attach( osg::Camera::BufferComponent( osg::Camera::COLOR_BUFFER0 ),
                     mTexture.get() );//, 0, 0, false, 0, 0 );

    mCamera->setViewMatrix( osg::Matrix::identity() );
    mCamera->setProjectionMatrix( osg::Matrix::identity() );
}
////////////////////////////////////////////////////////////////////////////////
