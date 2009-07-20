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
#include "SceneGLTransformInfo.h"

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

// ---  VR Juggler Includes --- //
#if __VJ_version >= 2003000
#include <vrj/Draw/OpenGL/Window.h>
#include <vrj/Draw/OpenGL/DrawManager.h>
#include <vrj/Draw/OpenGL/ContextData.h>
#else
#include <vrj/Draw/OGL/GlWindow.h>
#include <vrj/Draw/OGL/GlDrawManager.h>
#include <vrj/Draw/OGL/GlContextData.h>
#endif
#include <vrj/Display/SurfaceViewport.h>
#include <vrj/Display/Frustum.h>
#include <vrj/Display/Projection.h>

// --- OSG Includes --- //

//#include <osgUtil/SceneView>


// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
SceneGLTransformInfo::SceneGLTransformInfo(
    const gmtl::Matrix44d& ortho2DMatrix,
    const gmtl::Matrix44d& identityMatrix )
    :
    m_ortho2DMatrix( ortho2DMatrix ),
    m_osgOrtho2DMatrix( m_ortho2DMatrix.mData ),
    m_identityMatrix( identityMatrix ),
    m_osgIdentityMatrix( m_identityMatrix.mData )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SceneGLTransformInfo::~SceneGLTransformInfo()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
#if __VJ_version >= 2003000
scenegraph::GLTransformInfoPtr const SceneGLTransformInfo::GetGLTransformInfo(
    vrj::ViewportPtr const viewport )
#else
scenegraph::GLTransformInfoPtr const SceneGLTransformInfo::GetGLTransformInfo(
    vrj::Viewport* const viewport )
#endif
{
    GLTransformInfoMap::const_iterator itr =
        (*m_glTransformInfoMap).find( viewport );
    if( itr != (*m_glTransformInfoMap).end() )
    {
        return itr->second;
    }
    else
    {
        std::cout << "SceneGLTransformInfo::GetGLTransformInfo - "
                  << "GLTransformInfo not found!" << std::endl;

        return scenegraph::GLTransformInfoPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& SceneGLTransformInfo::GetOrtho2DMatrix() const
{
    return m_ortho2DMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& SceneGLTransformInfo::GetOSGOrtho2DMatrix() const
{
    return m_osgOrtho2DMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& SceneGLTransformInfo::GetIdentityMatrix() const
{
    return m_identityMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& SceneGLTransformInfo::GetOSGIdentityMatrix() const
{
    return m_osgIdentityMatrix;
}
////////////////////////////////////////////////////////////////////////////////
void SceneGLTransformInfo::Initialize()
{
    //Get window and viewport information
#if __VJ_version >= 2003000
    vrj::opengl::DrawManager* glDrawManager =
        vrj::opengl::DrawManager::instance();
    vrj::opengl::UserData* userData = glDrawManager->currentUserData();
    vrj::opengl::WindowPtr glWindow = userData->getGlWindow();
    vrj::DisplayPtr display = glWindow->getDisplay();
#else
    vrj::GlDrawManager* glDrawManager = vrj::GlDrawManager::instance();
    vrj::GlUserData* userData = glDrawManager->currentUserData();
    vrj::GlWindowPtr glWindow = userData->getGlWindow();
    vrj::DisplayPtr display = glWindow->getDisplay();
#endif

    //Get state info about the screen
    int windowOriginX, windowOriginY, windowWidth, windowHeight;
    display->getOriginAndSize(
        windowOriginX, windowOriginY, windowWidth, windowHeight );

    size_t numViewports = display->getNumViewports();
    for( size_t i = 0; i < numViewports; ++i )
    {
#if __VJ_version >= 2003000
        vrj::ViewportPtr viewport = display->getViewport( i );
#else
        vrj::Viewport* viewport = display->getViewport( i );
#endif

        //Get state info about the viewport
        float vp_ox, vp_oy, vp_sx, vp_sy;
        viewport->getOriginAndSize( vp_ox, vp_oy, vp_sx, vp_sy );

        const int viewportOriginX = static_cast< int >( vp_ox * windowWidth );
        const int viewportOriginY = static_cast< int >( vp_oy * windowHeight );
        const int viewportWidth = static_cast< int >( vp_sx * windowWidth );
        const int viewportHeight = static_cast< int >( vp_sy * windowHeight );

        //Calculate the window matrix for the viewport
        gmtl::Matrix44d windowMatrix;
        windowMatrix.mState =
            gmtl::Matrix44d::AFFINE | gmtl::Matrix44d::NON_UNISCALE;
        windowMatrix.mData[  0 ] = 0.5 * viewportWidth;
        windowMatrix.mData[  5 ] = 0.5 * viewportHeight;
        windowMatrix.mData[ 10 ] = 0.5;
        windowMatrix.mData[ 12 ] = ( 0.5 * viewportWidth ) + viewportOriginX;
        windowMatrix.mData[ 13 ] = ( 0.5 * viewportHeight ) + viewportOriginY;
        windowMatrix.mData[ 14 ] = 0.5;

        scenegraph::GLTransformInfoPtr glTransformInfo =
            scenegraph::GLTransformInfoPtr( new scenegraph::GLTransformInfo(
                viewportOriginX, viewportOriginY, viewportWidth, viewportHeight,
                0, 0, windowWidth, windowHeight,
                windowMatrix ) );

        (*m_glTransformInfoMap)[ viewport ] = glTransformInfo;
    }
}
////////////////////////////////////////////////////////////////////////////////
