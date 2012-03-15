/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include <ves/xplorer/Debug.h>

// ---  VR Juggler Includes --- //
#include <vrj/Draw/OpenGL/App.h>
#include <vrj/Draw/OpenGL/Window.h>
#include <vrj/Draw/OpenGL/DrawManager.h>
#include <vrj/Draw/OpenGL/ContextData.h>

#include <vrj/Display/SurfaceViewport.h>
#include <vrj/Display/Frustum.h>
#include <vrj/Display/Projection.h>
#include <vrj/Display/DisplayManager.h>
#include <vrj/Display/SurfaceProjection.h>

#include <vrj/Kernel/User.h>

#include <gmtl/MatrixOps.h>
#include <gmtl/Generate.h>
#include <gmtl/Xforms.h>

#include <gmtl/Misc/MatrixConvert.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
SceneGLTransformInfo::SceneGLTransformInfo(
    gmtl::Matrix44d const& ortho2DMatrix,
    gmtl::Matrix44d const& identityMatrix,
    gmtl::Matrix44d const& zUpMatrix )
    :
    m_ortho2DMatrix( ortho2DMatrix ),
    m_ortho2DMatrixOSG( m_ortho2DMatrix.getData() ),
    m_identityMatrix( identityMatrix ),
    m_identityMatrixOSG( m_identityMatrix.getData() ),
    m_zUpMatrix( zUpMatrix ),
    m_zUpMatrixOSG( m_zUpMatrix.getData() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SceneGLTransformInfo::~SceneGLTransformInfo()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
scenegraph::GLTransformInfoPtr const SceneGLTransformInfo::GetGLTransformInfo(
    vrj::ViewportPtr const viewport )
{
    GLTransformInfoMap::const_iterator itr =
        (*m_glTransformInfoMap).find( viewport );
    if( itr != (*m_glTransformInfoMap).end() )
    {
        return itr->second;
    }
    else
    {
        vprDEBUG( vesDBG, 1 ) << "SceneGLTransformInfo::GetGLTransformInfo - "
                  << "Not initialized yet." << std::endl << vprDEBUG_FLUSH;

        return scenegraph::GLTransformInfoPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& SceneGLTransformInfo::GetOrtho2DMatrix() const
{
    return m_ortho2DMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& SceneGLTransformInfo::GetOrtho2DMatrixOSG() const
{
    return m_ortho2DMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& SceneGLTransformInfo::GetIdentityMatrix() const
{
    return m_identityMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& SceneGLTransformInfo::GetIdentityMatrixOSG() const
{
    return m_identityMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d const& SceneGLTransformInfo::GetZUpMatrix() const
{
    return m_zUpMatrix;
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd const& SceneGLTransformInfo::GetZUpMatrixOSG() const
{
    return m_zUpMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
void SceneGLTransformInfo::Initialize()
{
    //Get window and viewport information
    vrj::opengl::DrawManager* glDrawManager =
        vrj::opengl::DrawManager::instance();
    vrj::opengl::UserData* userData = glDrawManager->currentUserData();
    vrj::opengl::WindowPtr glWindow = userData->getGlWindow();
    vrj::DisplayPtr display = glWindow->getDisplay();

    //Get state info about the screen
    int windowOriginX, windowOriginY, windowWidth, windowHeight;
    display->getOriginAndSize(
        windowOriginX, windowOriginY, windowWidth, windowHeight );

    //Push back window and viewport information
    scenegraph::SceneManager* sceneManager =
        scenegraph::SceneManager::instance();
    unsigned int numViewports = display->getNumViewports();
    for( unsigned int i = 0; i < numViewports; ++i )
    {
        vrj::ViewportPtr viewport = display->getViewport( i );

        //Get state info about the viewport
        float vp_ox, vp_oy, vp_sx, vp_sy;
        viewport->getOriginAndSize( vp_ox, vp_oy, vp_sx, vp_sy );

        const int viewportOriginX = static_cast< int >( vp_ox * windowWidth );
        const int viewportOriginY = static_cast< int >( vp_oy * windowHeight );
        const int viewportWidth = static_cast< int >( vp_sx * windowWidth );
        const int viewportHeight = static_cast< int >( vp_sy * windowHeight );

        scenegraph::GLTransformInfoPtr glTransformInfo(
            new scenegraph::GLTransformInfo( viewport->inStereo() ) );
        glTransformInfo->UpdateViewportValues(
            viewportOriginX, viewportOriginY, viewportWidth, viewportHeight );
        glTransformInfo->UpdateWindowValues( windowWidth, windowHeight );
        (*m_glTransformInfoMap)[ viewport ] = glTransformInfo;
        sceneManager->PushBackGLTransformInfo( viewport, glTransformInfo );
    }
    
    if( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        ves::xplorer::scenegraph::SceneManager::instance()->
            SetCurrentGLTransformInfo( 
            GetGLTransformInfo( display->getViewport( 0 ) ) );
    }
                                                                                      
    vprDEBUG( vesDBG, 1 ) << "SceneGLTransformInfo::Initialize - "
        << "GLTransformInfo is initialized." << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void SceneGLTransformInfo::CalculateCenterViewMatrix( vrj::ProjectionPtr const projection )
{
    /*
    vrj::DisplayManager* displayManager =
        vrj::DisplayManager::instance();
    const std::vector< vrj::DisplayPtr >& displays = 
        displayManager->getActiveDisplays();
    vrj::opengl::DrawManager* glDrawManager =
        vrj::opengl::DrawManager::instance();
    const float positionScale = glDrawManager->getApp()->getDrawScaleFactor();
    for( size_t i = 0; i < displays.size(); ++i )
    {
        vrj::DisplayPtr display = displays.at( i );
        unsigned int numViewports = displays.at( i )->getNumViewports();
        for( unsigned int j = 0; j < numViewports; ++j )
        {
            vrj::ViewportPtr viewport = display->getViewport( j );
            vrj::ProjectionPtr proj = viewport->getLeftProj();

            gmtl::Matrix44f cur_head_pos = 
                viewport->getUser()->getHeadPosProxy()->getData( positionScale );

            const gmtl::Point3f left_eye_pos(
                cur_head_pos * gmtl::Point3f( 0, 0, 0 ) );

            proj->calcViewMatrix( left_eye_pos, positionScale );
            const gmtl::Matrix44f& projMatrix = proj->getViewMatrix();
        }
    }
    */
    //vrj::ProjectionPtr proj = viewport->getLeftProj();
    vrj::ViewportPtr viewport = projection->getViewport();
    const float positionScale = 
        vrj::opengl::DrawManager::instance()->getApp()->getDrawScaleFactor();
    gmtl::Matrix44f cur_head_pos = 
        viewport->getUser()->getHeadPosProxy()->getData( positionScale );
    
    const gmtl::Point3f center_pos( cur_head_pos * gmtl::Point3f( 0, 0, 0 ) );
    projection->calcViewMatrix( gmtl::MAT_IDENTITY44F, center_pos, positionScale );
    const gmtl::Matrix44d& viewMatrix = 
        gmtl::convertTo< double >( projection->getViewMatrix() ) * GetZUpMatrix();
    scenegraph::GLTransformInfoPtr glTI = GetGLTransformInfo( viewport );
    glTI->UpdateCenterViewMatrix( viewMatrix );
}
////////////////////////////////////////////////////////////////////////////////
vrj::Frustum SceneGLTransformInfo::CalculateFrustum( vrj::ViewportPtr const viewport,
                                                    gmtl::Point3f const& eyePoint )
{
    vrj::SurfaceViewportPtr surfaceViewport = 
        boost::static_pointer_cast< vrj::SurfaceViewport >( viewport );

    /*if( !surfaceViewport )
    {
        vrj::Frustum temp;
        return temp;
    }*/

    gmtl::Point3f llCorner;
    gmtl::Point3f lrCorner;
    gmtl::Point3f urCorner;
    gmtl::Point3f ulCorner;

    surfaceViewport->getCorners( llCorner, lrCorner, urCorner, ulCorner );

    const float positionScale = 
        vrj::opengl::DrawManager::instance()->getApp()->getDrawScaleFactor();
    
    gmtl::Matrix44f viewMatrix = 
        gmtl::convertTo< float >( scenegraph::SceneManager::instance()->GetPureNavMatrix() );

    const float invScale = 1./positionScale;
    viewMatrix.mData[ 12 ] = viewMatrix.mData[ 12 ] * invScale;
    viewMatrix.mData[ 13 ] = viewMatrix.mData[ 13 ] * invScale;
    viewMatrix.mData[ 14 ] = viewMatrix.mData[ 14 ] * invScale;
    
    llCorner = viewMatrix * llCorner;
    lrCorner = viewMatrix * lrCorner;
    urCorner = viewMatrix * urCorner;
    ulCorner = viewMatrix * ulCorner;

    vrj::SurfaceProjectionPtr tempProjection;
        //vrj::SurfaceProjection::create( llCorner, lrCorner, urCorner, ulCorner );
    
    tempProjection->calcViewMatrix( gmtl::MAT_IDENTITY44F, eyePoint, positionScale );
    
    return tempProjection->getFrustum();
}
////////////////////////////////////////////////////////////////////////////////
