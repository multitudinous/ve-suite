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

// --- wxWidgets Includes --- //
#include <wx/wxprec.h>

#if !wxUSE_GLCANVAS
    #error "OpenGL required: set wxUSE_GLCANVAS to 1 and rebuild the library"
#endif

// --- My Includes --- //
#include "CameraEntityView.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneManager.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>

#include <osgUtil/SceneView>

#include <osgDB/ReadFile>

using namespace cpt;

BEGIN_EVENT_TABLE( CameraEntityView, wxGLCanvas )
    EVT_SIZE( CameraEntityView::OnSize )
    EVT_PAINT( CameraEntityView::OnPaint )
    EVT_ERASE_BACKGROUND( CameraEntityView::OnEraseBackground )
    EVT_ENTER_WINDOW( CameraEntityView::OnEnterWindow )
END_EVENT_TABLE()

CameraEntityView::CameraEntityView( wxWindow *parent,
                                    wxWindowID id,
                                    const wxPoint& pos,
                                    const wxSize& size,
                                    long style,
                                    const wxString& name )
        :
        wxGLCanvas( parent,
                    ( wxGLCanvas* ) NULL,
                    id,
                    pos,
                    size,
                    style | wxFULL_REPAINT_ON_RESIZE,
                    name )
{   
    //Create the view of the scene
    mSceneView = new osgUtil::SceneView();
    mSceneView->setDefaults();

    osg::ref_ptr< osg::MatrixTransform > matrixTransform =
        new osg::MatrixTransform();
    matrixTransform->setMatrix(
        osg::Matrix::translate( osg::Vec3d( 0.0, 50.0, 0.0 ) ) );
    osg::ref_ptr< osg::Node > node =
        osgDB::readNodeFile( std::string( "cow.osg" ) );
    matrixTransform->addChild( node.get() );

    //Add model to viewer.
    mSceneView->setSceneData( matrixTransform.get() );
}
////////////////////////////////////////////////////////////////////////////////
CameraEntityView::~CameraEntityView()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityView::Render()
{
    wxPaintDC dc( this );

#ifndef __WXMOTIF__
    if( !GetContext() )
    {
        return;
    }
#endif

    SetCurrent();
    
    //Set the view
    mSceneView->setViewMatrixAsLookAt( osg::Vec3( 0, 0, 0 ),
                                       osg::Vec3( 0, 1, 0 ),
                                       osg::Vec3( 0, 0, 1 ) );

    mSceneView->setProjectionMatrixAsPerspective( 30.0, 1.0, 5.0, 10.0 );

    //Do the update traversal for the scene graph
    mSceneView->update();

    //Do the cull traversal
    //Collect all objects into a sorted set of rendering bins
    mSceneView->cull();

    //Draw the rendering bins
    mSceneView->draw();

    SwapBuffers();
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityView::OnEnterWindow( wxMouseEvent& event )
{
    SetFocus();
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityView::OnPaint( wxPaintEvent& WXUNUSED( event ) )
{
    Render();
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityView::OnSize( wxSizeEvent& event )
{
    //This is also necessary to update the context on some platforms
    wxGLCanvas::OnSize( event );

    //Set GL viewport( not called by wxGLCanvas::OnSize on all platforms )
    int w, h;
    GetClientSize( &w, &h );
#ifndef __WXMOTIF__
    if( GetContext() )
#endif
    {
        SetCurrent();
        glViewport( 0, 0, (GLint) w, (GLint) h );
    }

    mSceneView->setViewport( 0, 0, w, h );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityView::OnEraseBackground( wxEraseEvent& WXUNUSED( event ) )
{
    //Do nothing, to avoid flashing
}
////////////////////////////////////////////////////////////////////////////////
