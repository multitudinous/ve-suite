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

// --- My Includes --- //
#include "GLCanvasWrapper.h"
#include "GL_Engine.h"

// --- wxWidgets Includes --- //
#include <wx/sizer.h>

////////////////////////////////////////////////////////////////////////////////
GLCanvasWrapper::GLCanvasWrapper( wxWindow* parent, wxBoxSizer* sizer )
{
    int* attribList = new int[ 6 ];
    attribList[ 0 ] = WX_GL_DEPTH_SIZE;
    attribList[ 1 ] = 16;
    attribList[ 2 ] = WX_GL_STENCIL_SIZE;
    attribList[ 3 ] = 8;
    attribList[ 4 ] = int( WX_GL_DOUBLEBUFFER );
    attribList[ 5 ] = WX_GL_RGBA;
    
    mGLEngine = new GL_Engine( parent, -1, attribList, wxPoint( 0, 0 ),
        wxSize( 800, 600 ), wxSUNKEN_BORDER, wxT( "Stove Design Canvas" ) );
    sizer->Add( mGLEngine, 8, wxALIGN_CENTER_HORIZONTAL );

    delete [] attribList;
}
////////////////////////////////////////////////////////////////////////////////
GLCanvasWrapper::~GLCanvasWrapper()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GLCanvasWrapper::DrawCanvas()
{
    wxPaintEvent event;
    mGLEngine->OnPaint( event );
}
////////////////////////////////////////////////////////////////////////////////
void GLCanvasWrapper::DrawNewBaffle()
{
    mGLEngine->_drawNewBaffle();
}
////////////////////////////////////////////////////////////////////////////////
void GLCanvasWrapper::RemoveBaffle(
    long int temp1, long int temp2, long int temp3, long int temp4 )
{
    mGLEngine->_removeBaffle( temp1, temp2, temp3, temp4 );	
}
////////////////////////////////////////////////////////////////////////////////
void GLCanvasWrapper::RedrawBaffle(
    long int temp1, long int temp2, long int temp3, long int temp4, int index )
{
    mGLEngine->_reDrawBaffle( temp1, temp2, temp3, temp4, index );	
}
////////////////////////////////////////////////////////////////////////////////
float* GLCanvasWrapper::GetGridInfo()
{
    mGridInfo[ 0 ] = mGLEngine->gridptsx[ mGLEngine->actpt1[ 0 ] ];
    mGridInfo[ 1 ] = mGLEngine->gridptsx[ mGLEngine->actpt2[ 0 ] ]; 
    mGridInfo[ 2 ] = mGLEngine->gridptsy[ mGLEngine->actpt1[ 1 ] ];
    mGridInfo[ 3 ] = mGLEngine->gridptsy[ mGLEngine->actpt2[ 1 ] ];

    return mGridInfo;
}
////////////////////////////////////////////////////////////////////////////////
int* GLCanvasWrapper::GetGridPoints( int arrayNum )
{
    if( arrayNum == 1 )
    {
        mGridPoint1[ 0 ] = mGLEngine->actpt1[ 0 ];
        mGridPoint1[ 1 ] = mGLEngine->actpt1[ 1 ];
        mGridPoint1[ 2 ] = mGLEngine->actpt1[ 2 ];
        mGridPoint1[ 3 ] = mGLEngine->actpt1[ 3 ];

        return mGridPoint1;
    }
    else if( arrayNum == 2 )
    {
        mGridPoint2[ 0 ] = mGLEngine->actpt2[ 0 ];
        mGridPoint2[ 1 ] = mGLEngine->actpt2[ 1 ];
        mGridPoint2[ 2 ] = mGLEngine->actpt2[ 2 ];
        mGridPoint2[ 3 ] = mGLEngine->actpt2[ 3 ];

        return mGridPoint2;
    }
    else
    {
        return NULL;
    }
}
////////////////////////////////////////////////////////////////////////////////
