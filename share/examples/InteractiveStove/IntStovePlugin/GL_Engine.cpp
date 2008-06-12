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

// --- My Includes --- //
#include "GL_Engine.h"
#include "IntStoves_UI_Dialog.h"

// --- OpenGL Includes --- //
#ifdef __APPLE__
#include <OpenGL/glu.h>
#include <OpenGL/gl.h>
#include <GLUT/glut.h>
#else
#include <GL/glu.h>
#include <GL/gl.h>
#include <GL/glut.h>
#endif

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>

// --- wxWidgets Includes --- //
#include <wx/wx.h>
#include <wx/sizer.h>

// --- C/C++ Libraries --- //
#include <iostream>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <ostream>
#include <cstdlib>
#include <cstdio>
#include <vector>

BEGIN_EVENT_TABLE( GL_Engine, wxGLCanvas )
EVT_MOUSE_EVENTS( GL_Engine::_onMouse )
EVT_PAINT( GL_Engine::OnPaint )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
GL_Engine::GL_Engine( wxWindow* parent,
                      wxWindowID id,
                      int* attrlist,
                      const wxPoint& pos,
                      const wxSize& size,
                      long style,
                      const wxString& name )
:
#ifndef __WXMAC__
    wxGLCanvas( parent, id, attrlist, pos, size, style, name )
#else
    wxGLCanvas( parent, id, pos, size, style, name, attrlist )
#endif
{
#ifndef WIN32
    //You must initialize glut before using certain functions
    int argc = 0;
    char** argv = NULL;
    glutInit( &argc, argv );
#endif

    for( int i = 0; i < 2; ++i )
    {
        actindex[ i ] = -1;
	    actpt1[ i ] = -1;
	    actpt2[ i ] = -1;
    }

    baffleCreated = false;

    gluOrtho2D( 0.0, 53.0, 0.0, 39.0 );

#ifndef __WXMAC__
    m_glContext = new wxGLContext( this );
#else
    m_glContext = GetContext();
#endif
    m_dialog = dynamic_cast< IntStoves_UI_Dialog* >( parent );
}
////////////////////////////////////////////////////////////////////////////////
GL_Engine::~GL_Engine()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::_draw()
{
    //gridptsx = new int[ 48 ];
    //gridptsy = new int[ 32 ];

	double scaleval = 0.002;
	double offsetx =  -0.95;
	double offsety =  -0.95;

    for( int i = 0; i < 49; ++i )
    {
	    gridptsx[ i ] = static_cast< float >( i ) / 25 + offsetx;
    }
    for( int j = 0; j < 33; ++j )
    {
	    gridptsy[ j ] = static_cast< float >( j ) / 17 + offsety;
    }

    glClearColor( 1.0, 1.0, 1.0, 0.0 );
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    glColor3f( 0.0, 0.0, 0.0 );

    glPointSize( 4.0 );
    glLineWidth( 2.0 );
    glColor3f( 0.0, 1.0, 0.0 );
    glBegin( GL_POINTS );

    for( int i = 0; i < 49; ++i )
    {
	    for( int j = 0; j < 33; ++j )
        {
		    if( i < 8 && j > 7 || i > 7 && j < 8 || i > 7 && j > 7 )
            {
			    if( i > 39 && j < 25 || i < 41 && j > 24 || i < 40 && j < 25 )
                {
				    glVertex2f( gridptsx[ i ], gridptsy[ j ] );
                }
            }
        }
    }

    glEnd();
    glBegin( GL_LINES );
    for( int i = 0; i < 48; ++i )
    {
	    for( int j = 0; j < 32; ++j )
	    {
		    if( i < 8 && j > 7 || i > 7 && j < 8 || i > 7 && j > 7 )
            {
			    if( i > 38 && j < 24 || i < 40 && j > 23 || i < 39 && j < 24 )
			    {
				    glVertex2f( gridptsx[ i ],     gridptsy[ j ] );
				    glVertex2f( gridptsx[ i + 1 ], gridptsy[ j ] );
				    glVertex2f( gridptsx[ i ],     gridptsy[ j ] );
				    glVertex2f( gridptsx[ i ],     gridptsy[ j + 1 ] );

				    if( i == 47 )
				    {
					    glVertex2f( gridptsx[ i + 1 ], gridptsy[ j ] );
					    glVertex2f( gridptsx[ i + 1 ], gridptsy[ j + 1 ] );
				    }

				    if( j == 31 )
				    {
					    glVertex2f( gridptsx[ i ],     gridptsy[ j + 1 ] );
					    glVertex2f( gridptsx[ i + 1 ], gridptsy[ j + 1 ] );
				    }

				    if( i == 39 && j > 23 )
				    {
					    glVertex2f( gridptsx[ i + 1 ], gridptsy[ j ] );
					    glVertex2f( gridptsx[ i + 1 ], gridptsy[ j + 1 ] );
				    }

				    if( j == 23 && i > 38 )
				    {
					    glVertex2f( gridptsx[ i ], gridptsy[ j + 1 ] );
					    glVertex2f( gridptsx[ i + 1 ], gridptsy[ j + 1 ] );
				    }
                }
            }
	    }
	}

    glEnd();
    InletText();
    OutletText();
    glFlush();
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::_checkForMouseHitsOne( int xpos, int ypos )
{
    int zpos = 0;

    GLint viewport[ 4 ];
    GLdouble mvmatrix[ 16 ], projmatrix[ 16 ];

    GLdouble sx, sy, sz;

    //Prepare to Project
    glGetIntegerv( GL_VIEWPORT, viewport );
    glGetDoublev( GL_MODELVIEW_MATRIX, mvmatrix );
    glGetDoublev( GL_PROJECTION_MATRIX, projmatrix );

	for( int i = 0; i < 49; ++i )
    {
		for( int j = 0; j < 33; ++j )
        {
			if( i < 8 && j > 7 || i > 7 && j < 8 || i > 7 && j > 7 )
            {
			    if( i > 39 && j < 25 || i < 41 && j > 24 || i < 40 && j < 25 )
			    {
				    gluProject(	static_cast< GLdouble >( gridptsx[ i ] ),
		    				    static_cast< GLdouble >( gridptsx[ j ] ),
			    			    static_cast< GLdouble >( 0.0 ),
							    mvmatrix,
							    projmatrix,
							    viewport,
							    &sx,
							    &sy,
							    &sz);

				    double distance = ( ( sx - xpos ) * ( sx - xpos ) +
                        ( ( sy * 1.47 ) - ypos ) * ( ( sy * 1.47 ) - ypos ) );
		    
				    if( distance < 30 )
                    {
					    actindex[ 0 ] = i;
					    actindex[ 1 ] = j;

                        if( actpt1[ 0 ] == -1 )
					    {
						    actpt1[ 0 ] = actindex[ 0 ];
						    actpt1[ 1 ] = actindex[ 1 ];

						    glColor3f( 0.0, 0.0, 1.0 );
						    glPointSize( 4.0 );
						    glBegin( GL_POINTS );
							    glVertex2f( gridptsx[ actpt1[ 0 ] ],
                                            gridptsy[ actpt1[ 1 ] ] );
						    glEnd();
                            xp = gridptsx[ actpt1[ 0 ] ];
                            yp = gridptsy[ actpt1[ 1 ] ];
                            baffleCreated = true;

						    return;
					    }
				    }
			    }
            }
        }
    }

    baffleCreated = false;
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::_checkForMouseHitsTwo( int xpos, int ypos )
{
    int zpos = 0;

    GLint viewport[ 4 ];
    GLdouble mvmatrix[ 16 ], projmatrix[ 16 ];

    GLdouble sx, sy, sz;

    //Prepare to Project
    glGetIntegerv( GL_VIEWPORT, viewport );
    glGetDoublev( GL_MODELVIEW_MATRIX, mvmatrix );
    glGetDoublev( GL_PROJECTION_MATRIX, projmatrix );

	for( int i = 0; i < 49; ++i )
    {
		for( int j = 0; j < 33; ++j )
        {
			if( i < 8 && j > 7 || i > 7 && j < 8 || i > 7 && j > 7 )
            {
			    if( i > 39 && j < 25 || i < 41 && j > 24 || i < 40 && j < 25 )
			    {
				    gluProject(	static_cast< GLdouble >( gridptsx[ i ] ),
		    				    static_cast< GLdouble >( gridptsx[ j ] ),
			    			    static_cast< GLdouble >( 0.0 ),
							    mvmatrix,
							    projmatrix,
							    viewport,
							    &sx,
							    &sy,
							    &sz );

				    double distance = ( ( sx - xpos ) * ( sx - xpos ) +
                        ( ( sy * 1.47 ) - ypos ) * ( ( sy * 1.47 ) - ypos ) );
		    
				    if( distance < 30 )
                    {
					    actindex[ 0 ] = i;
					    actindex[ 1 ] = j;

						actpt2[ 0 ] = actindex[ 0 ];
						actpt2[ 1 ] = actindex[ 1 ];
                        if( actpt2[ 0 ] == actpt1[ 0 ] ||
                            actpt2[ 1 ] == actpt1[ 1 ] )
                        {
						    glColor3f( 0.0, 0.0, 1.0 );
						    glPointSize( 4.0 );
						    glBegin( GL_POINTS );
							    glVertex2f( gridptsx[ actpt2[ 0 ] ],
                                            gridptsy[ actpt2[ 1 ] ] );
						    glEnd();
                            baffleCreated = true;

						    return;
                         }
                         else
                         {
                            actpt1[ 0 ] = -1;
						    actpt1[ 1 ] = -1;
                            baffleCreated = false;

                            return;
                         }
				    }
			    }
            }
        }
    }

    glColor3f( 0.0, 1.0, 0.0 );
	glPointSize( 4.0 );
	glBegin( GL_POINTS );
	    glVertex2f( gridptsx[ actindex[ 0 ] ], gridptsy[ actindex[ 1 ] ] );
	glEnd();
	actpt1[ 0 ] = -1;
	actpt1[ 1 ] = -1;
    baffleCreated = false;

	return;
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::_drawNewBaffle()
{
#ifndef __WXMAC__
    SetCurrent( *GetContext() );
#else
    SetCurrent();
#endif

    glLineWidth( 2.0 );
    glColor3f( 0.0, 0.0, 1.0 );
    glBegin( GL_LINES );
	    glVertex2f( gridptsx[ actpt1[ 0 ] ], gridptsy[ actpt1[ 1 ] ] );
	    glVertex2f( gridptsx[ actpt2[ 0 ] ], gridptsy[ actpt2[ 1 ] ] );
    glEnd();
    glFlush();
    
    SwapBuffers();

    actpt1[ 0 ] = -1;
    actpt1[ 1 ] = -1;
    actpt2[ 0 ] = -1;
    actpt2[ 1 ] = -1;
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::_removeBaffle( int x, int y, int direction, int length )
{
	int x2,y2;
	if( direction == 0 )
	{
		x2 = x + length;
		y2 = y;
	}
	else if( direction == 1 )
	{
		x2 = x - length;
		y2 = y;
	}
	else if( direction == 2 )
	{
		x2 = x;
		y2 = y + length;
	}
	else if( direction == 3 )
	{
		x2 = x;
		y2 = y - length;
	}

#ifndef __WXMAC__
	SetCurrent( *GetContext() );
#else
	SetCurrent();
#endif

	glPointSize( 4.0 );
	glLineWidth( 2.0 );
	glColor3f( 0.0, 1.0, 0.0 );
	glBegin( GL_POINTS );
		glVertex2f( gridptsx[ x ],  gridptsy[ y ] );
		glVertex2f( gridptsx[ x2 ], gridptsy[ y2 ] );
	glEnd();
	glBegin( GL_LINES );
		glVertex2f( gridptsx[ x ],  gridptsy[ y ] );
		glVertex2f( gridptsx[ x2 ], gridptsy[ y2 ] );
	glEnd();
	
    SwapBuffers();
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::_reDrawBaffle(
    int x, int y, int direction, int length, int index )
{
	int x2,y2;
	if( direction == 0 )
	{
		x2 = x + length;
		y2 = y;
	}
	else if( direction == 1 )
	{
		x2 = x - length;
		y2 = y;
	}
	else if( direction == 2 )
	{
		x2 = x;
		y2 = y + length;
	}
	else if( direction == 3 )
	{
		x2 = x;
		y2 = y - length;
	}

    if( length > 0 )
    {
	    glPointSize( 4.0 );
	    glLineWidth( 2.0 );
	    glColor3f( 0.0, 0.0, 1.0 );
	    glBegin( GL_POINTS );
		    glVertex2f( gridptsx[ x ],  gridptsy[ y ] );
		    glVertex2f( gridptsx[ x2 ], gridptsy[ y2 ] );
	    glEnd();
	    glBegin( GL_LINES );
		    glVertex2f( gridptsx[ x ],  gridptsy[ y ] );
		    glVertex2f( gridptsx[ x2 ], gridptsy[ y2 ] );
	    glEnd();
        AddBaffleLabel( gridptsx[ x ], gridptsy[ y ],
                        gridptsx[ x2 ], gridptsy[ y2 ], index );
    }
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::_onMouse( wxMouseEvent& event )
{
    if( event.GetEventType() == wxEVT_LEFT_DOWN )
    {
	    event.GetPosition( &xpos2, &ypos2 );
	    ypos2 = 600 - ypos2;
        xPoint = xpos2;
        yPoint = ypos2;

#ifndef __WXMAC__
	    SetCurrent( *GetContext() );
#else
        SetCurrent();
#endif

        _checkForMouseHitsOne( xpos2, ypos2 );

        SwapBuffers();
    }
    else if( event.m_leftDown && event.Dragging())
    {
        if( baffleCreated )
        {
            event.GetPosition( &xline, &yline );
            yline = 600 - yline;
            double x = xline;
            double y = yline;
            xLine = ( x / 400 ) - 1;
            yLine = ( y / 300 ) - 1;

#ifndef __WXMAC__
            SetCurrent( *GetContext() );
#else
            SetCurrent();
#endif

            glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
            _draw();
            for( int i = 0; i < m_dialog->GetNumBaffles(); ++i )
	        {
		        m_dialog->_reDrawBaff( i );	  
	        }

            glLineWidth( 2.0 );
            glColor3f( 0.0, 0.0, 1.0 );
            glBegin( GL_LINES );
                glVertex2f( xp, yp );
                glVertex2f( xLine, yLine );
            glEnd();
        
            SwapBuffers();
        }
    }
    else if( event.GetEventType() == wxEVT_LEFT_UP )
    {
        event.GetPosition( &xpos2, &ypos2 );
	    ypos2 = 600 - ypos2;
        xPoint = xpos2;
        yPoint = ypos2;

#ifndef __WXMAC__
	    SetCurrent( *GetContext() );
#else
        SetCurrent();
#endif

        _checkForMouseHitsTwo( xpos2, ypos2 );

        if(	actpt2[ 0 ] == actpt1[ 0 ] && actpt2[ 1 ] == actpt1[ 1 ] )
        {
            m_dialog->_rebuildActBaffSel();

            return;
        }

        if( baffleCreated )
        {
            m_dialog->_onAddBaff();
        }
        else
        {
            m_dialog->_rebuildActBaffSel();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::OnPaint( wxPaintEvent& WXUNUSED( event ) )
{
    wxPaintDC dc( this );

#ifndef __WXMAC__
    SetCurrent( *GetContext() );
#else
    SetCurrent();
#endif

    _draw();
    for( size_t i = 0; i < 7; ++i )
    {   
        x = m_dialog->GetStartX( i );
        y = m_dialog->GetStartY( i );
        direction = m_dialog->GetDirection( i );
        length = m_dialog->GetLength( i );
        _reDrawBaffle( x, y, direction, length, i );
    }

    SwapBuffers();
}
////////////////////////////////////////////////////////////////////////////////
wxGLContext* GL_Engine::GetContext()
{
    return m_glContext;
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::InletText()
{
    glColor3f( 0.0, 0.0, 0.0 );
    glRasterPos2f( 0.75, 0.7 );
    char* textString = "INLET";
    for( size_t i = 0; i < strlen( textString ); ++i )
    {
        glutBitmapCharacter( GLUT_BITMAP_TIMES_ROMAN_24, textString[ i ] );
    }

}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::OutletText()
{
    glColor3f( 0.0, 0.0, 0.0 );
    glRasterPos2f( -0.94, -0.75 );
    char* textString = "OUTLET";
    for( size_t i = 0; i < strlen( textString ); ++i )
    {
        glutBitmapCharacter( GLUT_BITMAP_TIMES_ROMAN_24, textString[ i ] );
    }
}
////////////////////////////////////////////////////////////////////////////////
void GL_Engine::AddBaffleLabel(
    float x1, float y1, float x2, float y2, int baffleNum )
{
    float xPos = 0;
    float yPos = 0;

    if( x2 > x1 )
    {
        xPos = x2 - ( ( x2 - x1 ) / 2 );
    }
    else if( x1 > x2 )
    {
        xPos = x1 - ( ( x1 - x2 ) / 2 );
    }
    else
    {
        xPos = x1;
    }

    if( y2 > y1 )
    {
        yPos = y2 - ( ( y2 - y1 ) / 2 );
    }
    else if( y1 > y2 )
    {
        yPos = y1 - ( ( y1 - y2 ) / 2 );
    }
    else
    {
        yPos = y1;
    }

    glColor3f( 0.0, 0.0, 0.0 );
    glRasterPos2f( xPos, yPos );
    
    std::ostringstream textString;
    textString << baffleNum + 1;
    std::string testing = textString.str();
    char* test = const_cast< char* >( testing.c_str() );

    for( size_t i = 0; i < strlen( test ); ++i )
    {
        glutBitmapCharacter( GLUT_BITMAP_TIMES_ROMAN_24, test[ i ] );
    }
}
////////////////////////////////////////////////////////////////////////////////
