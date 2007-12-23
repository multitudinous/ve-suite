#include "GL_Engine.h"
#include "IntStoves_UI_Dialog.h"
#include <GL/glu.h>
#include <GL/gl.h>
#include "wx/wx.h"
#include "wx/sizer.h"
#include <iostream>

#include <ves/open/xml/DataValuePair.h>

BEGIN_EVENT_TABLE(GL_Engine, wxGLCanvas)
  EVT_MOUSE_EVENTS  (GL_Engine::_onMouse)
  EVT_PAINT         (GL_Engine::OnPaint)
END_EVENT_TABLE()
/*
#ifdef (__linux__)
#define _itoa _itoa
char* _itoa(int value, char* str, int radix);
#endif
*/

///////////////
//Constructor//
///////////////
GL_Engine::GL_Engine(wxWindow* parent, wxWindowID id, int* attrlist, const wxPoint& pos, const wxSize& size, long style, const wxString& name)
	:wxGLCanvas(parent, id, attrlist, pos, size, style, name)
{
    for (int i=0; i<2; i++)
    {
        actindex[i] = -1;
	    actpt1[i] = -1;
	    actpt2[i] = -1;
    }

    baffleCreated = false;

    gluOrtho2D(0.0, 53.0, 0.0, 39.0);

    m_glContext = new wxGLContext(this);

    m_dialog = dynamic_cast< IntStoves_UI_Dialog* >( parent );
}
///////////////////////////////////////////////////////////////////////////////
GL_Engine::~GL_Engine()
{
}
///////////////////////////////////////////////////////////////////////////////
void GL_Engine::_draw()
{  
  //gridptsx = new int[48];
  //gridptsy = new int[32];

	double scaleval = 0.002;
	double offsetx =  -0.95;
	double offsety =  -0.95;

    for (int i=0; i<49; i++)
    {
	    gridptsx[i] = (float)i / 25 + offsetx;
    }
    for (int j=0; j<33; j++)
    {
	    gridptsy[j] = (float)j / 17 + offsety;
    }

    glClearColor(1.0, 1.0, 1.0, 0.0);
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    glColor3f(0.0, 0.0, 0.0);

    glPointSize(4.0);
    glLineWidth(2.0);
    glColor3f(0.0, 1.0, 0.0);
    glBegin(GL_POINTS);

    for (int i=0; i<49; i++)
    {
	    for (int j=0; j<33; j++)
        {
		    if ( i < 8 && j > 7 || i > 7 && j < 8 || i > 7 && j > 7 )
            {
			    if ( i > 39 && j < 25 || i < 41 && j > 24 || i < 40 && j < 25)
                {
				    glVertex2f(gridptsx[i], gridptsy[j]);
                }
            }
        }
    }

    glEnd();
    glBegin(GL_LINES);
    for (int i=0; i<48; i++)
    {
	    for (int j=0; j<32; j++)
	    {
		    if ( i < 8 && j > 7 || i > 7 && j < 8 || i > 7 && j > 7)
            {
			    if ( i > 38 && j < 24 || i < 40 && j > 23 || i < 39 && j < 24)
			    {
				    glVertex2f(gridptsx[i], gridptsy[j]);
				    glVertex2f(gridptsx[i+1], gridptsy[j]);
				    glVertex2f(gridptsx[i], gridptsy[j]);
				    glVertex2f(gridptsx[i], gridptsy[j+1]);
				    if(i==47)
				    {
					    glVertex2f(gridptsx[i+1], gridptsy[j]);
					    glVertex2f(gridptsx[i+1], gridptsy[j+1]);
				    }
				    if(j==31)
				    {
					    glVertex2f(gridptsx[i], gridptsy[j+1]);
					    glVertex2f(gridptsx[i+1], gridptsy[j+1]);
				    }
				    if(i==39 && j > 23)
				    {
					    glVertex2f(gridptsx[i+1], gridptsy[j]);
					    glVertex2f(gridptsx[i+1], gridptsy[j+1]);
				    }
				    if(j==23 && i > 38)
				    {
					    glVertex2f(gridptsx[i], gridptsy[j+1]);
					    glVertex2f(gridptsx[i+1], gridptsy[j+1]);
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
///////////////////////////////////////////////////////////////////////////////
void GL_Engine::_checkForMouseHitsOne(int xpos, int ypos)
{
    int zpos = 0;

    GLint viewport[4];
    GLdouble mvmatrix[16], projmatrix[16];

    GLdouble sx,sy,sz;

    //Prepare to Project
    glGetIntegerv(GL_VIEWPORT, viewport);
    glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
    glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);

	for (int i=0; i<49; i++)
    {
		for (int j=0; j<33; j++)
        {
			if ( i < 8 && j > 7 || i > 7 && j < 8 || i > 7 && j > 7 )
            {
			    if ( i > 39 && j < 25 || i < 41 && j > 24 || i < 40 && j < 25)
			    {
				    gluProject(	(GLdouble) gridptsx[i],
		    				    (GLdouble) gridptsx[j],
			    			    (GLdouble) 0.0,
							    mvmatrix,
							    projmatrix,
							    viewport,
							    &sx,
							    &sy,
							    &sz);
				    double distance = ((sx - xpos)*(sx - xpos) + ((sy*1.47) - ypos)*((sy*1.47) - ypos));
		    
				    if ( distance < 30 )
                    {
					    actindex[0] = i;
					    actindex[1] = j;

                        if ( actpt1[0] == -1 )
					    {
						    actpt1[0] = actindex[0];
						    actpt1[1] = actindex[1];
						    glColor3f(0.0, 0.0, 1.0);
						    glPointSize(4.0);
						    glBegin(GL_POINTS);
							    glVertex2f(gridptsx[actpt1[0]], gridptsy[actpt1[1]]);
						    glEnd();
                            xp = gridptsx[actpt1[0]];
                            yp = gridptsy[actpt1[1]];
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
///////////////////////////////////////////////////////////////////////////////
void GL_Engine::_checkForMouseHitsTwo(int xpos, int ypos)
{
    int zpos = 0;

    GLint viewport[4];
    GLdouble mvmatrix[16], projmatrix[16];

    GLdouble sx,sy,sz;

    //Prepare to Project
    glGetIntegerv(GL_VIEWPORT, viewport);
    glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
    glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);

	for (int i=0; i<49; i++)
    {
		for (int j=0; j<33; j++)
        {
			if ( i < 8 && j > 7 || i > 7 && j < 8 || i > 7 && j > 7 )
            {
			    if ( i > 39 && j < 25 || i < 41 && j > 24 || i < 40 && j < 25)
			    {
				    gluProject(	(GLdouble) gridptsx[i],
		    				    (GLdouble) gridptsx[j],
			    			    (GLdouble) 0.0,
							    mvmatrix,
							    projmatrix,
							    viewport,
							    &sx,
							    &sy,
							    &sz);

				    double distance = ((sx - xpos)*(sx - xpos) + ((sy*1.47) - ypos)*((sy*1.47) - ypos));
		    
				    if ( distance < 30 )
                    {
					    actindex[0] = i;
					    actindex[1] = j;

						actpt2[0] = actindex[0];
						actpt2[1] = actindex[1];
                        if( actpt2[0] == actpt1[0] || actpt2[1] == actpt1[1] )
                        {
						    glColor3f(0.0, 0.0, 1.0);
						    glPointSize(4.0);
						    glBegin(GL_POINTS);
							    glVertex2f(gridptsx[actpt2[0]], gridptsy[actpt2[1]]);
						    glEnd();
                            baffleCreated = true;
						    return;
                         }
                         else
                         {
                            actpt1[0] = -1;
						    actpt1[1] = -1;
                            baffleCreated = false;
                            return;
                         }
				    }
			    }
            }
        }
    }                
    glColor3f(0.0, 1.0, 0.0);
	glPointSize(4.0);
	glBegin(GL_POINTS);
	    glVertex2f(gridptsx[actindex[0]], gridptsy[actindex[1]]);
	glEnd();
	actpt1[0] = -1;
	actpt1[1] = -1;
    baffleCreated = false;
	return;
}
///////////////////////////////////////////////////////////////////////////////
void GL_Engine::_drawNewBaffle()
{
    glLineWidth(2.0);
    glColor3f(0.0, 0.0, 1.0);
    glBegin(GL_LINES);
	    glVertex2f(gridptsx[actpt1[0]], gridptsy[actpt1[1]]);
	    glVertex2f(gridptsx[actpt2[0]], gridptsy[actpt2[1]]);
    glEnd();
    glFlush();

    actpt1[0] = -1;
    actpt1[1] = -1;
    actpt2[0] = -1;
    actpt2[1] = -1;
}

void GL_Engine::_removeBaffle(int x, int y, int direction, int length)
{
	int x2,y2;
	if ( direction == 0 )
	{
		x2 = x + length;
		y2 = y;
	}
	else if ( direction == 1 )
	{
		x2 = x - length;
		y2 = y;
	}
	else if ( direction == 2 )
	{
		x2 = x;
		y2 = y + length;
	}
	else if ( direction == 3 )
	{
		x2 = x;
		y2 = y - length;
	}

	glPointSize(4.0);
	glLineWidth(2.0);
	glColor3f(0.0, 1.0, 0.0);
	glBegin(GL_POINTS);
		glVertex2f(gridptsx[x], gridptsy[y]);
		glVertex2f(gridptsx[x2], gridptsy[y2]);
	glEnd();
	glBegin(GL_LINES);
		glVertex2f(gridptsx[x], gridptsy[y]);
		glVertex2f(gridptsx[x2], gridptsy[y2]);
	glEnd();
}

void GL_Engine::_reDrawBaffle(int x, int y, int direction, int length, int index)
{
	int x2,y2;
	if ( direction == 0 )
	{
		x2 = x + length;
		y2 = y;
	}
	else if ( direction == 1 )
	{
		x2 = x - length;
		y2 = y;
	}
	else if ( direction == 2 )
	{
		x2 = x;
		y2 = y + length;
	}
	else if ( direction == 3 )
	{
		x2 = x;
		y2 = y - length;
	}

    if( length > 0 )
    {
	    glPointSize(4.0);
	    glLineWidth(2.0);
	    glColor3f(0.0, 0.0, 1.0);
	    glBegin(GL_POINTS);
		    glVertex2f(gridptsx[x], gridptsy[y]);
		    glVertex2f(gridptsx[x2], gridptsy[y2]);
	    glEnd();
	    glBegin(GL_LINES);
		    glVertex2f(gridptsx[x], gridptsy[y]);
		    glVertex2f(gridptsx[x2], gridptsy[y2]);
	    glEnd();
        AddBaffleLabel(gridptsx[x], gridptsy[y], gridptsx[x2], gridptsy[y2], index);
    }
}

void GL_Engine::_onMouse(wxMouseEvent& event)
{
    if (event.GetEventType() == wxEVT_LEFT_DOWN )
    {
	    event.GetPosition(&xpos2, &ypos2);
	    ypos2 = 600 - ypos2;
        xPoint = xpos2;
        yPoint = ypos2;
	    SetCurrent( *GetContext() );
        _checkForMouseHitsOne( xpos2, ypos2 );

        SwapBuffers();
    }
    else if( event.m_leftDown && event.Dragging())
    {
        if( baffleCreated )
        {
            event.GetPosition(&xline, &yline);
            yline = 600 - yline;
            double x = xline;
            double y = yline;
            xLine = (x/400) - 1;
            yLine = (y/300) - 1;

            SetCurrent( *GetContext() );
            glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
            _draw();
            for ( int i=0; i<m_dialog->GetNumBaffles(); i++ )
	        {
		        m_dialog->_reDrawBaff(i);	  
	        }
            glLineWidth(2.0);
            glColor3f(0.0, 0.0, 1.0);
            glBegin(GL_LINES);
                glVertex2f(xp, yp);
                glVertex2f(xLine, yLine);
            glEnd();
        
            SwapBuffers();
        }
    }
    else if( event.GetEventType() == wxEVT_LEFT_UP )
    {
        event.GetPosition(&xpos2, &ypos2);
	    ypos2 = 600 - ypos2;
        xPoint = xpos2;
        yPoint = ypos2;
	    SetCurrent( *GetContext() );
        _checkForMouseHitsTwo( xpos2, ypos2 );

        if(	actpt2[0] == actpt1[0] && actpt2[1] == actpt1[1] )
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
///////////////////////////////////////////////////////////////////////////////
void GL_Engine::OnPaint(wxPaintEvent& WXUNUSED(event) )
{
    wxPaintDC dc(this);
    SetCurrent( *GetContext() );
    _draw();
    for( size_t i=0; i<7; ++i )
    {   
        x = m_dialog->GetStartX(i);
        y = m_dialog->GetStartY(i);
        direction = m_dialog->GetDirection(i);
        length = m_dialog->GetLength(i);
        _reDrawBaffle(x, y, direction, length, i);
    }
    SwapBuffers();
}
///////////////////////////////////////////////////////////////////////////////
wxGLContext* GL_Engine::GetContext()
{
    return m_glContext;
}
///////////////////////////////////////////////////////////////////////////////
void GL_Engine::InletText()
{
    glColor3f( 0.0, 0.0, 0.0 );
    glRasterPos2f(0.75f, 0.7f);
    char* string = "INLET";
    for ( int i = 0; i<(int)strlen(string); i++ )
    {
        glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, (int)string[i]);
    }
}
///////////////////////////////////////////////////////////////////////////////
void GL_Engine::OutletText()
{
    glColor3f( 0.0, 0.0, 0.0 );
    glRasterPos2f(-0.94f, -0.75f);
    char* string = "OUTLET";
    for ( int i = 0; i<(int)strlen(string); i++ )
    {
        glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, (int)string[i]);
    }
}
///////////////////////////////////////////////////////////////////////////////
void GL_Engine::AddBaffleLabel(float x1, float y1, float x2, float y2, int baffleNum)
{
    float xPos = 0;
    float yPos = 0;

    if( x2 > x1 )
    {
        xPos = x2 - ((x2 - x1) / 2);
    }
    else if( x1 > x2)
    {
        xPos = x1 - ((x1 - x2) / 2);
    }
    else
    {
        xPos = x1;
    }
    if( y2 > y1 )
    {
        yPos = y2 - ((y2 - y1) / 2);
    }
    else if( y1 > y2 )
    {
        yPos = y1 - ((y1 - y2) / 2);
    }
    else
    {
        yPos = y1;
    }

    glColor3f( 0.0, 0.0, 0.0 );
    glRasterPos2f(xPos, yPos);
    
    std::ostringstream string;
    string << (baffleNum+1);
    std::string testing = string.str();
    char* test = const_cast<char*>(testing.c_str());

    for ( int i = 0; i<(int)strlen(test); i++ )
    {
        glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, (int)test[i]);
    } 
}
