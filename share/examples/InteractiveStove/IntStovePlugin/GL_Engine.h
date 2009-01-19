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

#ifndef GL_ENGINE_H
#define GL_ENGINE_H

// --- My Includes --- //
class IntStoves_UI_Dialog;

// --- wxWidgets Includes --- //
#include <wx/glcanvas.h>

class GL_Engine : public wxGLCanvas
{
public:
    GL_Engine( wxWindow* parent,
               int id,
               int* attrlist,
               const wxPoint& pos,
               const wxSize& size,
               long style,
               const wxString& name );

    virtual ~GL_Engine();

    void _draw();
    void _checkForMouseHitsOne( int x, int y );
    void _checkForMouseHitsTwo( int x, int y );
    void _drawNewBaffle();
    void _removeBaffle( int, int, int, int );
    void _reDrawBaffle( int, int, int, int, int index );
    void _onMouse(wxMouseEvent& event);
    void OnPaint(wxPaintEvent& event);
    void InletText();
    void OutletText();
    void AddBaffleLabel( float x1, float y1, float x2, float y2, int baffleNum );

    wxGLContext* GetContext();
    wxGLContext* m_glContext;

    IntStoves_UI_Dialog* m_dialog;

    int x, y , direction, length;

    float gridptsx[ 49 ];
    float gridptsy[ 33 ];
    int actindex[ 2 ];
    int actpt1[ 2 ];
    int actpt2[ 2 ];

    int xpos2;
    int ypos2;
    int xline;
    int yline;

    int xPoint;
    int yPoint;
    double xLine;
    double yLine;

    double xp;
    double yp;

    bool baffleCreated;

    DECLARE_EVENT_TABLE();

};

#endif //GL_ENGINE_H
