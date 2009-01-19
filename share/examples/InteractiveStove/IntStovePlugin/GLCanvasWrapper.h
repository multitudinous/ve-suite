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

#ifndef GL_CANVAS_WRAPPER_H
#define GL_CANVAS_WRAPPER_H

// --- My Includes --- //
class GL_Engine;

// --- wxWidgets Includes --- //
class wxWindow;
class wxBoxSizer;

class GLCanvasWrapper
{
public:
    GLCanvasWrapper( wxWindow* parent, wxBoxSizer* sizer );

    ~GLCanvasWrapper();
    
    void DrawCanvas();

    void DrawNewBaffle();

    void RemoveBaffle( long int temp1,
                       long int temp2,
                       long int temp3,
                       long int temp4 );

    void RedrawBaffle( long int temp1,
                       long int temp2,
                       long int temp3,
                       long int temp4,
                       int index );

    float* GetGridInfo();
    int* GetGridPoints( int arrayNum );

private:
    GL_Engine* mGLEngine;
    float mGridInfo[ 4 ];
    int mGridPoint1[ 4 ];
    int mGridPoint2[ 4 ];
};

#endif //GL_CANVAS_WRAPPER_H
