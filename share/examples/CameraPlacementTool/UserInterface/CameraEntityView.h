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

#ifndef CAMERA_ENTITY_VIEW_H
#define CAMERA_ENTITY_VIEW_H

// --- wxWidgets Includes --- //
#include <wx/glcanvas.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osgUtil
{
class SceneView; 
}

namespace cpt
{

class CameraEntityView : public wxGLCanvas
{
public:
    CameraEntityView( wxWindow *parent,
                      wxWindowID id = wxID_ANY,
                      const wxPoint& pos = wxDefaultPosition,
                      const wxSize& size = wxDefaultSize,
                      long style = 0,
                      const wxString& name = wxT( "Title" ) );

    virtual ~CameraEntityView();

    void OnPaint( wxPaintEvent& event );
    void OnSize( wxSizeEvent& event );
    void OnEraseBackground( wxEraseEvent& event );
    void OnEnterWindow( wxMouseEvent& event );

    void Render();

    osg::ref_ptr< osgUtil::SceneView > mSceneView;

protected:

private:
    DECLARE_EVENT_TABLE()
};

} //end cpt

#endif //CAMERA_ENTITY_VIEW_H
