/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Apr 16 2008)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#ifndef __ScalarControlDialog__
#define __ScalarControlDialog__

#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/gdicmn.h>
#include <wx/font.h>
#include <wx/colour.h>
#include <wx/settings.h>
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/statline.h>
#include <wx/statbox.h>
#include <wx/dialog.h>

///////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
/// Class ScalarControlDialog
///////////////////////////////////////////////////////////////////////////////
namespace ves
{
namespace conductor
{
class ScalarControlDialog : public wxDialog 
{
    private:
    
    protected:
        wxTextCtrl* m_minTextCtrl;
        wxSlider* m_minSlider;
        wxStaticLine* m_staticline1;
        wxTextCtrl* m_maxTextControl;
        wxSlider* m_maxSlider;
        
        // Virtual event handlers, overide them in your derived class
        virtual void OnMinTextInput( wxCommandEvent& event ){ event.Skip(); }
        virtual void OnMinSlider( wxScrollEvent& event ){ event.Skip(); }
        virtual void OnMaxTextInput( wxCommandEvent& event ){ event.Skip(); }
        virtual void OnMaxSlider( wxScrollEvent& event ){ event.Skip(); }
        
    
    public:
        ScalarControlDialog( wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxT("Scalar Control"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxCLOSE_BOX|wxDEFAULT_DIALOG_STYLE|wxMAXIMIZE_BOX|wxMINIMIZE_BOX );
        ~ScalarControlDialog();
    
};
}
}
#endif //__ScalarControlDialog__
