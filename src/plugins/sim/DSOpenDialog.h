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
#ifndef DSOPENDIALOG_H
#define DSOPENDIALOG_H

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/dir.h>

#undef DSOpenDialog_STYLE
#define DSOpenDialog_STYLE wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

class DSOpenDialog : public wxDialog
{
    private:
        DECLARE_EVENT_TABLE();
		
    public:
        DSOpenDialog(wxWindow *parent, wxWindowID id = 1,
			const wxString &title = wxT("DynSim File"),
			const wxPoint& pos = wxDefaultPosition,
			const wxSize& size = wxDefaultSize,
			long style = DSOpenDialog_STYLE);
        
		virtual ~DSOpenDialog();
        void OKButtonClick(wxCommandEvent& event);
        void CancelButtonClick(wxCommandEvent& event);
        void SetPopulateFilenames( );
        wxString GetFilename( );

    private:
        wxStaticText *Label;
        wxButton *CancelButton;
        wxButton *OKButton;
        wxComboBox *ComboBox;
        wxArrayString arrayStringFor_ComboBox;

        void OnClose(wxCloseEvent& event);
        void CreateGUIControls();
};

#endif
