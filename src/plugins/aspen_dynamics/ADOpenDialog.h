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
#ifndef ADOPENDIALOG_h
#define ADOPENDIALOG_h

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/dir.h>

#undef ADOpenDialog_STYLE
#define ADOpenDialog_STYLE

/*!\file ADOpenDialog.h
  Aspen Dynamics File Open Dialog
  */
/*!\class ves::conductor::ADOpenDialog
 * This class is the dialog for Aspen Dynamics files.
 */
class ADOpenDialog : public wxDialog
{
public:
    
    ///Constructor
    ADOpenDialog(wxWindow *parent, wxWindowID id = 1,
        const wxString &title = wxT("BKP/APW File"),
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize,
        long style = wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT |
        wxMINIMIZE_BOX | wxCLOSE_BOX);
    
    ///Destructor
    virtual ~ADOpenDialog();
    
    ///???
    void SetPopulateFilenames( );
    
    ///???
    wxString GetFilename( );

private:
    wxStaticText *Label;
    wxButton *CancelButton;
    wxButton *OKButton;
    wxComboBox *ComboBox;
    wxArrayString arrayStringFor_ComboBox;

    ///???
    void OnClose(wxCloseEvent& event);

    ///???
    void CreateGUIControls();

    ///???
    void OKButtonClick(wxCommandEvent& event);

    ///???
    void CancelButtonClick(wxCommandEvent& event);

    DECLARE_EVENT_TABLE();
};

#endif
