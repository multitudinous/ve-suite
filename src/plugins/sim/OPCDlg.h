/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#ifndef OPCDLG_H
#define OPCDLG_H

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/listbox.h>
#include <vector>
#include <string>
#include "DSPlugin.h"

class wxMenu;

namespace ves
{
namespace conductor
{

/*!\file OPCDlg.h
  The OPC variable dialog
  */
/*!\class ves::conductor::OPCDlg
 * This class is used to create a dialog of all available OPC variables.
 */
class OPCDlg : public wxDialog
{       
public:

    ///Constructor
    OPCDlg(wxWindow *parent, wxWindowID id = 1,
        const wxString &title = wxT("OPCDialog"),
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize,
        long style = wxDIALOG_NO_PARENT | wxCLOSE_BOX | wxCAPTION |
        wxCLOSE_BOX | wxSYSTEM_MENU);

    ///Destructor
    virtual ~OPCDlg();
    
    ///???
    void SetParentPlugin( DSPlugin * parent );

private:    
    wxButton *WxButton4;
    wxButton *WxButton3;
    wxButton *WxButton2;
    wxButton *WxButton1;
    wxListBox *WxListBox3;
    wxListBox *WxListBox1;
    wxArrayString m_availableVariables;
    wxArrayString m_selectedVariables;
    DSPlugin * m_parentPlugin;
    
    enum
    {
        ID_WXBUTTON4 = 1008,
        ID_WXBUTTON3 = 1007,
        ID_WXBUTTON2 = 1006,
        ID_WXBUTTON1 = 1004,
        ID_WXLISTBOX3 = 1003,
        ID_WXLISTBOX1 = 1001,
        ID_DUMMY_VALUE_
    };
    
    ///???
    void OnClose(wxCloseEvent& event);
    
    ///???
    void CreateGUIControls();
    
    ///???
    void OnSaveButton( wxCommandEvent& event );
    
    ///???
    void OnCancelButton( wxCommandEvent& event );
    
    ///???
    void OnAddButton( wxCommandEvent& event );
    
    ///???
    void OnRemoveButton( wxCommandEvent& event );
    
    ///???
    bool SearchArrayList( wxArrayString arrayList, wxString entry );

    DECLARE_EVENT_TABLE();
};

}
}

#endif
