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
#ifndef __DYNAMICDATADLG_H__
#define __DYNAMICDATADLG_H__

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <ves/conductor/util/CORBAServiceList.h>

class DynamicDataDlg : public wxDialog
{
public:
    ///Constructor
    DynamicDataDlg(wxWindow *parent, wxWindowID id = 1,
        const wxString &title = wxT("Dynamic Data"),
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize,
        long style = wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT |
        wxMINIMIZE_BOX | wxCLOSE_BOX);

    ///Destructor
    virtual ~DynamicDataDlg();

    ///???
    void SetName( std::string name );

    ///???
    void ReadValue( );

    ///???
    void SetCORBAServiceList(
        ves::conductor::util::CORBAServiceList* servicelist );

private:
    wxTextCtrl *WxEdit1;
    wxStaticText *WxStaticText2;
    wxButton *closeButton;
    wxButton *setButton;
    std::string compName;
    ves::conductor::util::CORBAServiceList* serviceList;
    wxTimer * m_timer;
    
    enum
    {
        ID_WXEDIT1 = 1005,
        ID_WXSTATICTEXT2 = 1004,
        ID_CLOSEBUTTON = 1002,
        ID_SETBUTTON = 1001,
        TIMER_ID = 1006,
        ID_DUMMY_VALUE_
    };

    ///???
    void OnClose(wxCloseEvent& event);

    ///???
    void CreateGUIControls();

    ///???
    void OnTimer( wxTimerEvent& event );

    ///???
    void closeButtonClick(wxCommandEvent& event);

    ///???
    void setButtonClick(wxCommandEvent& event);

    DECLARE_EVENT_TABLE();
};

#endif
