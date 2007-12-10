/*************** <auto-copyright.pl BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
*************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef FINDDIALOG_H
#define FINDDIALOG_H

#include <ves/VEConfig.h>
#include <vector>
#include <string>

#include <wx/dialog.h>
class wxWindow;
//class wxDialog;
class wxStaticText;
class wxButton;
class wxString;
class wxSize;
class wxPoint;
class wxCommandEvent;
class wxChoice;
class wxString;

#undef FindDialog_STYLE
#define FindDialog_STYLE wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

namespace ves
{
namespace conductor
{
class VE_GUIPLUGINS_EXPORTS FindDialog : public wxDialog
{
public:
    FindDialog( wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT( "Untitled1" ), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = FindDialog_STYLE );
    virtual ~FindDialog();

    enum
    {
        ID_UNITLABEL = 1005,
        ID_WXCHOICE1 = 1004,
        ID_CANCELBUTTON = 1002,
        ID_FINDBUTTON = 1001,
        ID_DUMMY_VALUE_ //don't remove this value unless you have other enum values
    };

    void CancelButtonClick( wxCommandEvent& event );
    void FindButtonClick( wxCommandEvent& event );
    void SetModuleList( std::vector< std::string > );
    const char * GetSelectedModule();
    int GetSelectedModulePos();
    wxStaticText *UnitLabel;
    wxChoice *WxChoice1;
    wxButton *CancelButton;
    wxButton *FindButton;
    wxString selectedModule;
    int selectedModulePos;

private:
    void OnClose( wxCloseEvent& event );
    void CreateGUIControls();

    DECLARE_EVENT_TABLE()
};
}
}
#endif
