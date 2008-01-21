/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 *************** <auto-copyright.rb END do not edit this line> **************/
#ifndef QUERYINPUTSDLG_H
#define QUERYINPUTSDLG_H

//#ifdef __BORLANDC__
// #pragma hdrstop
//#endif

#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/listbox.h>
#include <ves/VEConfig.h>

class wxWindow;
class wxDialog;
class wxListBox;
class wxButton;
class wxString;
class wxSize;
class wxPoint;
class wxCommandEvent;

#undef QueryInputsDlg_STYLE
#define QueryInputsDlg_STYLE wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

namespace ves
{
namespace conductor
{
class VE_GUIPLUGINS_EXPORTS QueryInputsDlg : public wxDialog
{
public:
    QueryInputsDlg( wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT( "Query Inputs" ), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = QueryInputsDlg_STYLE );
    virtual ~QueryInputsDlg();

    enum
    {
        ID_WXSTATICTEXT2 = 1011,
        ID_WXSTATICTEXT1 = 1010,
        ID_WXBUTTON4 = 1009,
        ID_WXBUTTON3 = 1008,
        ID_WXBUTTON2 = 1006,
        ID_WXBUTTON1 = 1005,
        ID_WXLISTBOX2 = 1004,
        ID_WXLISTBOX1 = 1002,
        ID_DUMMY_VALUE_
    };

    void WxButton1Click( wxCommandEvent& event );
    void WxButton2Click( wxCommandEvent& event );
    void WxButton3Click( wxCommandEvent& event );
    void WxButton4Click( wxCommandEvent& event );
    void AppendList( const char * );
    bool IsSubmit();
    wxString GetDataString( int );
    int GetDataSize();
    wxStaticText *WxStaticText2;
    wxStaticText *WxStaticText1;
    wxButton *WxButton4;
    wxButton *WxButton3;
    wxButton *WxButton2;
    wxButton *WxButton1;
    wxListBox *WxListBox2;
    wxListBox *WxListBox1;
    bool submit;

private:
    void OnClose( wxCloseEvent& event );
    void CreateGUIControls();

    DECLARE_EVENT_TABLE();
};
}
}
#endif
