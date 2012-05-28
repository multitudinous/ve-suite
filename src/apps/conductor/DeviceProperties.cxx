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
#include <ves/conductor/util/CORBAServiceList.h>

#include "DeviceProperties.h"
#include "ConductorAppEnums.h"

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>

#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/stattext.h>
#include <wx/scrolwin.h>
#include <wx/listbox.h>
#include <wx/splitter.h>
#include <wx/button.h>
#include <wx/filename.h>

using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( DeviceProperties, wxDialog )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
DeviceProperties::DeviceProperties( wxWindow* parent )
    : wxDialog( parent, -1, _( "Device Interface" ), wxDefaultPosition, wxDefaultSize,
                ( wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX | wxMINIMIZE_BOX )&~wxSTAY_ON_TOP )
{
    device_splitter = NULL;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
DeviceProperties::~DeviceProperties()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceProperties::BuildGUI()
{
    wxBoxSizer* box_sizer_1 = new wxBoxSizer( wxVERTICAL );
    this->SetSizer( box_sizer_1 );

    wxBoxSizer* box_sizer_2 = new wxBoxSizer( wxHORIZONTAL );
    box_sizer_1->Add( box_sizer_2, 0, wxALIGN_LEFT | wxLEFT | wxTOP, 5 );

    wxStaticText* static_text_1 = new wxStaticText( this, wxID_STATIC, _( "Devices" ), wxDefaultPosition, wxDefaultSize, 0 );
    static_text_1->SetFont( wxFont( 9, wxDEFAULT, wxNORMAL, wxBOLD, false ) );
    box_sizer_2->Add( static_text_1, 0, wxALIGN_CENTER_VERTICAL | wxALL | wxADJUST_MINSIZE, 5 );

    box_sizer_2->Add( 75, 5, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    wxStaticText* static_text_2 = new wxStaticText( this, wxID_STATIC, _( "Properties" ), wxDefaultPosition, wxDefaultSize, 0 );
    static_text_2->SetFont( wxFont( 9, wxDEFAULT, wxNORMAL, wxBOLD, false ) );
    box_sizer_2->Add( static_text_2, 0, wxALIGN_CENTER_VERTICAL | wxALL | wxADJUST_MINSIZE, 5 );

    device_splitter = new wxSplitterWindow( this, DEVICEPROPERTIES_SPLITTERWINDOW, wxDefaultPosition, wxSize( 100, 100 ), wxNO_BORDER );
    device_splitter->SetMinimumPaneSize( 0 );

    wxString list_box_strings[] = {_( "KeyboardMouse" ), _( "Wand" )};
    wxListBox* list_box_1 = new wxListBox( device_splitter, DEVICEPROPERTIES_LISTBOX, wxDefaultPosition, wxDefaultSize, 2, list_box_strings, wxLB_SINGLE );
    list_box_1->SetStringSelection( _( "KeyboardMouse" ) );

    wxPanel* panel_trackball = new wxPanel( device_splitter, DEVICEPROPERTIES_TRACKBALL_PANEL, wxDefaultPosition, wxDefaultSize, wxSUNKEN_BORDER | wxTAB_TRAVERSAL );
    wxBoxSizer* box_sizer_3 = new wxBoxSizer( wxVERTICAL );
    panel_trackball->SetSizer( box_sizer_3 );

    wxBoxSizer* box_sizer_4 = new wxBoxSizer( wxHORIZONTAL );
    box_sizer_3->Add( box_sizer_4, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );

    device_splitter->SplitVertically( list_box_1, panel_trackball, 150 );
    box_sizer_1->Add( device_splitter, 1, wxGROW | wxLEFT | wxRIGHT, 5 );

    wxBoxSizer* box_sizer_5 = new wxBoxSizer( wxHORIZONTAL );
    box_sizer_1->Add( box_sizer_5, 0, wxALIGN_RIGHT, 5 );

    wxButton* button_ok = new wxButton( this, wxID_OK, _( "&OK" ), wxDefaultPosition, wxDefaultSize, 0 );
    box_sizer_5->Add( button_ok, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    wxButton* button_cancel = new wxButton( this, wxID_CANCEL, _( "&Cancel" ), wxDefaultPosition, wxDefaultSize, 0 );
    box_sizer_5->Add( button_cancel, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
}
////////////////////////////////////////////////////////////////////////////////
void DeviceProperties::SendCommandsToXplorer()
{
    //Build the command
    CommandPtr command( new Command() );
    command->SetCommandName( "DEVICE_PROPERTIES" );

    for( size_t i = 0; i < instructions.size(); i++ )
    {
        command->AddDataValuePair( instructions.at( i ) );
    }

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void DeviceProperties::ClearInstructions()
{
    instructions.clear();
}
////////////////////////////////////////////////////////////////////////////////
