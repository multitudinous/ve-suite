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

// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "CoinFunnelUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/dialog.h>

BEGIN_EVENT_TABLE( CoinFunnelUIDialog, wxDialog )
EVT_BUTTON( OK_BUTTON, CoinFunnelUIDialog::OnOK )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
CoinFunnelUIDialog::CoinFunnelUIDialog( wxWindow* parent,
                                        int id,
                                        ves::conductor::util::CORBAServiceList* service,
                                        std::string* portNumber )
:
UIDialog( ( wxWindow * )parent, id, _( "CoinFunnel" ) ), p_portNumber( portNumber )
{
    serviceList = service;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
CoinFunnelUIDialog::~CoinFunnelUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CoinFunnelUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool CoinFunnelUIDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::BuildGUI()
{
    SetForegroundColour( wxColour( 255, 255, 255 ) );
    SetBackgroundColour( wxColour( 0, 0, 0 ) );

    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::UpdateGUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::OnOK( wxCommandEvent& event )
{
    Close( true );
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::ClearInstructions()
{
    instructions.clear();
    command_name.clear();
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUIDialog::SendCommandsToXplorer()
{
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );

    for( size_t i = 0; i < instructions.size(); i++ )
    {
        command->AddDataValuePair( instructions.at( i ) );
    }

    command->SetCommandName( command_name );

    serviceList->SendCommandStringToXplorer( command );

}
////////////////////////////////////////////////////////////////////////////////
