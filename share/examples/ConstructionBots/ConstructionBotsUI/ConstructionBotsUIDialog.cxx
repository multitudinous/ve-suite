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

// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "ConstructionBotsUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/dialog.h>

using namespace bots;

BEGIN_EVENT_TABLE( ConstructionBotsUIDialog, wxDialog )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ConstructionBotsUIDialog::ConstructionBotsUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ConstructionBotsUIDialog::ConstructionBotsUIDialog(
    wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
:
UIDialog( static_cast< wxWindow* >( parent ),
          id,
          wxT( "ConstructionBots" ) )
{
    mServiceList = service;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
ConstructionBotsUIDialog::~ConstructionBotsUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool ConstructionBotsUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool ConstructionBotsUIDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::BuildGUI()
{
    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::ClearInstructions()
{
    mInstructions.clear();
    mCommandName.clear();
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsUIDialog::SendCommandsToXplorer()
{
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 

    for( size_t i = 0; i < mInstructions.size(); ++i )
    {
        command->AddDataValuePair( mInstructions.at( i ) );
    }

    command->SetCommandName( mCommandName );

    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
