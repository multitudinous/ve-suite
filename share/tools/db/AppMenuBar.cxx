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

// --- VE-Suite Includes --- //
#include "AppMenuBar.h"
#include "AppFrame.h"
#include "DBAppEnums.h"

BEGIN_EVENT_TABLE( AppMenuBar, wxMenuBar )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppMenuBar::AppMenuBar( wxWindow* parent )
    :
    wxMenuBar(),
    m_appFrame( static_cast< AppFrame* >( parent ) ),
    m_fileMenu( NULL ),
    m_helpMenu( NULL )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
AppMenuBar::~AppMenuBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppMenuBar::CreateGUI()
{
	SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	m_fileMenu = new wxMenu();
	Append( m_fileMenu, wxT( "File" ) );
	
	m_helpMenu = new wxMenu();
	Append( m_helpMenu, wxT( "Help" ) );
}
////////////////////////////////////////////////////////////////////////////////
