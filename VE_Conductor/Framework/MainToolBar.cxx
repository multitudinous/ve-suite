/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2007-05-09 14:51:35 -0500 (Wed, 09 May 2007) $
 * Version:       $Rev: 7579 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: MainToolBar.cxx 7579 2007-05-09 19:51:35Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include "VE_Conductor/Framework/MainToolBar.h"

#include "VE_Conductor/xpm/selection32x32.xpm"
#include "VE_Conductor/xpm/navigation32x32.xpm"


////////////////////////////////////////////////////////////////////////////////
MainToolBar::MainToolBar( wxWindow* parent )
:
wxToolBar( parent, wxWindowID( -1 ), wxPoint( wxDefaultPosition ), wxSize( wxDefaultSize ), long( wxTB_HORIZONTAL | wxNO_BORDER ), wxString( "Main ToolBar" ) )
{
    this->CreateMainToolBar();
}
////////////////////////////////////////////////////////////////////////////////
MainToolBar::~MainToolBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::CreateMainToolBar()
{

    this->SetBackgroundColour( wxColour( 255, 255, 255 ) );
    this->SetToolBitmapSize( wxSize( 32, 32 ) );

    wxBitmap navigation_bitmap( navigation32x32_xpm );
    this->AddTool( NAVIGATION_MODE, _( "" ), navigation_bitmap, _( "Navigation" ), wxITEM_RADIO );
    wxBitmap selection_bitmap( selection32x32_xpm );
    this->AddTool( SELECTION_MODE, _( "" ), selection_bitmap, _( "Selection" ), wxITEM_RADIO );
    this->AddSeparator();
    this->Realize();

}
////////////////////////////////////////////////////////////////////////////////
