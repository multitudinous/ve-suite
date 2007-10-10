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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// For compilers that supports precompilation , includes "wx/wx.h"
#include "wx/wx.h"
#include "toolBarApp.h"
#include "makeFrame.h"
//#include "wx/log.h"

IMPLEMENT_APP(toolBarApp)

bool toolBarApp::OnInit()
{
       
	makeFrame *frame = new makeFrame( "Convert", 100, 100, 400, 300 );
	
	frame->CreateToolBar( wxNO_BORDER | wxTB_HORIZONTAL, -1, 
			wxString("toolBar") );
	frame->GetToolBar()->SetMargins( 2, 2 );
	//--------GET TOOLBAR ADDRESS IN THE FRAME
	wxToolBar* toolBar = frame->GetToolBar();
	//--------ADD TOOLBAR TO THE FRAME
	frame->InitToolbar( toolBar );
	//--------CREATE STATUSBAR
	frame->CreateStatusBar( true );
	frame->Show( true );
	SetTopWindow(frame);
       return true;
}

BEGIN_EVENT_TABLE( makeFrame, wxFrame )	
	EVT_MENU( MENU_OPEN, makeFrame::onFileOpen )
	EVT_MENU( MENU_CONV_ASCII, makeFrame::onFileConvAscii )
	EVT_MENU( MENU_CONV_BINARY, makeFrame::onFileConvBinary )		
	EVT_MENU( MENU_MAK_VTK_SURF, makeFrame::onFileMakVtkSurf )
	EVT_MENU( MENU_DEL, makeFrame::onDel )	
	EVT_MENU( MENU_QT, makeFrame::onFileQuit )
	EVT_MENU( MENU_ABOUT, makeFrame::onAbout )
END_EVENT_TABLE()

BEGIN_EVENT_TABLE( makePopupDialog, wxDialog )
   EVT_RADIOBOX( RADIO_BOX, makePopupDialog::onRadioBox )
END_EVENT_TABLE()
