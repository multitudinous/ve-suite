/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *   - National Energy Technology Laboratory, www.netl.doe.gov
 *   - West Virginia Virtual Environments Laboratory, wvvel.csee.wvu.edu
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
 * File:          $RCSfile: FluentTranslator.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

// Author: Jeremy Jarrell jarrell@csee.wvu.edu
//
// This is a portion of the GUI written for the CFD Translator developed on contract from DOE-NETL.

#include "FluentTranslator.h"


FluentTranslator::FluentTranslator(wxWindow* parent, int id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
    // FluentTranslator::FluentTranslator
    label_2 = new wxStaticText(this, -1, wxT("This is a placeholder for a Fluent file translator."), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
	OKButton = new wxButton(this, BUTTON_OK, wxT("OK"));
	
	set_properties();
    do_layout();
}


void FluentTranslator::set_properties()
{
    // FluentTranslator::set_properties
    SetTitle(wxT("Fluent File Translator"));
    SetSize(wxSize(400, 300));
	OKButton->SetToolTip(wxT("OK"));

	// Set icon
	wxIcon icon;
    icon.CopyFromBitmap(wxBitmap(wxT("res/vel.ico"), wxBITMAP_TYPE_ANY));
    SetIcon(icon);
}


void FluentTranslator::do_layout()
{
    // FluentTranslator::do_layout
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(20, 100, 0, 0, 0);
    sizer_1->Add(label_2, 0, wxALIGN_CENTER_HORIZONTAL, 0);
	sizer_1->Add(20, 25, 0, 0, 0);
	sizer_1->Add(OKButton, 0, wxALIGN_CENTER_HORIZONTAL, 0);
    sizer_1->Add(20, 100, 0, 0, 0);
    SetAutoLayout(true);
    SetSizer(sizer_1);
    Layout();
}

BEGIN_EVENT_TABLE(FluentTranslator, wxDialog)
	EVT_BUTTON	(BUTTON_OK, FluentTranslator::OnOKButton)
	EVT_BUTTON	(wxID_CANCEL, FluentTranslator::OnCancelButton)
END_EVENT_TABLE()

void FluentTranslator::OnOKButton(wxCommandEvent& event)
{
	OnOK(event);
	Destroy();
}

void FluentTranslator::OnCancelButton(wxCommandEvent& event)
{
	OnCancel(event);
	Destroy();
}
