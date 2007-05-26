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
* Date modified: $Date: 2007-02-06 16:35:58 -0600 (Tue, 06 Feb 2007) $
* Version:       $Rev: 6788 $
* Author:        $Author: jbkoch $
* Id:            $Id: CORBAServiceList.h 6788 2007-02-06 22:35:58Z jbkoch $
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/GUIPlugin/FindDialog.h"

#include <wx/stattext.h>
#include <wx/choice.h>
#include <wx/button.h>
#include <wx/dialog.h>
BEGIN_EVENT_TABLE(FindDialog,wxDialog)
	EVT_CLOSE(FindDialog::OnClose)
	EVT_BUTTON(ID_CANCELBUTTON,FindDialog::CancelButtonClick)
	EVT_BUTTON(ID_FINDBUTTON,FindDialog::FindButtonClick)
END_EVENT_TABLE()

FindDialog::FindDialog(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}

FindDialog::~FindDialog()
{
} 

void FindDialog::CreateGUIControls()
{
	SetTitle(wxT("Find"));
	SetIcon(wxNullIcon);
	SetSize(8,8,261,144);
	Center();

	UnitLabel = new wxStaticText(this, ID_UNITLABEL, wxT("Unit Operations"), wxPoint(4,22), wxDefaultSize, 0, wxT("UnitLabel"));
	UnitLabel->SetFont(wxFont(10, wxSWISS, wxNORMAL,wxBOLD, FALSE, wxT("Times New Roman")));

	wxArrayString arrayStringFor_WxChoice1;
	WxChoice1 = new wxChoice(this, ID_WXCHOICE1, wxPoint(96,19), wxSize(145,21), arrayStringFor_WxChoice1, 0, wxDefaultValidator, wxT("WxChoice1"));
	WxChoice1->SetSelection(-1);

	CancelButton = new wxButton(this, ID_CANCELBUTTON, wxT("Cancel"), wxPoint(165,57), wxSize(75,25), 0, wxDefaultValidator, wxT("CancelButton"));

	FindButton = new wxButton(this, ID_FINDBUTTON, wxT("Find"), wxPoint(86,57), wxSize(75,25), 0, wxDefaultValidator, wxT("FindButton"));
}

void FindDialog::OnClose(wxCloseEvent& /*event*/)
{
	Destroy();
}

void FindDialog::CancelButtonClick(wxCommandEvent& event)
{
	Destroy();
}

void FindDialog::FindButtonClick(wxCommandEvent& event)
{
	selectedModule = WxChoice1->GetString(WxChoice1->GetSelection());
	selectedModulePos = WxChoice1->GetSelection();
	Destroy();
}

void FindDialog::SetModuleList(std::vector< std::string > modules)
{
	for(int i = 0; i < (int)modules.size(); i++)
		WxChoice1->Insert( wxString( modules[i].c_str(), wxConvUTF8 ), i);
}

const char * FindDialog::GetSelectedModule()
{
	return selectedModule.mb_str();
}

int FindDialog::GetSelectedModulePos()
{
	return selectedModulePos;
}