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
 * File:          $RCSfile: FluentTranslator.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

// Author: Jeremy Jarrell jarrell@csee.wvu.edu
//
// This is a portion of the GUI written for the CFD Translator developed on contract from DOE-NETL.

#include <wx/wx.h>
#include <wx/image.h>

#include <wx/progdlg.h>
#ifndef FLUENTTRANSLATOR_H
#define FLUENTTRANSLATOR_H


class FluentTranslator: public wxDialog {
public:
    // FluentTranslator::ids

    FluentTranslator(wxWindow* parent, int id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);
	
private:
    // FluentTranslator::methods
    void set_properties();
    void do_layout();

protected:
    // FluentTranslator::attributes
    wxStaticText* label_2;

	wxButton* OKButton;


	DECLARE_EVENT_TABLE()

	enum EVENTS
	{
		BUTTON_OK
	};

	
	void OnOKButton(wxCommandEvent& event);
	void OnCancelButton(wxCommandEvent& event);
	void OnCloseWindow(wxCommandEvent& event);
};


#endif // FLUENTTRANSLATOR_H
