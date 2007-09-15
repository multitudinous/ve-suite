/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 *************** <auto-copyright.pl END do not edit this line> **************/
#ifndef ICONCHOOSER_H
#define ICONCHOOSER_H

#include "VE_Installer/include/VEConfig.h"

#include <wx/frame.h>
#include <wx/dirdlg.h>
#include <wx/menu.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/notebook.h>
#include <wx/panel.h>
#include <wx/bmpbuttn.h>
#include <wx/choice.h>
#include <wx/scrolwin.h>
#include <map>
#include <string>

#undef IconChooser_STYLE
#define IconChooser_STYLE wxCAPTION | wxSYSTEM_MENU | wxMINIMIZE_BOX | wxCLOSE_BOX

class UIPluginBase;

class VE_GUIPLUGINS_EXPORTS IconChooser : public wxFrame
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		IconChooser(wxWindow *parent, /*std::string path,*/ wxWindowID id = 1, const wxString &title = wxT("IconChooser"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = IconChooser_STYLE);
		virtual ~IconChooser();
		void WxButtonClick(wxCommandEvent& event);
		void okButtonClick(wxCommandEvent& event);
		void cancelButtonClick(wxCommandEvent& event);
		void IconDirectoryClick(wxCommandEvent& event);
		//void AppendList(const char * input);
		void SetPlugin( UIPluginBase * plugin);
		void AddIconsDir(wxString directory);
		
	private:		
		std::map< int, std::string > iconPaths;
		wxTextCtrl* WxEdit;
		UIPluginBase* thePlugin;
		//wxString directory;
		wxButton * okButton;
		wxButton * cancelButton;
		wxDirDialog *WxDirDialog;
		wxMenuBar *WxMenuBar1;
		wxNotebook * WxNotebook;
		wxPanel * WxPanel;
		wxChoice *WxChoice;
		wxArrayString choices;
		wxWindow * networkFrame;
        //wxArrayString componentList;
		int maxRows;
		
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();

      std::string ConvertUnicode( const wxChar* data )
      {
         std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
         return tempStr;
      }
};
//Form to get the function for aspen plus icons
//GetVESuite_Valve_Valve_VALVE1
#define GET_ICON_STREAM(name)  \
    name();

#endif
