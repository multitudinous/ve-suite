#ifndef ICONCHOOSER_H
#define ICONCHOOSER_H

#include "VE_Conductor/GUIPlugin/Plugin_base.h"
#include "VE_Installer/include/VEConfig.h"
#include <wx/wx.h>
#include <wx/frame.h>
#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/notebook.h>
#include <wx/panel.h>
#include <map>

#undef IconChooser_STYLE
#define IconChooser_STYLE wxCAPTION | wxSYSTEM_MENU | wxMINIMIZE_BOX | wxCLOSE_BOX

class VE_GUIPLUGINS_EXPORTS IconChooser : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		IconChooser(wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT("IconChooser"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = IconChooser_STYLE);
		virtual ~IconChooser();
		void WxButtonClick(wxCommandEvent& event);
		//void AppendList(const char * input);
		void SetPlugin( REI_Plugin * plugin);
		
	private:		
		std::map< int, std::string > iconPaths;
		wxTextCtrl * WxEdit;
		REI_Plugin * thePlugin;
		//wxChoice *WxChoice;
        //wxArrayString componentList;
		
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
};
#endif
