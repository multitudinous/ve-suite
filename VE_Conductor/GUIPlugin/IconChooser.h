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
		//wxChoice *WxChoice;
        //wxArrayString componentList;
		
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();

      std::string ConvertUnicode( const wxChar* data )
      {
         std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
         return tempStr;
      }
};
#endif
