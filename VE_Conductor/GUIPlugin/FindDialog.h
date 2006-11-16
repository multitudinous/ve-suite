#ifndef FINDDIALOG_H
#define FINDDIALOG_H

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/choice.h>
#include <wx/button.h>
#include "VE_Installer/include/VEConfig.h"
#include <vector>
#include <string>

class wxWindow;
class wxDialog;
class wxButton;
class wxString;
class wxSize;
class wxPoint;
class wxCommandEvent;
class wxChoice;
class wxString;

#undef FindDialog_STYLE
#define FindDialog_STYLE wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

class VE_GUIPLUGINS_EXPORTS FindDialog : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		FindDialog(wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT("Untitled1"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = FindDialog_STYLE);
		virtual ~FindDialog();
		void CancelButtonClick(wxCommandEvent& event);
		void FindButtonClick(wxCommandEvent& event);
		void SetModuleList(std::vector< std::string >);
		const char * GetSelectedModule();
		int GetSelectedModulePos();
		wxStaticText *UnitLabel;
		wxChoice *WxChoice1;
		wxButton *CancelButton;
		wxButton *FindButton;
		wxString selectedModule;
		int selectedModulePos;
		
	private:
		enum
		{
			ID_UNITLABEL = 1005,
			ID_WXCHOICE1 = 1004,
			ID_CANCELBUTTON = 1002,
			ID_FINDBUTTON = 1001,
			ID_DUMMY_VALUE_ //don't remove this value unless you have other enum values
		};
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
};

#endif
