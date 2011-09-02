#ifndef SWITCHDLG_H
#define SWITCHDLG_H

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/stattext.h>

#undef SwitchDlg_STYLE
#define SwitchDlg_STYLE wxCAPTION | wxSYSTEM_MENU | wxSTAY_ON_TOP | wxDIALOG_NO_PARENT | wxCLOSE_BOX

class SwitchDlg : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		SwitchDlg(wxWindow *parent,
            ves::conductor::util::CORBAServiceList* service,
            wxWindowID id = 1, const wxString &title = wxT("switch"),
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, long style = SwitchDlg_STYLE);
		virtual ~SwitchDlg();
		void SetButtonClick(wxCommandEvent& event);
		void CancelClick(wxCommandEvent& event);
		void SwitchBodyBrowseButtonClick(wxCommandEvent& event);
		void OnButtonBrowseButtonClick(wxCommandEvent& event);
		void OffButtonBrowseButtonClick(wxCommandEvent& event);
	
	private:
		wxButton *Cancel;
		wxButton *SetButton;
		wxTextCtrl *OffButtonBrowseBox;
		wxTextCtrl *OnButtonBrowseBox;
		wxTextCtrl *SwitchBodyBrowseBox;
		wxButton *OffButtonBrowseButton;
		wxButton *OnButtonBrowseButton;
		wxButton *SwitchBodyBrowseButton;
		wxStaticText *OffButtonLabel;
		wxStaticText *OnButtonLabel;
		wxStaticText *SwitchBodyLabel;
        ves::conductor::util::CORBAServiceList* mServiceList;
		
	private:
		enum
		{
			ID_CANCELBUTTON = 1011,
			ID_SETBUTTON = 1010,
			ID_OFFBUTTONBROWSEBOX = 1009,
			ID_ONBUTTONBROWSEBOX = 1008,
			ID_SWITCHBODYBROWSEBOX = 1007,
			ID_OFFBUTTONBROWSEBUTTON = 1006,
			ID_ONBUTTONBROWSEBUTTON = 1005,
			ID_SWITCHBODYBROWSEBUTTON = 1004,
			ID_OFFBUTTONLABEL = 1003,
			ID_ONBUTTONLABEL = 1002,
			ID_SWITCHBODYLABEL = 1001,
			ID_DUMMY_VALUE_ 
        };
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
};
#endif
