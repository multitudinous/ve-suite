#ifndef VALVEDLG_H
#define VALVEDLG_H

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/stattext.h>

#undef ValveDlg_STYLE
#define ValveDlg_STYLE wxCAPTION | wxSYSTEM_MENU | wxSTAY_ON_TOP | wxDIALOG_NO_PARENT | wxCLOSE_BOX

class ValveDlg : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		ValveDlg( wxWindow *parent,
            ves::conductor::util::CORBAServiceList* service,
            wxWindowID id = 1, const wxString &title = wxT("valve"),
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize,
            long style = ValveDlg_STYLE );
		virtual ~ValveDlg();
		void SetButtonClick(wxCommandEvent& event);
		void CancelClick(wxCommandEvent& event);
		void ValveBodyBrowseButtonClick(wxCommandEvent& event);
		void StemBrowseButtonClick(wxCommandEvent& event);
		void HandWheelBrowseButtonClick(wxCommandEvent& event);
	
	private:
		wxButton *Cancel;
		wxButton *SetButton;
		wxTextCtrl *StemBrowseBox;
		wxTextCtrl *HandWheelBrowseBox;
		wxTextCtrl *ValveBodyBrowseBox;
		wxButton *StemBrowseButton;
		wxButton *HandWheelBrowseButton;
		wxButton *ValveBodyBrowseButton;
		wxStaticText *StemLabel;
		wxStaticText *HandWheelLabel;
		wxStaticText *ValveBodyLabel;
        ves::conductor::util::CORBAServiceList* mServiceList;
		
	private:
		enum
		{
			ID_CANCELBUTTON = 1011,
			ID_SETBUTTON = 1010,
			ID_STEMBROWSEBOX = 1009,
			ID_HANDWHEELBROWSEBOX = 1008,
			ID_VALVEBODYBROWSEBOX = 1007,
			ID_STEMBROWSEBUTTON = 1006,
			ID_HANDWHEELBROWSEBUTTON = 1005,
			ID_VALVEBODYBROWSEBUTTON = 1004,
			ID_STEMLABEL = 1003,
			ID_HANDWHEELLABEL = 1002,
			ID_VALVEBODYLABEL = 1001,
			ID_DUMMY_VALUE_
		};
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
};

#endif
