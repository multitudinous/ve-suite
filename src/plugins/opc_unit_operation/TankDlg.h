#ifndef TANKDLG_H
#define TANKDLG_H

#include <wx/wx.h>
#include <wx/dialog.h>

#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/stattext.h>

#undef TankDlg_STYLE
#define TankDlg_STYLE wxCAPTION | wxSYSTEM_MENU | wxSTAY_ON_TOP | wxDIALOG_NO_PARENT | wxCLOSE_BOX

class TankDlg : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		TankDlg(wxWindow *parent,
            ves::conductor::util::CORBAServiceList* service,
            wxWindowID id = 1, const wxString &title = wxT("Tank"),
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, long style = TankDlg_STYLE);
		virtual ~TankDlg();
		void SetButtonClick(wxCommandEvent& event);
		void CancelClick(wxCommandEvent& event);
		void TankBrowseButtonClick(wxCommandEvent& event);
	
	private:
		wxButton *Cancel;
		wxButton *SetButton;
		wxTextCtrl *TankBrowseBox;
		wxButton *TankBrowseButton;
		wxStaticText *TankLabel;
        ves::conductor::util::CORBAServiceList* mServiceList;
    std::string ConvertUnicode( const wxChar* data )
        {
                std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }


	private:
		enum
		{
			ID_CANCELBUTTON = 1011,
			ID_SETBUTTON = 1010,
			ID_TANKBROWSEBOX = 1007,
			ID_TANKBROWSEBUTTON = 1004,
			ID_TANKLABEL = 1001,
			ID_DUMMY_VALUE_
		};
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
};

#endif
