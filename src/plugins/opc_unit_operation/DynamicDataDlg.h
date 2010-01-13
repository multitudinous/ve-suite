#ifndef __DYNAMICDATADLG_H__
#define __DYNAMICDATADLG_H__

#ifdef __BORLANDC__
	#pragma hdrstop
#endif

#ifndef WX_PRECOMP
	#include <wx/wx.h>
	#include <wx/dialog.h>
#else
	#include <wx/wxprec.h>
#endif

#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/button.h>

#include <ves/conductor/util/CORBAServiceList.h>

#undef DynamicDataDlg_STYLE
#define DynamicDataDlg_STYLE wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

class DynamicDataDlg : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		DynamicDataDlg(wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT("Dynamic Data"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = DynamicDataDlg_STYLE);
		virtual ~DynamicDataDlg();
		void closeButtonClick(wxCommandEvent& event);
		void setButtonClick(wxCommandEvent& event);
		void SetName( std::string name );
		void ReadValue( );
		void SetCORBAServiceList( ves::conductor::util::CORBAServiceList* servicelist );
	
	private:
		wxTextCtrl *WxEdit1;
		wxStaticText *WxStaticText2;
		wxButton *closeButton;
		wxButton *setButton;
		std::string compName;
		ves::conductor::util::CORBAServiceList* serviceList;
		wxTimer * m_timer;
		
	private:
		enum
		{
			ID_WXEDIT1 = 1005,
			ID_WXSTATICTEXT2 = 1004,
			ID_CLOSEBUTTON = 1002,
			ID_SETBUTTON = 1001,
			TIMER_ID = 1006,
			ID_DUMMY_VALUE_
		};
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
		void OnTimer( wxTimerEvent& event );

};

#endif
