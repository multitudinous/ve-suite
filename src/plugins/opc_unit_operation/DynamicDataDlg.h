///-----------------------------------------------------------------
///
/// @file      DynamicDataDlg.h
/// @author    tjordan
/// Created:   12/16/2009 10:58:31 AM
/// @section   DESCRIPTION
///            DynamicDataDlg class declaration
///
///------------------------------------------------------------------

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

//Do not add custom headers between 
//Header Include Start and Header Include End.
//wxDev-C++ designer will remove them. Add custom headers after the block.
////Header Include Start
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/button.h>
////Header Include End
#include <ves/conductor/util/CORBAServiceList.h>

////Dialog Style Start
#undef DynamicDataDlg_STYLE
#define DynamicDataDlg_STYLE wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX
////Dialog Style End

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
		//Do not add custom control declarations between 
		//GUI Control Declaration Start and GUI Control Declaration End.
		//wxDev-C++ will remove them. Add custom code after the block.
		////GUI Control Declaration Start
		wxTextCtrl *WxEdit1;
		wxStaticText *WxStaticText2;
		wxButton *closeButton;
		wxButton *setButton;
		////GUI Control Declaration End
		std::string compName;
		ves::conductor::util::CORBAServiceList* serviceList;
		wxTimer * m_timer;
		
	private:
		//Note: if you receive any error with these enum IDs, then you need to
		//change your old form code that are based on the #define control IDs.
		//#defines may replace a numeric value for the enum names.
		//Try copy and pasting the below block in your old form header files.
		enum
		{
			////GUI Enum Control ID Start
			ID_WXEDIT1 = 1005,
			ID_WXSTATICTEXT2 = 1004,
			ID_CLOSEBUTTON = 1002,
			ID_SETBUTTON = 1001,
			TIMER_ID = 1006,
			////GUI Enum Control ID End
			ID_DUMMY_VALUE_ //don't remove this value unless you have other enum values
		};
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
		void OnTimer( wxTimerEvent& event );

};

#endif
