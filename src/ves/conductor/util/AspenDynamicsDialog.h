#ifndef __ASPENDYNAMICSDIALOG_h__
#define __ASPENDYNAMICSDIALOG_h__

#ifdef __BORLANDC__
	#pragma hdrstop
#endif

#ifndef WX_PRECOMP
	#include <wx/wx.h>
	#include <wx/dialog.h>
#else
	#include <wx/wxprec.h>
#endif

#include <wx/button.h>
#include <wx/grid.h>
#include <wx/sizer.h>

#undef AspenDynamicsDialog_STYLE
#define AspenDynamicsDialog_STYLE wxCAPTION | wxRESIZE_BORDER | wxSYSTEM_MENU | wxTHICK_FRAME | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxCLOSE_BOX

class AspenDynamicsDialog : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		AspenDynamicsDialog(wxWindow *parent, wxWindowID id = 1,
            const wxString &title = wxT("AspenDynamicsDialog"),
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize,
            long style = AspenDynamicsDialog_STYLE);

		virtual ~AspenDynamicsDialog();
		void CancelButtonClick(wxCommandEvent& event);
		void SetButtonClick(wxCommandEvent& event);
        void SetData( wxString name = wxT(""), wxString description = wxT(""),
            wxString value = wxT(""), wxString units = wxT("") );
        void UpdateSizes();
	
	private:
		wxButton *CancelButton;
		wxButton *SetButton;
		wxBoxSizer *WxBoxSizer1;
		wxGrid *WxGrid;
		wxFlexGridSizer *WxFlexGridSizer;
		
	private:
		enum
		{
			ID_CANCELBUTTON = 1005,
			ID_SETBUTTON = 1004,
			ID_WXGRID = 1002,
			ID_DUMMY_VALUE_
		};
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
};

#endif
