#ifndef __ASPENDYNAMICSDIALOG_h__
#define __ASPENDYNAMICSDIALOG_h__

#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/VEConfig.h>

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/grid.h>
#include <wx/sizer.h>

#include <vector>

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
        void SetComponentName( wxString name );
        void SetServiceList(
            ves::conductor::util::CORBAServiceList * serviceList );
        wxString CompName;
        ves::conductor::util::CORBAServiceList * ServiceList;
	
	private:
		wxButton *CancelButton;
		wxButton *SetButton;
		wxBoxSizer *WxBoxSizer1;
		wxGrid *WxGrid;
		wxFlexGridSizer *WxFlexGridSizer;
        std::vector< int > rowsChanged;
		
        std::string ConvertUnicode( const wxChar* data )
        {
            std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
            return tempStr;
        }
    
    
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
        void WxGridCellChange(wxGridEvent& event);
};

#endif
