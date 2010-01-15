#ifndef __OPCDLG_H__
#define __OPCDLG_H__

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/listbox.h>
#include <vector>
#include <string>
#include "SDPlugin.h"

#undef OPCDlg_STYLE
#define OPCDlg_STYLE wxDIALOG_NO_PARENT | wxCLOSE_BOX | wxCAPTION | wxCLOSE_BOX | wxSYSTEM_MENU

class wxMenu;

namespace ves
{
namespace conductor
{
class OPCDlg : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		OPCDlg(wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT("OPCDialog"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = OPCDlg_STYLE);
		virtual ~OPCDlg();
	
	private:
		wxButton *WxButton4;
		wxButton *WxButton3;
		wxButton *WxButton2;
		wxButton *WxButton1;
		wxListBox *WxListBox3;
		wxListBox *WxListBox1;
		wxArrayString m_availableVariables;
		wxArrayString m_selectedVariables;
		SDPlugin * m_parentPlugin;
		
	private:
		enum
		{
			ID_WXBUTTON4 = 1008,
			ID_WXBUTTON3 = 1007,
			ID_WXBUTTON2 = 1006,
			ID_WXBUTTON1 = 1004,
			ID_WXLISTBOX3 = 1003,
			ID_WXLISTBOX1 = 1001,
			ID_DUMMY_VALUE_
		};
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
		void OnSaveButton( wxCommandEvent& event );
		void OnCancelButton( wxCommandEvent& event );
		void OnAddButton( wxCommandEvent& event );
		void OnRemoveButton( wxCommandEvent& event );
		bool SearchArrayList( wxArrayString arrayList, wxString entry );

	public:
		//void PopulateLists( std::vector< std::string > list, wxArrayString *selected);
		void SetParentPlugin( SDPlugin * parent );
};
}
}

#endif
