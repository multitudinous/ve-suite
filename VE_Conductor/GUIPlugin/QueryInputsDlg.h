#ifndef QUERYINPUTSDLG_H
#define QUERYINPUTSDLG_H

//#ifdef __BORLANDC__
//	#pragma hdrstop
//#endif

#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/listbox.h>
#include "VE_Installer/include/VEConfig.h"

class wxWindow;
class wxDialog;
class wxListBox;
class wxButton;
class wxString;
class wxSize;
class wxPoint;
class wxCommandEvent;

#undef QueryInputsDlg_STYLE
#define QueryInputsDlg_STYLE wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

class VE_GUIPLUGINS_EXPORTS QueryInputsDlg : public wxDialog
{
   public:
		QueryInputsDlg(wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT("Query Inputs"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = QueryInputsDlg_STYLE);
		virtual ~QueryInputsDlg();

		enum
		{
			ID_WXSTATICTEXT2 = 1011,
			ID_WXSTATICTEXT1 = 1010,
			ID_WXBUTTON4 = 1009,
			ID_WXBUTTON3 = 1008,
			ID_WXBUTTON2 = 1006,
			ID_WXBUTTON1 = 1005,
			ID_WXLISTBOX2 = 1004,
			ID_WXLISTBOX1 = 1002,
			ID_DUMMY_VALUE_
		};

		void WxButton1Click(wxCommandEvent& event);
		void WxButton2Click(wxCommandEvent& event);
		void WxButton3Click(wxCommandEvent& event);
		void WxButton4Click(wxCommandEvent& event);
		void AppendList(const char *);
		bool IsSubmit();
		wxString GetDataString(int);
		int GetDataSize();
		wxStaticText *WxStaticText2;
		wxStaticText *WxStaticText1;
		wxButton *WxButton4;
		wxButton *WxButton3;
		wxButton *WxButton2;
		wxButton *WxButton1;
		wxListBox *WxListBox2;
		wxListBox *WxListBox1;
		bool submit;
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();

		DECLARE_EVENT_TABLE();
};

#endif
