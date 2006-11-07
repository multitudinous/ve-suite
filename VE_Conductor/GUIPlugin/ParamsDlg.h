#ifndef PARAMSDLG_H
#define PARAMSDLG_H

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/choice.h>
#include <wx/grid.h>
#include <wx/panel.h>
//#include <iostream>
//#include <iomanip>
#include <vector>
#include <map>
#include <iostream>
#include <sstream>
#include "VE_Installer/include/VEConfig.h"

#undef ParamsDlg_STYLE
#define ParamsDlg_STYLE wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

class wxWindow;
class wxDialog;
class wxListBox;
class wxString;
class wxSize;
class wxPoint;
class wxCommandEvent;
class wxGrid;

class VE_GUIPLUGINS_EXPORTS ParamsDlg : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
	public:
		ParamsDlg(wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT("Params"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = ParamsDlg_STYLE);
		virtual ~ParamsDlg();
		void WxChoice1Selected(wxCommandEvent& event );
		void AddToList(const char *);
		void AddResults(std::string, std::vector< std::string >, std::vector< std::string >);
		wxChoice *WxChoice1;
		wxGrid *WxGrid1;
		wxPanel *WxPanel1;
		//std::map<const char *, std::vector< std::string >> ParamNames;
		//std::map<const char *, std::vector< std::string >> ParamValues;
		std::map<std::string, std::vector< std::string >> ParamNames;
		std::map<std::string, std::vector< std::string >> ParamValues;
		
		int sizeName;
		int sizeValue;
		enum
		{
			////GUI Enum Control ID Start
			ID_WXCHOICE1 = 1003,
			ID_WXGRID1 = 1002,
			ID_WXPANEL1 = 1001,
			////GUI Enum Control ID End
			ID_DUMMY_VALUE_ //don't remove this value unless you have other enum values
		};
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
};

#endif
