#ifndef ADRUNDIALOG_H
#define ADRUNDIALOG_H

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/checkbox.h>
#include <wx/combobox.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/statbox.h>

#undef ADRunDialog_STYLE
#define ADRunDialog_STYLE wxCAPTION | wxSYSTEM_MENU | wxSTAY_ON_TOP | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

class ADRunDialog : public wxDialog
{
	private:
		DECLARE_EVENT_TABLE();
		
	public:
		ADRunDialog(wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT("rundialog"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = ADRunDialog_STYLE);
		virtual ~ADRunDialog();
		void okButtonClick(wxCommandEvent& event);
		void cancelButtonClick(wxCommandEvent& event);
		void mPauseAfterClick(wxCommandEvent& event);
		void mModeSelected(wxCommandEvent& event );
		void SetMode();
	
	private:
		wxComboBox *mMode;
		wxCheckBox *mPauseAt;
		wxCheckBox *mHistory;
		wxCheckBox *mRTS;
		wxCheckBox *mPauseAfter;
		wxComboBox *mSynchro;
		wxStaticText *WxStaticText15;
		wxStaticText *WxStaticText14;
		wxStaticText *WxStaticText13;
		wxStaticText *WxStaticText12;
		wxStaticText *WxStaticText11;
		wxTextCtrl *mRTSFactor;
		wxTextCtrl *mPauseAfterTime;
		wxTextCtrl *mPauseAtTime;
		wxStaticText *WxStaticText10;
		wxComboBox *mDisplayTime;
		wxComboBox *mModelUnits;
		wxStaticText *WxStaticText9;
		wxStaticText *WxStaticText8;
		wxStaticText *WxStaticText7;
		wxStaticText *WxStaticText6;
		wxStaticText *WxStaticText5;
		wxTextCtrl *mTimeNow;
		wxTextCtrl *mDis;
		wxTextCtrl *mCom;
		wxStaticText *WxStaticText4;
		wxStaticText *WxStaticText3;
		wxStaticText *WxStaticText2;
		wxStaticText *WxStaticText1;
		wxButton *cancelButton;
		wxButton *okButton;
		wxStaticBox *WxStaticBox4;
		wxStaticBox *WxStaticBox3;
		wxStaticBox *Time_Control;
		wxStaticBox *mRunMode;
		
	private:
		enum
		{
			ID_MMODE = 1015,
			ID_MPAUSEAT = 1047,
			ID_MHISTORY = 1046,
			ID_MRTS = 1044,
			ID_MPAUSEAFTER = 1043,
			ID_MSYNCHRO = 1041,
			ID_WXSTATICTEXT15 = 1040,
			ID_WXSTATICTEXT14 = 1039,
			ID_WXSTATICTEXT13 = 1037,
			ID_WXSTATICTEXT12 = 1036,
			ID_WXSTATICTEXT11 = 1035,
			ID_MRTSFACTOR = 1034,
			ID_MPAUSEATTIME = 1033,
			ID_WXSTATICTEXT10 = 1031,
			ID_MDISPLAYTIME = 1026,
			ID_MMODELUNITS = 1024,
			ID_WXSTATICTEXT9 = 1023,
			ID_WXSTATICTEXT8 = 1022,
			ID_WXSTATICTEXT7 = 1021,
			ID_WXSTATICTEXT6 = 1020,
			ID_WXSTATICTEXT5 = 1019,
			ID_MTIMENOW = 1018,
			ID_MDIS = 1017,
			ID_MCOM = 1016,
			ID_WXSTATICTEXT4 = 1014,
			ID_WXSTATICTEXT3 = 1013,
			ID_WXSTATICTEXT2 = 1012,
			ID_WXSTATICTEXT1 = 1011,
			ID_CANCELBUTTON = 1010,
			ID_OKBUTTON = 1009,
			ID_WXSTATICBOX4 = 1008,
			ID_WXSTATICBOX3 = 1007,
			ID_TIME_CONTROL = 1006,
			ID_LABEL1 = 1005,
			ID_DUMMY_VALUE_
        };
	
	private:
		void OnClose(wxCloseEvent& event);
		void CreateGUIControls();
};

#endif
