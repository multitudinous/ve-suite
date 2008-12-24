#ifndef APOPENDIALOG_h
#define APOPENDIALOG_h

#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/dir.h>

#undef APOpenDialog_STYLE
#define APOpenDialog_STYLE wxCAPTION | wxSYSTEM_MENU | wxDIALOG_NO_PARENT | wxMINIMIZE_BOX | wxCLOSE_BOX

class APOpenDialog : public wxDialog
{
    private:
        DECLARE_EVENT_TABLE();
		
    public:
        APOpenDialog(wxWindow *parent, wxWindowID id = 1, const wxString &title = wxT("BKP/APW File"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = APOpenDialog_STYLE);
        virtual ~APOpenDialog();
        void OKButtonClick(wxCommandEvent& event);
        void CancelButtonClick(wxCommandEvent& event);
        void SetPopulateFilenames( );
        wxString GetFilename( );

    private:
        wxStaticText *Label;
        wxButton *CancelButton;
        wxButton *OKButton;
        wxComboBox *ComboBox;
        wxArrayString arrayStringFor_ComboBox;

        void OnClose(wxCloseEvent& event);
        void CreateGUIControls();
};

#endif
