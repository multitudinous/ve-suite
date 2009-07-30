
#ifndef PS_OPEN_DIALOG_H
#define PS_OPEN_DIALOG_H

// --- wxWidgets Includes --- //
#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/dir.h>

class wxStaticText;
class wxButton;
class wxComboBox;

class PSOpenDialog : public wxDialog
{
public:
    ///Constructor
    PSOpenDialog( wxWindow* parent );

    ///Destructor
    virtual ~PSOpenDialog();

    ///
    void OKButtonClick( wxCommandEvent& event );

    ///
    void CancelButtonClick( wxCommandEvent& event );

    ///
    void SetPopulateFilenames();

    ///
    wxString GetFilename();

protected:

private:
    ///
    void OnClose( wxCloseEvent& event );

    ///
    void CreateGUIControls();

    wxStaticText* Label;
    wxButton* CancelButton;
    wxButton* OKButton;
    wxComboBox* ComboBox;
    wxArrayString arrayStringFor_ComboBox;

    DECLARE_EVENT_TABLE();

};

#endif //PS_OPEN_DIALOG_H
