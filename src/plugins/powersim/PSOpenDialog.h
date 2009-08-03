
#ifndef PS_OPEN_DIALOG_H
#define PS_OPEN_DIALOG_H

// --- wxWidgets Includes --- //
#include <wx/dialog.h>

class wxComboBox;

class PSOpenDialog : public wxDialog
{
public:
    ///Constructor
    PSOpenDialog( wxWindow* parent );

    ///Destructor
    virtual ~PSOpenDialog();

    ///
    void OnOK( wxCommandEvent& event );

    ///
    void OnCancel( wxCommandEvent& event );

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

    ///
    wxComboBox* m_comboBox;

    ///
    DECLARE_EVENT_TABLE();

};

#endif //PS_OPEN_DIALOG_H
