
#ifndef CONNECTION_DIALOG_H
#define CONNECTION_DIALOG_H

// --- VE-Suite Includes --- //

// --- wxWidgets Includes --- //
#include <wx/dialog.h>

class wxTextCtrl;
class wxComboBox;

// --- C/C++ Includes --- //


/*!\file ConnectionDialog.h
 *
 */

/*!\class ConnectionDialog
 * wx dialog for connecting to a db
 */
class ConnectionDialog : public wxDialog
{
public:
    ///Default Constructor
    ConnectionDialog();

    ///Constructor
    ConnectionDialog(
        wxWindow* parent,
        wxWindowID id = wxID_ANY,
        const wxString& title = wxT( "Add a Connection" ),
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxSize( -1, -1 ),
        long style = wxCAPTION | wxCLOSE_BOX | wxMAXIMIZE_BOX | wxMINIMIZE_BOX |
                     wxRESIZE_BORDER | wxSTAY_ON_TOP | wxSYSTEM_MENU );

    ///Destructor
    virtual ~ConnectionDialog();

protected:

private:
    void CreateGUI();

    wxTextCtrl* m_connectionNameTextCtrl;
    wxComboBox* m_connectionTypeComboBox;
    wxTextCtrl* m_hostTextCtrl;
    wxTextCtrl* m_portTextCtrl;
    wxTextCtrl* m_usernameTextCtrl;
    wxTextCtrl* m_passwordTextCtrl;

    DECLARE_EVENT_TABLE();

};

#endif //CONNECTION_DIALOG_H
