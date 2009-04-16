
#ifndef CONNECTION_DIALOG_H
#define CONNECTION_DIALOG_H

// --- VE-Suite Includes --- //

// --- wxWidgets Includes --- //
#include <wx/dialog.h>

class wxTextCtrl;
class wxChoice;

// --- C/C++ Includes --- //


/*!\file ConnectionDialog.h
 *
 */

/*!\class ConnectionDialog
 * wx dialog for connecting to a database
 */
class ConnectionDialog : public wxDialog
{
public:
    ///Constructor
    ConnectionDialog( wxWindow* parent );

    ///Destructor
    virtual ~ConnectionDialog();

protected:

private:
    ///
    void CreateGUI();

    wxTextCtrl* m_connectionNameTextCtrl;
    wxChoice* m_connectionTypeChoice;
    wxTextCtrl* m_hostTextCtrl;
    wxTextCtrl* m_portTextCtrl;
    wxTextCtrl* m_usernameTextCtrl;
    wxTextCtrl* m_passwordTextCtrl;

    DECLARE_EVENT_TABLE();

};

#endif //CONNECTION_DIALOG_H
