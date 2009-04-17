
#ifndef CONNECTION_DIALOG_H
#define CONNECTION_DIALOG_H

// --- VE-Suite Includes --- //

// --- wxWidgets Includes --- //
#include <wx/dialog.h>
#include <wx/string.h>

class wxComboBox;
class wxChoice;
class wxTextCtrl;

// --- C/C++ Includes --- //
#include <string>

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

    ///
    void Connect( wxCommandEvent& event );

    ///
    std::string ConvertUnicode( const wxChar* data );

    wxComboBox* m_storedConnectionComboBox;
    wxChoice* m_connectionTypeChoice;
    wxTextCtrl* m_serverHostTextCtrl;
    wxTextCtrl* m_portTextCtrl;
    wxTextCtrl* m_usernameTextCtrl;
    wxTextCtrl* m_passwordTextCtrl;
    wxTextCtrl* m_defaultSchemaTextCtrl;

    DECLARE_EVENT_TABLE();

};

#endif //CONNECTION_DIALOG_H
