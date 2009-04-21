
#ifndef DB_CONNECTION_DIALOG_H
#define DB_CONNECTION_DIALOG_H

// --- VE-Suite Includes --- //
class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/dialog.h>
#include <wx/string.h>

class wxComboBox;
class wxChoice;
class wxTextCtrl;

// --- C/C++ Includes --- //
#include <string>

/*!\file DBConnectionDialog.h
 *
 */

/*!\class DBConnectionDialog
 * wx dialog for connecting to a database
 */
class DBConnectionDialog : public wxDialog
{
public:
    ///Constructor
    DBConnectionDialog( wxWindow* parent );

    ///Destructor
    virtual ~DBConnectionDialog();

protected:

private:
    ///
    void CreateGUI();

    ///
    void Connect( wxCommandEvent& event );

    ///
    void Clear( wxCommandEvent& event );

    ///
    std::string ConvertUnicode( const wxChar* data );

    ///
    AppFrame* m_appFrame;

    wxComboBox* m_storedConnectionComboBox;
    wxChoice* m_connectionTypeChoice;
    wxTextCtrl* m_serverHostTextCtrl;
    wxTextCtrl* m_portTextCtrl;
    wxTextCtrl* m_usernameTextCtrl;
    wxTextCtrl* m_passwordTextCtrl;
    wxTextCtrl* m_defaultSchemaTextCtrl;

    DECLARE_EVENT_TABLE();

};

#endif //DB_CONNECTION_DIALOG_H
