
#ifndef VES_CONNECTION_DIALOG_H
#define VES_CONNECTION_DIALOG_H

// --- VE-Suite Includes --- //
class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/dialog.h>
#include <wx/string.h>

class wxComboBox;
class wxTextCtrl;

// --- C/C++ Includes --- //
#include <string>

/*!\file VESConnectionDialog.h
 *
 */

/*!\class VESConnectionDialog
 * wx dialog for connecting to VE_Suite
 */
class VESConnectionDialog : public wxDialog
{
public:
    ///Constructor
    VESConnectionDialog( wxWindow* parent );

    ///Destructor
    virtual ~VESConnectionDialog();

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

	wxComboBox* m_workingDirectoryComboBox;
	wxTextCtrl* m_ceServerHostTextCtrl;
	wxTextCtrl* m_cePortTextCtrl;

    DECLARE_EVENT_TABLE();

};

#endif //VES_CONNECTION_DIALOG_H
