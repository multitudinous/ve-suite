
#ifndef APP_FRAME_H
#define APP_FRAME_H

// --- VE-Suite Includes --- //
class MenuBar;
class ToolBar;
class Notebook;
class ConnectionDialog;

// --- wxWidgets Includes --- //
#include <wx/frame.h>

class wxTreeCtrl;

// --- C/C++ Includes --- //


/*!\file AppFrame.h
 *
 */

/*!\class AppFrame
 * Main wx frame for db
 */
class AppFrame : public wxFrame
{
public:
    ///Constructor
    AppFrame( wxWindow* parent, wxWindowID id );

    ///Destructor
    virtual ~AppFrame();

protected:

private:
    ///
    void CreateGUI();

    ///
    void OpenConnectionDialog( wxCommandEvent& event );

    MenuBar* m_menuBar;
    ToolBar* m_toolBar;
    ConnectionDialog* m_connectionDialog;
    Notebook* m_notebook;

    wxTreeCtrl* m_connectionsTreeCtrl;
    
    DECLARE_EVENT_TABLE()

};

#endif //APP_FRAME_H
