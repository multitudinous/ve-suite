
#ifndef APP_FRAME_H
#define APP_FRAME_H

// --- VE-Suite Includes --- //
class AppMenuBar;
class AppToolBar;
class AppTreeCtrl;
class AppNotebook;
class ConnectionDialog;

// --- wxWidgets Includes --- //
#include <wx/frame.h>


/*!\file AppFrame.h
 *
 */

/*!\class AppFrame
 * Main wxFrame for DBApp
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

    AppMenuBar* m_appMenuBar;
    AppToolBar* m_appToolBar;
    AppTreeCtrl* m_appTreeCtrl;
    AppNotebook* m_appNotebook;
    ConnectionDialog* m_connectionDialog;
    
    DECLARE_EVENT_TABLE()

};

#endif //APP_FRAME_H
