
#ifndef APP_FRAME_H
#define APP_FRAME_H

// --- VE-Suite Includes --- //
class AppMenuBar;
class AppToolBar;
class AppTreeCtrl;
class AppNotebook;
class DBConnectionDialog;
class VESConnectionDialog;
class CorbaUnitManager;

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

    ///
    DBConnectionDialog* const GetDBConnectionDialog() const;

    ///
    VESConnectionDialog* const GetVESConnectionDialog() const;

protected:

private:
    ///
    void CreateGUI();

    AppMenuBar* m_appMenuBar;
    AppToolBar* m_appToolBar;
    AppTreeCtrl* m_appTreeCtrl;
    AppNotebook* m_appNotebook;
    DBConnectionDialog* m_dbConnectionDialog;
    VESConnectionDialog* m_vesConnectionDialog;
    CorbaUnitManager* m_corbaUnitManager;
    
    DECLARE_EVENT_TABLE()

};

#endif //APP_FRAME_H
