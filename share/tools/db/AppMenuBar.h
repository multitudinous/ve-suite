
#ifndef APP_MENU_BAR_H
#define APP_MENU_BAR_H

// --- VE-Suite Includes --- //
class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/menu.h>

// --- C/C++ Libraries --- //

/*!\file AppMenuBar.h
 */

/*!\class AppMenuBar
 *
 */
class AppMenuBar : public wxMenuBar
{
public:
    ///Constructor
    AppMenuBar( wxWindow* parent );

    ///Destructor
    virtual ~AppMenuBar();

protected:

private:
    ///Adds the tools to the toolbar
    void CreateGUI();

    ///
    AppFrame* m_appFrame;

    wxMenu* m_fileMenu;
    wxMenu* m_helpMenu;

    DECLARE_EVENT_TABLE()
};

#endif //APP_MENU_BAR_H
