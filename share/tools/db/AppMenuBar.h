
#ifndef APP_MENU_BAR_H
#define APP_MENU_BAR_H

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
    AppMenuBar();

    ///Destructor
    virtual ~AppMenuBar();

protected:

private:
    ///Adds the tools to the toolbar
    void CreateGUI();

    wxMenu* m_fileMenu;
    wxMenu* m_helpMenu;

    DECLARE_EVENT_TABLE()
};

#endif //APP_MENU_BAR_H
