
#ifndef MENU_BAR_H
#define MENU_BAR_H

// --- wxWidgets Includes --- //
#include <wx/menu.h>

// --- C/C++ Libraries --- //

/*!\file MenuBar.h
 */

/*!\class MenuBar
 *
 */
class MenuBar : public wxMenuBar
{
public:
    ///Constructor
    MenuBar();

    ///Destructor
    virtual ~MenuBar();

protected:

private:
    ///Adds the tools to the toolbar
    void CreateGUI();

    wxMenu* m_fileMenu;
    wxMenu* m_helpMenu;

    DECLARE_EVENT_TABLE()
};

#endif //MENU_BAR_H
