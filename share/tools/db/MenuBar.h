
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
    ///Default Constructor
    MenuBar();

    ///Destructor
    virtual ~MenuBar();

protected:

private:
    ///Adds the tools to the toolbar
    void CreateGUI();

    DECLARE_EVENT_TABLE()
};

#endif //MENU_BAR_H
