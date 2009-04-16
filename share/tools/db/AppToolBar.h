
#ifndef APP_TOOL_BAR_H
#define APP_TOOL_BAR_H

// --- wxWidgets Includes --- //
#include <wx/toolbar.h>

// --- C/C++ Libraries --- //
#include <map>
#include <string>

/*!\file AppToolBar.h
 */

/*!\class AppToolBar
 *
 */
class AppToolBar : public wxToolBar
{
public:
    ///Constructor
    AppToolBar( wxWindow* parent );

    ///Destructor
    virtual ~AppToolBar();

protected:

private:
    ///Loads and stores the xpm images into a std::map for this toolbar
    void LoadBitmaps();

    ///Adds the tools to the toolbar
    void CreateGUI();

    ///A map that holds the bitmaps for this toolbar
    std::map< std::string, wxBitmap > mToolbarBitmaps;

    DECLARE_EVENT_TABLE()
};

#endif //APP_TOOL_BAR_H
