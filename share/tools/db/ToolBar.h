
#ifndef TOOL_BAR_H
#define TOOL_BAR_H

// --- wxWidgets Includes --- //
#include <wx/toolbar.h>

// --- C/C++ Libraries --- //
#include <map>
#include <string>

/*!\file ToolBar.h
 */

/*!\class ToolBar
 *
 */
class ToolBar : public wxToolBar
{
public:
    ///Constructor
    ToolBar( wxWindow* parent );

    ///Destructor
    virtual ~ToolBar();

protected:

private:
    ///Loads and stores the xpm images into a std::map for this toolbar
    void LoadToolBarBitmaps();

    ///Adds the tools to the toolbar
    void CreateGUI();

    ///
    void OpenConnectionDialog( wxCommandEvent& event );

    ///A map that holds the bitmaps for this toolbar
    std::map< std::string, wxBitmap > mToolbarBitmaps;

    DECLARE_EVENT_TABLE()
};

#endif //TOOL_BAR_H
