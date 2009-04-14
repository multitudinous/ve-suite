
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
    ///Default Constructor
    ToolBar();

    ///Constructor
    ToolBar(
        wxWindow* parent,
        wxWindowID id = wxID_ANY,
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize,
        long style = wxNO_BORDER | wxTB_HORIZONTAL,
        const wxString& name = wxT( "ToolBar" ) );

    ///Destructor
    virtual ~ToolBar();

protected:

private:
    ///Loads and stores the xpm images into a std::map for this toolbar
    void LoadToolBarBitmaps();

    ///Adds the tools to the toolbar
    void CreateGUI();

    ///A map that holds the bitmaps for this toolbar
    std::map< std::string, wxBitmap > mToolbarBitmaps;

    DECLARE_EVENT_TABLE()
};

#endif //TOOL_BAR_H
