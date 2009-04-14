
#ifndef APP_FRAME_H
#define APP_FRAME_H

// --- VE-Suite Includes --- //
class MenuBar;
class ToolBar;
class ConnectionDialog;

// --- wxWidgets Includes --- //
#include <wx/frame.h>

class wxMenuBar;
class wxMenu;
class wxToolBar;
class wxTreeCtrl;
class wxNotebook;
class wxPanel;
class wxGrid;

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
    ///Default Constructor
    AppFrame();

    ///Constructor
    AppFrame(
        wxWindow* parent,
        wxWindowID id = wxID_ANY,
        const wxString& title = wxT( "VE-DB" ),
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxSize( 693, 595 ),
        long style = wxDEFAULT_FRAME_STYLE | wxICONIZE | wxTAB_TRAVERSAL );

    ///Destructor
    virtual ~AppFrame();

protected:

private:
    void CreateGUI();

    MenuBar* m_menuBar;
    ToolBar* m_toolBar;
    ConnectionDialog* m_connectionDialog;
    
    DECLARE_EVENT_TABLE()

};

#endif //APP_FRAME_H
