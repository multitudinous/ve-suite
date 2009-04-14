
#ifndef APP_FRAME_H
#define APP_FRAME_H

// --- VE-Suite Includes --- //


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
    AppFrame( wxWindow* parent, wxWindowID id, const wxString& title );

    ///Destructor
    virtual ~AppFrame();

protected:

private:
    
    DECLARE_EVENT_TABLE()

};

#endif //APP_FRAME_H
