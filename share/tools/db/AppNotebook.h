
#ifndef APP_NOTEBOOK_H
#define APP_NOTEBOOK_H

// --- VE-Suite Includes --- //
class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/notebook.h>

class wxPanel;
class wxGrid;

/*!\file AppNotebook.h
 *
 */

/*!\class AppNotebook
 *
 */
class AppNotebook : public wxNotebook
{
public:
    ///Constructor
    AppNotebook( wxWindow* parent );

    ///Destructor
    virtual ~AppNotebook();

protected:

private:
    ///Adds the tools to the toolbar
    void CreateGUI();

    ///
    AppFrame* m_appFrame;

    wxPanel* m_tableDetailsPanel;
    wxPanel* m_dataPanel;
    wxPanel* m_sqlPanel;

    wxGrid* m_tableDetailsGrid;

    DECLARE_EVENT_TABLE()
};

#endif //APP_NOTEBOOK_H
