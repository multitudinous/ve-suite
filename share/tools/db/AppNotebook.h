
#ifndef APP_NOTEBOOK_H
#define APP_NOTEBOOK_H

// --- wxWidgets Includes --- //
#include <wx/notebook.h>

class wxPanel;

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
    ///Loads and stores the images for this notebook
    void LoadImages();

    ///Adds the tools to the toolbar
    void CreateGUI();

    wxPanel* m_tableDetailsPanel;
    wxPanel* m_dataPanel;
    wxPanel* m_sqlPanel;

    DECLARE_EVENT_TABLE()
};

#endif //APP_NOTEBOOK_H
