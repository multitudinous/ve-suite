
#ifndef NOTEBOOK_H
#define NOTEBOOK_H

// --- wxWidgets Includes --- //
#include <wx/notebook.h>

class wxPanel;

/*!\file Notebook.h
 *
 */

/*!\class Notebook
 *
 */
class Notebook : public wxNotebook
{
public:
    ///Constructor
    Notebook( wxWindow* parent );

    ///Destructor
    virtual ~Notebook();

protected:

private:
    ///Adds the tools to the toolbar
    void CreateGUI();

    wxPanel* m_tableDetailsPanel;
    wxPanel* m_dataPanel;
    wxPanel* m_sqlPanel;

    DECLARE_EVENT_TABLE()
};

#endif //NOTEBOOK_H
