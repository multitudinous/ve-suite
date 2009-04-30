
#ifndef APP_NOTEBOOK_H
#define APP_NOTEBOOK_H

// --- VE-Suite Includes --- //
#include "TypeDefs.h"

class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/notebook.h>

class wxPanel;
class wxGrid;

// --- C/C++ Includes --- //


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

    ///
    void ClearTableDetails();

    ///
    void ClearTableData();

    ///
    void PopulateTableDetails( const StringVector2D* tableDetails );

    ///
    void PopulateTableData(
        const StringVector1D* tableFieldNames, const StringVector2D* tableData );

protected:

private:
    ///Adds the tools to the toolbar
    void CreateGUI();

    ///
    AppFrame* m_appFrame;

    wxPanel* m_tableDetailsPanel;
    wxPanel* m_tableDataPanel;
    wxPanel* m_sqlPanel;

    wxGrid* m_tableDetailsGrid;
    wxGrid* m_tableDataGrid;

    DECLARE_EVENT_TABLE()
};

#endif //APP_NOTEBOOK_H
