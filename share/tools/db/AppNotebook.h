
#ifndef APP_NOTEBOOK_H
#define APP_NOTEBOOK_H

// --- VE-Suite Includes --- //
class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/notebook.h>

class wxPanel;
class wxGrid;

// --- C/C++ Includes --- //
#include <string>
#include <vector>

// --- typedef --- //
typedef std::vector< std::string > StringArray1D;
typedef std::vector< std::vector< std::string > > StringArray2D;

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
    void PopulateTableDetails( const StringArray2D* tableDetails );

    ///
    void PopulateTableData(
        const StringArray1D* tableFieldNames, const StringArray2D* tableData );

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
