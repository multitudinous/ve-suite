
#ifndef APP_TREE_CTRL_H
#define APP_TREE_CTRL_H

// --- VE-Suite Includes --- //
class AppFrame;
class DBConnection;

// --- wxWidgets Includes --- //
#include <wx/treectrl.h>

// --- C/C++ Includes --- //
#include <string>


/*!\file AppTreeCtrl.h
 * AppTreeCtrl API
 */

/*!\class AppTreeCtrl
 *
 */
class AppTreeCtrl : public wxTreeCtrl
{
public:
    ///Constructor
    AppTreeCtrl( wxWindow* parent );

    ///Destructor
    virtual ~AppTreeCtrl();

    enum
    {
        DATABASE = 0,
        MSACCESS = 1,
        MYSQL = 2,
        TABLE = 3
    };

    ///
    void AddDBConnection( DBConnection* dbConnection );

protected:

private:
    ///Loads and stores the xpm images
    void LoadBitmaps();

    ///
    void CreateGUI();

    ///
    void SelectionChanged( wxTreeEvent& event );
    
    ///
    void RightClick( wxTreeEvent& event );
    
    ///
    void DoubleClick( wxTreeEvent& event );

    ///
    AppFrame* m_appFrame;

    ///
    wxTreeItemId m_rootID;

    ///
    wxTreeItemId m_selectionID;

    DECLARE_EVENT_TABLE();

};

struct DBConnectionData : public wxTreeItemData
{
    DBConnection* m_dbConnection;
};

struct DBTableData : public wxTreeItemData
{
    std::string m_dbTableName;
};

#endif //APP_TREE_CTRL_H
