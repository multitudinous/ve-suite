
#ifndef APP_TREE_CTRL_H
#define APP_TREE_CTRL_H

// --- VE-Suite Includes --- //
class AppFrame;
class DBConnection;

// --- wxWidgets Includes --- //
#include <wx/treectrl.h>


/*!\file AppTreeCtrl.h
 * AppTreeCtrl API
 */

/*!\class AppTreeCtrl
 *
 */
class AppTreeCtrl : public wxTreeCtrl
{
public:
    ///Default Constructor
    AppTreeCtrl( wxWindow* parent );

    ///Constructor
    //AppTreeCtrl();

    ///Destructor
    virtual ~AppTreeCtrl();

protected:
    ///
    void SelectionChanged( wxTreeEvent& event );
    
    ///
    void Expand( wxTreeEvent& event );
    
    ///
    void RightClick( wxTreeEvent& event );
    
    ///
    void DoubleClick( wxTreeEvent& event );
    
private:
    ///Loads and stores the xpm images
    void LoadBitmaps();

    ///
    void CreateGUI();

    ///
    AppFrame* m_appFrame;

    DECLARE_EVENT_TABLE();

};

class ConnectionData : public wxTreeItemData
{
public:
    ///
    const DBConnection* const GetDBConnection() const;

protected:

private:
    ///
    DBConnection* m_dbConnection;

};

#endif //APP_TREE_CTRL_H
