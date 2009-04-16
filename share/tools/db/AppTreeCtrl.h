
#ifndef APP_TREE_CTRL_H
#define APP_TREE_CTRL_H

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
    /*
    ///
    void OnSelChanged( wxTreeEvent& event );
    
    ///
    void OnExpanded( wxTreeEvent& event );
    
    ///
    void OnRightClick( wxTreeEvent& event );
    
    ///
    void OnDoubleClick( wxTreeEvent& event );
    
    ///
    void ProcessRightClickMenuEvents( wxCommandEvent& event );
    */

private:

    DECLARE_EVENT_TABLE();

};

/*
class ModuleData : public wxTreeItemData
{
public:
    unsigned int modId;
    std::string modelUUID;
    std::string modName;
    std::string systemId;
    std::string subSystemId;
};
*/

#endif //APP_TREE_CTRL_H
