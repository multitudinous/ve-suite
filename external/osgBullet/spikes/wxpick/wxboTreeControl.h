#ifndef __WXBO_TREE_CONTROL_H__
#define __WXBO_TREE_CONTROL_H__

#include <osg/Node>

#include <wx/treectrl.h>

namespace isu {

class TreeControl
    : public wxTreeCtrl
{
public:
    TreeControl(    wxWindow * parent,
                    wxWindowID id,
                    const wxPoint & pos = wxDefaultPosition,
                    const wxSize & size = wxDefaultSize,
                    long style = wxTR_HAS_BUTTONS,
                    const wxValidator & validator = wxDefaultValidator,
                    const wxString & name = wxT( "isu::TreeCtrl" ) );

    void onDoubleClick( wxTreeEvent & event );
    void onExpanding( wxTreeEvent & event );
    void onCollapsing( wxTreeEvent & event );
    
    enum { TreeControlID = 3412 };

private:
    DECLARE_EVENT_TABLE()
};
 
} // end namespace isu


#endif // __WXBO_TREE_CONTROL_H__
