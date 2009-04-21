//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#ifndef __OSGWXTREE_TREE_CONTROL_H__
#define __OSGWXTREE_TREE_CONTROL_H__ 1

#include <osg/Node>
#include <wx/treectrl.h>
#include <osgWxTree/Export.h>


namespace osgWxTree {


class OSGWXTREE_EXPORT TreeControl
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


#endif // __OSGWXTREE_TREE_CONTROL_H__
