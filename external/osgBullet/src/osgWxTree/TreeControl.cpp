//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#include <iostream>

#include <wx/defs.h>
#include <wx/treectrl.h>

#include <osgWxTree/TreeControl.h>


namespace osgWxTree {


BEGIN_EVENT_TABLE( TreeControl, wxTreeCtrl )
    EVT_TREE_ITEM_EXPANDING(    TreeControlID,  TreeControl::onExpanding )
    EVT_TREE_ITEM_COLLAPSING(   TreeControlID,  TreeControl::onCollapsing )
    EVT_TREE_ITEM_ACTIVATED(    TreeControlID,  TreeControl::onDoubleClick )
END_EVENT_TABLE()

TreeControl::TreeControl( wxWindow * parent,
                          wxWindowID id,
                          const wxPoint & pos,
                          const wxSize & size,
                          long style,
                          const wxValidator & validator,
                          const wxString & name )
    : wxTreeCtrl( parent, id, pos, size, style, validator, name )
{

}

void TreeControl::onDoubleClick( wxTreeEvent & event )
{
    std::cout << "onDoubleClick" << std::endl;
}

void TreeControl::onExpanding( wxTreeEvent & event )
{
    std::cout << "onExpanding" << std::endl;
}

void TreeControl::onCollapsing( wxTreeEvent & event )
{
    std::cout << "onCollapsing" << std::endl;
}

} // end namespace isu

