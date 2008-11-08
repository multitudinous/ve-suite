//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#include <iostream>
#include <sstream>

#include <osgWxTree/Utils.h>


namespace osgWxTree {


std::string indent( unsigned int ni, std::string indentstring = "\t" )
{
    std::stringstream ss;
    for( size_t ii = 0; ii < ni; ++ii )
    {
        ss << indentstring;
    }
    return( ss.str() );
}

wxTreeItemId getOrCreateTreeRoot( wxTreeCtrl *tree, wxTreeItemId currentRoot, osg::Node *node )
{
    wxTreeItemId newid = currentRoot;
    if( newid == wxTreeItemId( NULL ) )
    {
        newid = osgWxTree::setTreeRoot( tree, node );
    }
    return( newid );
}

void PopulateTreeControlWithNodeVisitor::apply( osg::Node & node )
{
    activeGroup_ = getOrCreateTreeRoot( tree_, activeGroup_, &node );
    activeChild_ = osgWxTree::addTreeItem( tree_, activeGroup_, &node );
    if( recurse_ ) traverse( node );
}

void PopulateTreeControlWithNodeVisitor::apply( osg::Group & group )
{
    wxTreeItemId pushgroup;
    // first time, initialize root
    activeGroup_ = getOrCreateTreeRoot( tree_, activeGroup_, &group );
    pushgroup = activeGroup_;
    activeGroup_ = osgWxTree::addTreeItem( tree_, activeGroup_, &group );
    indent_++;

    if( recurse_ ) traverse( group );
    indent_--;

    activeGroup_ = pushgroup;
}

#if 0
else
{
    wxTreeItemId root = osgWxTree::osgNodeTreeRoot( tree_, &group );
    for( unsigned int childNumber = 0; childNumber < group.getNumChildren(); ++childNumber )
    {
        osg::ref_ptr< osg::Node > child = group.getChild( childNumber );
        osgWxTree::osgNodeTreeItem( tree_, activeGroup_, *child );
    }
}
#endif

wxTreeItemId setTreeToFlatWithParent( wxTreeCtrl * tree,
                                      osg::Node * node,
                                      const std::string & name )
{
    wxTreeItemId root;
    osg::ref_ptr< osg::Group > parent = node->getParent( 0 ); // this only gets the _first_ parent
    if( parent.valid() )
    {
        root = osgWxTree::setTreeToFlatGroup( tree, parent.get(), node );
    }
    else
    {
        root = osgWxTree::setTreeRoot( tree, node );
        tree->SelectItem( root );
    }
    return( root );
}

wxTreeItemId setTreeToFlatGroup( wxTreeCtrl * tree,
                                 osg::Group * group,
                                 osg::Node * selected,
                                 const std::string & name )
{
    std::string rootname = "..";
    if( !name.empty() ) rootname += "(" + name + ")";
    wxTreeItemId root = osgWxTree::setTreeRoot( tree, group, rootname );
    for( unsigned int childNumber = 0; childNumber < group->getNumChildren(); ++childNumber )
    {
        osg::ref_ptr< osg::Node > child = group->getChild( childNumber );
        wxTreeItemId tree_child = osgWxTree::addTreeItem( tree, root, child.get() );
        if ( selected == child.get() ) tree->SelectItem( tree_child );
        if( dynamic_cast< osg::Group * >( child.get() ) != NULL ) tree->SetItemHasChildren( tree_child );
    }
    tree->Expand( root );
    return( root );
}

wxTreeItemId setTreeRoot( wxTreeCtrl * tree,
                          osg::Node * node,
                          const std::string & name )
{
    std::string nodename = "NULL";
    if( node != NULL )
    {
        nodename = node->className() + std::string( ": " );
        nodename += name.empty() ? node->getName() : name; // use supplied name if it exists, else nodename
    }
    tree->DeleteAllItems();
    return( tree->AddRoot( wxString::FromUTF8( nodename.c_str() ), -1, -1, new osgNodeItemData( node ) ) );
}

wxTreeItemId addTreeItem( wxTreeCtrl * tree,
                          wxTreeItemId parent,
                          osg::Node * node,
                          const std::string & name )
{
    std::string nodename = "NULL";
    if( node != NULL )
    {
        nodename = node->className() + std::string( ": " );
        nodename += name.empty() ?  node->getName() : name; // use supplied name if it exists, else nodename
    }
    return( tree->AppendItem( parent, wxString::FromUTF8( nodename.c_str() ), -1, -1, new osgNodeItemData( node ) ) );
}


} // end namespace osgWxTree
