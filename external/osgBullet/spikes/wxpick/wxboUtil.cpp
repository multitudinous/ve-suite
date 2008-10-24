#include <wxboUtil.h>
#include <osg/notify>

namespace isu {

void PopulateTreeControlWithNodeVisitor::apply( osg::Node & node )
{
    if( activeGroup_ == wxTreeItemId( NULL ) )
    {
        isu::setTreeRoot( tree_, &node );
    }
}

void PopulateTreeControlWithNodeVisitor::apply( osg::Group & group )
{
    // first time, initialize root
    if( activeGroup_ == wxTreeItemId( NULL ) )
    {
        activeGroup_ = isu::setTreeRoot( tree_, &group );
        if( recurse_ )
            addChildren( group );
    }
    else
    {
        if( !recurse_ )
            osg::notify( osg::ALWAYS ) << "Not recursing? How could that be..." << std::endl;
        addChildren( group );
    }
}
void PopulateTreeControlWithNodeVisitor::addChildren( osg::Group& group )
{
    wxTreeItemId parent( activeGroup_ );
    unsigned int idx;
    for( idx=0; idx<group.getNumChildren(); idx++ )
    {
        osg::Node* node = group.getChild( idx );
        activeGroup_ = isu::addTreeItem( tree_, parent, node );
        node->accept( *this );
    }
}


#if 0
else
{
    wxTreeItemId root = isu::osgNodeTreeRoot( tree_, &group );
    for( unsigned int childNumber = 0; childNumber < group.getNumChildren(); ++childNumber )
    {
        osg::ref_ptr< osg::Node > child = group.getChild( childNumber );
        isu::osgNodeTreeItem( tree_, activeGroup_, *child );
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
        root = isu::setTreeToFlatGroup( tree, parent.get(), node );
    }
    else
    {
        root = isu::setTreeRoot( tree, node );
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
    wxTreeItemId root = isu::setTreeRoot( tree, group, rootname );
    for( unsigned int childNumber = 0; childNumber < group->getNumChildren(); ++childNumber )
    {
        osg::ref_ptr< osg::Node > child = group->getChild( childNumber );
        wxTreeItemId tree_child = isu::addTreeItem( tree, root, child.get() );
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

} // end namespace isu
