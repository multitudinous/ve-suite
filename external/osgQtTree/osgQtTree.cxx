//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//

#include "osgQtTree.h"
#include "treemodel.h"
#include "osgTreeItem.h"

#include <QTreeView>
#include <QModelIndex>


#include <iostream>
#include <sstream>

namespace osgQtTree {


osgTreeItem*
getOrCreateTreeRoot( TreeModel *tree, osgTreeItem* currentRoot, osg::Node *node )
{
    osgTreeItem* newItem = currentRoot;
    if( newItem == 0 )
    {
        newItem = addTreeItem( tree->GetRoot(), node );
    }

    return( newItem );
}

void
PopulateTreeControlWithNodeVisitor::apply( osg::Node& node )
{
    activeGroup_ = getOrCreateTreeRoot( tree_, activeGroup_, &node );
    activeChild_ = osgQtTree::addTreeItem( activeGroup_, &node );

    traverse( node );
}

void
PopulateTreeControlWithNodeVisitor::apply( osg::Group& group )
{
    osgTreeItem* pushgroup;
    // first time, initialize root
    if( tree_->GetRoot()->childCount() == 0 )
    {
        // First item
        activeGroup_ = getOrCreateTreeRoot( tree_, activeGroup_, &group );
    }
    else
    {
        pushgroup = activeGroup_;
        activeGroup_ = osgQtTree::addTreeItem( activeGroup_, &group );
    }

    traverse( group );

    activeGroup_ = pushgroup;
}

QModelIndex
findItemIndex( TreeModel* tree, const osg::NodePath& nodepath )
{
    // Get the index of the first item
    QModelIndex modelIndex = tree->index( 0, 0 );
    if( !modelIndex.isValid() )
    {
        // Return an invalid index if there are no items in the tree
        return QModelIndex();
    }

    osgTreeItem* item = static_cast<osgTreeItem*>(modelIndex.internalPointer());

    // Find the first node in NodePath that matches the root item data.
    // Need to do this because NodePath might include a Camera, which
    // might not be in the tree (for example); or the tree could simply
    // be set up to display a subgraph.
    osg::NodePath::const_iterator it( nodepath.begin() );
    osg::Node* rootNode( *it );
    while( (it != nodepath.end()) &&
        ( item->GetNode() != rootNode) )
    {
        rootNode = *(++it);
    }
    if( it == nodepath.end() )
    {
        // Could not find the tree root item in the NodePath.
        return QModelIndex();
    }

    // Finally, walk the tree and the NodePath in sync.
    it++;
    for( ; it != nodepath.end(); it++ )
    {
        osg::Node* node( *it );
        QModelIndex childIndex;
        int row = 0;
        while( row < tree->rowCount(modelIndex) )
        {
            childIndex = modelIndex.child( row, 0 );
            osgTreeItem* child = static_cast<osgTreeItem*>(childIndex.internalPointer());
            if( child->GetNode() == node )
            {
                break;
            }
            ++row;
        }
        modelIndex = childIndex;
    }
    return( modelIndex );
}

QModelIndex
openToAndSelect( QTreeView* view, TreeModel* model, const osg::NodePath& nodepath )
{
    QModelIndex index( findItemIndex( model, nodepath) );
    if( index.isValid() )
    {
        view->clearSelection();
        view->setCurrentIndex( index );
        view->scrollTo( index );
    }
    return index;
}

osgTreeItem* addTreeItem( TreeItem* parent,
                          osg::Node* node,
                          const std::string& name )
{
    std::string nodename( "NULL" );
    if( node != NULL )
    {
        nodename = node->className() + std::string( ": " );
        nodename += name.empty() ?  node->getName() : name; // use supplied name if it exists, else nodename
    }

    QList<QVariant> viewData;
    viewData << QString::fromStdString(nodename);
    
    osgTreeItem* child = new osgTreeItem( viewData, parent, node);
    parent->appendChild( child );
    return( child );
}


// end namespace osgQtTree
}
