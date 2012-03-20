//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//

#include "osgQtTree.h"
#include "treemodel.h"
#include "osgTreeItem.h"

#include <QtGui/QTreeView>
#include <QtCore/QModelIndex>

#include <osgwTools/NodePathUtils.h>

#include <iostream>
#include <sstream>

namespace osgQtTree {


osgTreeItem*
getOrCreateTreeRoot( TreeModel *tree, osgTreeItem* currentRoot, osg::Node *node, osg::Node* rootnode )
{
    osgTreeItem* newItem = currentRoot;
    if( newItem == 0 )
    {
        // The nodepath here is not actually correct since we don't use the
        // proper root node.
        std::string nodepath = getNodePathString( rootnode, node );
        newItem = addTreeItem( tree->GetRoot(), node, nodepath );
    }

    return( newItem );
}

void
PopulateTreeControlWithNodeVisitor::apply( osg::Node& node )
{
    activeGroup_ = getOrCreateTreeRoot( tree_, activeGroup_, &node, rootnode_ );
    std::string nodepath = getNodePathString( rootnode_, &node );
    activeChild_ = osgQtTree::addTreeItem( activeGroup_, &node, nodepath );

    traverse( node );
}

void
PopulateTreeControlWithNodeVisitor::apply( osg::Group& group )
{
    osgTreeItem* pushgroup = 0;
    // first time, initialize root
    if( tree_->GetRoot()->childCount() == 0 )
    {
        // First item
        activeGroup_ = getOrCreateTreeRoot( tree_, activeGroup_, &group, rootnode_ );
    }
    else
    {
        pushgroup = activeGroup_;
        std::string nodepath = getNodePathString( rootnode_, &group );
        activeGroup_ = osgQtTree::addTreeItem( activeGroup_, &group, nodepath );
    }

    traverse( group );

    activeGroup_ = pushgroup;
}

void 
PopulateTreeControlWithNodeVisitor::setRootNode( osg::Node* rootnode )
{
    rootnode_ = rootnode;
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
            void* tempPointer = childIndex.internalPointer();
            if( tempPointer )
            {
                osgTreeItem* child = static_cast<osgTreeItem*>(tempPointer);
                if( child->GetNode() == node )
                {
                    break;
                }
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
    view->clearSelection();
    
    // Deal with empty nodepath
    if( nodepath == osg::NodePath() )
    {
        return QModelIndex();
    }
    
    QModelIndex index( findItemIndex( model, nodepath) );
    if( index.isValid() )
    {
        view->setCurrentIndex( index );
        view->scrollTo( index );
    }
    return index;
}

osgTreeItem* addTreeItem( TreeItem* parent,
                          osg::Node* node,
                          const std::string& nodepath,
                          const std::string& name )
{
    std::string nodename( "NULL" );
    if( node != NULL )
    {
        //nodename = node->className() + std::string( ": " );
        nodename = name.empty() ?  node->getName() : name; // use supplied name if it exists, else nodename
        nodename += std::string(": ") + node->className();
    }

    QList<QVariant> viewData;
    viewData << QString::fromStdString(nodename);
    
    osgTreeItem* child = new osgTreeItem( viewData, parent, node, nodepath );

    if( node )
    {
        osg::Node::DescriptionList descList = node->getDescriptions();
        for( size_t i = 0; i < descList.size(); ++i )
        {
            if( (descList.at( i ) == "Assembly") || (descList.at( i ) == "Part") )
            {
                child->SetIcon(QIcon(
                ":/tree/cadicon.png"));
            }
            else if( descList.at( i ) == "VE_DATA_NODE" )
            {
                child->SetIcon(QIcon(
                ":/tree/dataseticon.png"));
            }
            else if( descList.at( i ) == "VE_XPLORER_PLUGIN_ID" )
            {
                child->SetIcon(QIcon(
                ":/tree/pluginicon.jpg"));
            }
        }
    }

    parent->appendChild( child );
    return( child );
}

std::string getNodePathString( osg::Node* const startNode,
                                                 osg::Node* endNode )
{
    // Walk up from end to start
    osg::NodePath nodePath;
    nodePath.push_back( endNode );
    while( (endNode->getNumParents() != 0) && (endNode != startNode) )
    {
        endNode = endNode->getParent( 0 );
        nodePath.push_back( endNode );
    }

    // Reverse the nodePath so that it points from start to end
    osg::NodePath temp;
    temp.assign( nodePath.rbegin(), nodePath.rend() );

    return osgwTools::nodePathToString( temp );
}


// end namespace osgQtTree
}
