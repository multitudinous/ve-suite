//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//

#pragma once


#include "Export.h"

#include "treemodel.h"
#include "osgTreeItem.h"

#include <QtCore/QModelIndex>
#include <QtGui/QTreeView>

#include <osg/NodeVisitor>
#include <osg/Group>

//http://msdn.microsoft.com/en-us/library/62688esh.aspx
OSGQTTREE_EXPORT void initResources();

namespace osgQtTree {

class OSGQTTREE_EXPORT PopulateTreeControlWithNodeVisitor : public osg::NodeVisitor
{
public:
    PopulateTreeControlWithNodeVisitor( osg::Node* rootnode, TreeModel* tree, osg::NodeVisitor::TraversalMode travMode = osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
      : osg::NodeVisitor( travMode ),
        tree_( tree ),
        rootnode_( rootnode ),
        activeGroup_( NULL ),
        activeChild_( NULL )
    {
        initResources();
        setTraversalMask( 0xffffffff );
        setNodeMaskOverride( 0xffffffff );
    }

    virtual void apply( osg::Node& node );
    virtual void apply( osg::Group& group );
    
    void setRootNode( osg::Node* rootnode );

private:
    TreeModel* tree_;
    osg::Node* rootnode_;

    osgTreeItem* activeGroup_;
    osgTreeItem* activeChild_;
};

OSGQTTREE_EXPORT QModelIndex findItemIndex( TreeModel* tree, const osg::NodePath& nodepath );

OSGQTTREE_EXPORT QModelIndex openToAndSelect( QTreeView* view, TreeModel* model, const osg::NodePath& nodepath );

OSGQTTREE_EXPORT osgTreeItem* addTreeItem( TreeItem* parent,
                                           osg::Node* node,
                                           const std::string& nodepath,
                                           const std::string& name = "" );
                                           
OSGQTTREE_EXPORT std::string getNodePathString( osg::Node* const startNode,
                                                 osg::Node* endNode );


// end namespace osgQtTree
}


