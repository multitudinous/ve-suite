//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//

#pragma once


#include "Export.h"

#include "treemodel.h"
#include "osgTreeItem.h"

#include <QModelIndex>
#include <QTreeView>

#include <osg/NodeVisitor>
#include <osg/Group>


namespace osgQtTree {

class OSGQTTREE_EXPORT PopulateTreeControlWithNodeVisitor : public osg::NodeVisitor
{
public:
    PopulateTreeControlWithNodeVisitor( TreeModel* tree, osg::NodeVisitor::TraversalMode travMode = osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
      : osg::NodeVisitor( travMode ),
        tree_( tree ),
        activeGroup_( NULL ),
        activeChild_( NULL )
    {}

    virtual void apply( osg::Node& node );
    virtual void apply( osg::Group& group );

private:
    TreeModel* tree_;

    osgTreeItem* activeGroup_;
    osgTreeItem* activeChild_;
};

QModelIndex OSGQTTREE_EXPORT findItemIndex( TreeModel* tree, const osg::NodePath& nodepath );

QModelIndex OSGQTTREE_EXPORT openToAndSelect( QTreeView* view, TreeModel* model, const osg::NodePath& nodepath );

osgTreeItem* OSGQTTREE_EXPORT addTreeItem( TreeItem* parent,
                                           osg::Node* node,
                                           const std::string& name = "" );


// end namespace osgQtTree
}


