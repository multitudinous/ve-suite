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


namespace osgQtTree {

class OSGQTTREE_EXPORT PopulateTreeControlWithNodeVisitor : public osg::NodeVisitor
{
public:
    PopulateTreeControlWithNodeVisitor( TreeModel* tree, osg::NodeVisitor::TraversalMode travMode = osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
      : osg::NodeVisitor( travMode ),
        tree_( tree ),
        activeGroup_( NULL ),
        activeChild_( NULL )
    {
        setTraversalMask( 0xffffffff );
        setNodeMaskOverride( 0xffffffff );
    }

    virtual void apply( osg::Node& node );
    virtual void apply( osg::Group& group );

private:
    TreeModel* tree_;

    osgTreeItem* activeGroup_;
    osgTreeItem* activeChild_;
};

OSGQTTREE_EXPORT QModelIndex findItemIndex( TreeModel* tree, const osg::NodePath& nodepath );

OSGQTTREE_EXPORT QModelIndex openToAndSelect( QTreeView* view, TreeModel* model, const osg::NodePath& nodepath );

OSGQTTREE_EXPORT osgTreeItem* addTreeItem( TreeItem* parent,
                                           osg::Node* node,
                                           const std::string& name = "" );


// end namespace osgQtTree
}


