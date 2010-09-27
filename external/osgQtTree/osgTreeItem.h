#pragma once

#include "treeitem.h"

#include <osg/Node>

namespace osgQtTree
{

class osgTreeItem : public TreeItem
{
public:
    osgTreeItem(const QList<QVariant> &data, TreeItem *parent = 0, osg::Node* node = 0);

    void SetNode( osg::Node* node );
    osg::Node* GetNode();

private:
    osg::Node* mNode;
};

} // namespace osgQtTree

