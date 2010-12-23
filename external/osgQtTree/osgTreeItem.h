#pragma once

#include "treeitem.h"
#include "Export.h"

#include <osg/Node>

namespace osgQtTree
{

class OSGQTTREE_EXPORT osgTreeItem : public TreeItem
{
public:
    osgTreeItem(const QList<QVariant> &data, TreeItem *parent = 0, osg::Node* node = 0);

    void SetNode( osg::Node* node );
    osg::Node* GetNode();

private:
    osg::Node* mNode;
};

} // namespace osgQtTree

