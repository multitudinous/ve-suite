#pragma once

#include "treeitem.h"
#include "Export.h"

#include <osg/Node>
#include <string>

namespace osgQtTree
{

class OSGQTTREE_EXPORT osgTreeItem : public TreeItem
{
public:
    osgTreeItem(const QList<QVariant> &data, 
                TreeItem *parent = 0, 
                osg::Node* node = 0,
                const std::string& nodepath = "");

    void SetNode( osg::Node* node );
    osg::Node* GetNode();
    void SetNodePath( const std::string& nodepath );
    const std::string& GetNodePath();

private:
    osg::Node* mNode;
    std::string mNodePath;
};

} // namespace osgQtTree

