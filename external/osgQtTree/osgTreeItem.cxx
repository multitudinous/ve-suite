#include "osgTreeItem.h"

namespace osgQtTree
{

osgTreeItem::osgTreeItem( const QList<QVariant> &data, 
                          TreeItem *parent, 
                          osg::Node* node, 
                          const std::string& nodepath ):
        TreeItem( data, parent ),
        mNode( node ),
        mNodePath( nodepath )
{
}

void osgTreeItem::SetNode( osg::Node* node )
{
    mNode = node;
}

osg::Node* osgTreeItem::GetNode()
{
    return mNode;
}

void osgTreeItem::SetNodePath( const std::string& nodepath )
{
    mNodePath = nodepath;
}

const std::string& osgTreeItem::GetNodePath()
{
    return mNodePath;
}


} // namespace osgQtTree
