#include "osgTreeItem.h"

namespace osgQtTree
{

osgTreeItem::osgTreeItem( const QList<QVariant> &data, TreeItem *parent, osg::Node* node ):
        TreeItem( data, parent ),
        mNode( node )
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

} // namespace osgQtTree
