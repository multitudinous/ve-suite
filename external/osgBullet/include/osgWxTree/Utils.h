//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#ifndef __OSGWXBULLET_UTILS_H__
#define __OSGWXBULLET_UTILS_H__ 1

#include <wx/treectrl.h>
#include <osg/NodeVisitor>
#include <osg/Group>

#include <osgWxTree/Export.h>


namespace osgWxTree {


class OSGWXTREE_EXPORT osgNodeItemData
    : public wxTreeItemData
{
public:
    osgNodeItemData( osg::Node * node )
        : wxTreeItemData()
        , node_( node ) {
    }

private:
    osg::ref_ptr< osg::Node > node_;
};

class OSGWXTREE_EXPORT PopulateTreeControlWithNodeVisitor
    : public osg::NodeVisitor
{
public:
    PopulateTreeControlWithNodeVisitor( wxTreeCtrl * tree, bool recurse = false )
        : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
        , tree_( tree )
        , recurse_( recurse )
        , proc_( 0 )
        , activeGroup_( NULL )
        , indent_( 0 )
    {
    }

    virtual void apply( osg::Node & node );
    virtual void apply( osg::Group & group );
private:
    wxTreeCtrl *    tree_;
    bool recurse_;
    unsigned int proc_;

    unsigned int indent_;
    wxTreeItemId activeGroup_;
    wxTreeItemId activeChild_;
};

wxTreeItemId OSGWXTREE_EXPORT setTreeToFlatWithParent( wxTreeCtrl * tree,
                                      osg::Node * node,
                                      const std::string & name = "" );
wxTreeItemId OSGWXTREE_EXPORT setTreeToFlatGroup( wxTreeCtrl * tree,
                                 osg::Group * group,
                                 osg::Node * selected = NULL,
                                 const std::string & name = "" );
wxTreeItemId OSGWXTREE_EXPORT setTreeRoot( wxTreeCtrl * tree,
                          osg::Node * node,
                          const std::string & name = "" );
wxTreeItemId OSGWXTREE_EXPORT addTreeItem( wxTreeCtrl * tree,
                          wxTreeItemId parent,
                          osg::Node * node,
                          const std::string & name = "" );


} // end namespace osgWxTree

#endif // __OSGWXBULLET_UTILS_H__
