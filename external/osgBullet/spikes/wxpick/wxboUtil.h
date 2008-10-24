#ifndef __WXBO_UTIL_H__
#define __WXBO_UTIL_H__

#include <wx/treectrl.h>
#include <osg/NodeVisitor>
#include <osg/Group>

namespace isu {

class osgNodeItemData
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

class PopulateTreeControlWithNodeVisitor
    : public osg::NodeVisitor
{
public:
    PopulateTreeControlWithNodeVisitor( wxTreeCtrl * tree, bool recurse = false )
        : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
        , tree_( tree )
        , recurse_( recurse )
        , proc_( 0 )
        , activeGroup_( NULL )
    {
    }

    virtual void apply( osg::Node & node );
    virtual void apply( osg::Group & group );
private:
    void PopulateTreeControlWithNodeVisitor::addChildren( osg::Group& group );

    wxTreeCtrl *    tree_;
    bool recurse_;
    unsigned int proc_;

    wxTreeItemId activeGroup_;
    wxTreeItemId activeChild_;
};

wxTreeItemId setTreeToFlatWithParent( wxTreeCtrl * tree,
                                      osg::Node * node,
                                      const std::string & name = "" );
wxTreeItemId setTreeToFlatGroup( wxTreeCtrl * tree,
                                 osg::Group * group,
                                 osg::Node * selected = NULL,
                                 const std::string & name = "" );
wxTreeItemId setTreeRoot( wxTreeCtrl * tree,
                          osg::Node * node,
                          const std::string & name = "" );
wxTreeItemId addTreeItem( wxTreeCtrl * tree,
                          wxTreeItemId parent,
                          osg::Node * node,
                          const std::string & name = "" );

} // end namespace isu

#endif // __WXBO_UTIL_H__
