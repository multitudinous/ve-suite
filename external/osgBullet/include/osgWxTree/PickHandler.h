//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#ifndef __OSGWXTREE_PICK_HANDLER_H__
#define __OSGWXTREE_PICK_HANDLER_H__ 1

#include <osg/Group>
#include <osgGA/GUIEventHandler>
#include <osgViewer/Viewer>

#include <wx/treectrl.h>

#include <osgWxTree/Export.h>


namespace osgWxTree {


class OSGWXTREE_EXPORT PickHandler
    : public osgGA::GUIEventHandler
{
public:
    PickHandler( wxTreeCtrl * tree )
        : tree_( tree )
    {
    }

    ~PickHandler() {
    }

    bool handle( const osgGA::GUIEventAdapter & ea,
                 osgGA::GUIActionAdapter & aa );

    osgUtil::LineSegmentIntersector::Intersection pick( osgViewer::View * view,
                                                        const osgGA::GUIEventAdapter & ea,
                                                        osg::Vec3d & pickpoint );
protected:
    wxTreeCtrl * tree_;
    osg::ref_ptr< osg::Group > root_;
};


} // end namespace osgWxTree

#endif // __OSGWXTREE_PICK_HANDLER_H__
