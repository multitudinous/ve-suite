#ifndef __ISU_PICKGUIADAPTER_H__
#define __ISU_PICKGUIADAPTER_H__

#include <osg/Group>
#include <osgGA/GUIEventHandler>
#include <osgViewer/Viewer>

#include <wx/treectrl.h>

namespace isu {

class BulletWXPickHandler
    : public osgGA::GUIEventHandler
{
public:
    BulletWXPickHandler( wxTreeCtrl * tree )
        : tree_( tree )
    {
    }

    ~BulletWXPickHandler() {
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

} // end namespace isu


#endif /* __ISU_PICKGUIADAPTER_H__ */
