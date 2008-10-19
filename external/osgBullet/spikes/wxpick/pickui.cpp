#include <iostream>

#include <boost/foreach.hpp>

#include <osg/io_utils>

#include <pickui.h>
#include <wxboUtil.h>

namespace isu {

bool BulletWXPickHandler::handle( const osgGA::GUIEventAdapter & ea,
                                  osgGA::GUIActionAdapter & aa )
{
    bool handled = false;
    switch( ea.getEventType() )
    {
        // case ( osgGA::GUIEventAdapter::PUSH ):
        // {
        //     osgViewer::View * view = dynamic_cast< osgViewer::View * >( &aa );
        //     if( view ) pick( view, ea );
        //     handled = false;
        // }
        // break;

        case ( osgGA::GUIEventAdapter::KEYDOWN ):
        {
            char key = ea.getKey();
            switch( key )
            {

                case 'i':
                {
                    // do the pick
                    osgViewer::View * view = dynamic_cast< osgViewer::View * >( &aa );
                    osg::Vec3d pickpoint( 0, 0, 0 );
                    if( view )
                    {
                        osgUtil::LineSegmentIntersector::Intersection ii = pick( view, ea, pickpoint );
                        if( pickpoint != osg::Vec3d( 0, 0, 0 ) )
                        {
                            std::vector< osg::Node * >::reverse_iterator pathend = ii.nodePath.rbegin();
                            pathend++;
                            isu::setTreeToFlatWithParent( tree_, *pathend );
                            // BOOST_FOREACH( osg::Node * node, ii.nodePath )
                            // {
                            //     std::cout << node << ": " << node->className() << " : " << node->getName() << std::endl;
                            // }
                            // std::cout << pickpoint << std::endl;
                        }
                        handled = true;
                    }
                }
                break;

                case '[':
                {
                    handled = true;
                }
                break;

                case ']':
                {
                    handled = true;
                }
                break;

                case 'o':
                {
                    handled = true;
                }
                break;
            }
        }
        break;
    }
    return( handled );
}

osgUtil::LineSegmentIntersector::Intersection BulletWXPickHandler::pick( osgViewer::View * view,
                                                                         const osgGA::GUIEventAdapter & ea,
                                                                         osg::Vec3d & pickpoint )
{
    osgUtil::LineSegmentIntersector::Intersection closest;
    osgUtil::LineSegmentIntersector::Intersections intersections;
    if( view->computeIntersections( ea.getX(), ea.getY(), intersections ) )
    {
        if( !intersections.empty() )
        {
            closest = *( intersections.begin() );
            pickpoint = closest.getWorldIntersectPoint();
        }
    }
    return( closest );
}

} // end namespace isu

