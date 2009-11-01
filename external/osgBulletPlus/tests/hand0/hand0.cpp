// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osgGA/TrackballManipulator>

#include <osgbBulletPlus/HandNode.h>


class HandManipulator : public osgGA::GUIEventHandler
{
public:
    HandManipulator( osgbBulletPlus::HandNode* hn )
      : _hand( hn ),
        _mode( osgbBulletPlus::HandNode::FINGER_0_TRANSLATE ),
        _h( 0.f ),
        _p( 0.f ),
        _r( 0.f )
    {
    }

    bool handle( const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& )
    {
        const unsigned int mod = ea.getModKeyMask();
        const bool ctrl = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_CTRL) ||
            (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_CTRL) );
        const bool shift = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_SHIFT) ||
            (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_SHIFT) );

        const unsigned int buttonMask( ea.getButtonMask() );
        const bool ourLeft( (ctrl || shift) && (buttonMask == osgGA::GUIEventAdapter::LEFT_MOUSE_BUTTON) );

        switch( ea.getEventType() )
        {
            case osgGA::GUIEventAdapter::KEYUP:
            {
                if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Home)
                {
                    _hand->setPose( osgbBulletPlus::HandNode::POSE_DEFAULT );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_End)
                {
                    _hand->setPose( osgbBulletPlus::HandNode::POSE_HOOK );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Up)
                {
                    _hand->setPose( osgbBulletPlus::HandNode::POSE_POINT );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Page_Down)
                {
                    _hand->setPose( osgbBulletPlus::HandNode::POSE_FIST );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Delete)
                {
                    _hand->dump();
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F1)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_0_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F2)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_1_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F3)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_2_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F4)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_3_TRANSLATE;
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F5)
                {
                    _mode = osgbBulletPlus::HandNode::FINGER_4_TRANSLATE;
                    return true;
                }

                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Left)
                {
                    _hand->setArticulation( _mode,
                        _hand->getArticulation( _mode ) + 0.1 );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_Right)
                {
                    _hand->setArticulation( _mode,
                        _hand->getArticulation( _mode ) - 0.1 );
                    return true;
                }
                return false;
            }

            case osgGA::GUIEventAdapter::SCROLL:
            {
                const unsigned int mod = ea.getModKeyMask();
                const bool k1 = ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_CTRL) ||
                    (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_CTRL) );
                const bool k0 = ( !k1 || ( (mod&osgGA::GUIEventAdapter::MODKEY_LEFT_ALT) ||
                    (mod&osgGA::GUIEventAdapter::MODKEY_RIGHT_ALT) ) );

                float delta( 0.05 );
                osgGA::GUIEventAdapter::ScrollingMotion sm = ea.getScrollingMotion();
                if (sm == osgGA::GUIEventAdapter::SCROLL_UP)
                    delta = -delta;

                if (k0) _hand->setArticulation( _mode + 5 , _hand->getArticulation( _mode+5  ) + delta );
                if (k1) _hand->setArticulation( _mode + 10, _hand->getArticulation( _mode+10 ) + delta );
                return true;
            }
            case osgGA::GUIEventAdapter::PUSH:
            {
                if( !ourLeft )
                    return false;

                _lastX = ea.getXnormalized();
                _lastY = ea.getYnormalized();
                return true;
            }
            case osgGA::GUIEventAdapter::DRAG:
            {
                if( !ourLeft )
                    return false;

                if( ctrl )
                {
                    // X = Heading
                    // Y = Pitch
                    _h += ( _lastX - ea.getXnormalized() ) * 2.;
                    _p += ( _lastY - ea.getYnormalized() ) * 2.;
                }
                else if( shift )
                {
                    // X = Roll
                    _r += ( _lastX - ea.getXnormalized() ) * 2.;
                }
                _lastX = ea.getXnormalized();
                _lastY = ea.getYnormalized();

#if 1
                // We now have h, p, and r angles. Build a Quat to affect these rotatiions.
                // We do this by creating a Matrix that contains correctly-oriented x, y, and
                // z axes. Then we create the Quat from the Matrix.
                //
                // First, create x, y, and z axes that represent the h, p, and r angles.
                //   Rotate x and y axes by the heading.
                osg::Vec3 z( 0., 0., 1. );
                osg::Quat qHeading( _h, z );
                osg::Vec3 x = qHeading * osg::Vec3( 1., 0., 0. );
                osg::Vec3 y = qHeading * osg::Vec3( 0., 1., 0. );
                //   Rotate z and y axes by the pitch.
                osg::Quat qPitch( _p, x );
                y = qPitch * y;
                z = qPitch * z;
                //   Rotate x and z axes by the roll.
                osg::Quat qRoll( _r, y );
                x = qRoll * x;
                z = qRoll * z;
                // Use x, y, and z axes to create an orientation matrix.
                osg::Matrix m( x[0], x[1], x[2], 0.,
                    y[0], y[1], y[2], 0.,
                    z[0], z[1], z[2], 0.,
                    0., 0., 0., 1. );
                
                osg::Quat q;
                q.set( m );
#else
     osg::Vec3d pitch( 1, 0, 0 );
     osg::Vec3d roll( 0, 1, 0 );
     osg::Vec3d yaw( 0, 0, 1 );

     osg::Matrixd rotateMat;
     rotateMat.makeRotate( _h, yaw,
                          _p, pitch,
                          _r, roll );

     osg::Quat q;
     q = rotateMat.getRotate();
#endif
                _hand->setAttitude( q );
                return true;
            }
            default:
            break;
        }
        return false;
    }

protected:
    osg::ref_ptr< osgbBulletPlus::HandNode > _hand;
    osgbBulletPlus::HandNode::Articulation _mode;
    float _lastX, _lastY;
    float _h, _p, _r;
    osg::Vec3 _x, _y, _z;
};


int
main( int argc,
      char ** argv )
{
    osg::Group* root = new osg::Group;

    if( argc > 1 )
        root->addChild( osgDB::readNodeFile( argv[ 1 ] ) );

    osg::ref_ptr< osgbBulletPlus::HandNode > hn = new osgbBulletPlus::HandNode( NULL, osgbBulletPlus::HandNode::LEFT, 1. );
    root->addChild( hn.get() );

    hn->setPosition( osg::Vec3( 5., 0., 0. ) );
    hn->setDebug( true );

    osgViewer::Viewer viewer;
    viewer.setUpViewOnSingleScreen( 0 );
    viewer.setSceneData( root );
    viewer.setCameraManipulator( new osgGA::TrackballManipulator );
    viewer.addEventHandler( new HandManipulator( hn.get() ) );

    viewer.run();

    return 0;
}

