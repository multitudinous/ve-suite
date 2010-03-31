//
// Copyright (c) 2008 Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osgDB/ReadFile>
#include <osgDB/FileUtils>
#include <osgViewer/Viewer>
#include <osgViewer/ViewerEventHandlers>
#include <osgGA/TrackballManipulator>

#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/BlendFunc>
#include <osg/Depth>
#include <osg/Point>
#include <osg/PointSprite>
#include <osg/AlphaFunc>

#include <osgwTools/Shapes.h>

osg::ref_ptr< osg::Group > root;
osg::ref_ptr< osg::Group > largeSphere;

// Allows you to change the animation play rate:
//   '+' speed up
//   '-' slow down
//   'p' pause
class PlayStateHandler
: public osgGA::GUIEventHandler
{
public:
    PlayStateHandler()
    : elapsedTime( 0. ),
    scalar( 1. ),
    paused( false )
    {}
    
    double getCurrentTime() const
    {
        if( paused )
            return( elapsedTime );
        else
            return( elapsedTime + ( timer.time_s() * scalar ) );
    }
    
    virtual bool handle( const osgGA::GUIEventAdapter & event_adaptor,
                        osgGA::GUIActionAdapter & action_adaptor )
    {
        bool handled = false;
        switch( event_adaptor.getEventType() )
        {
            case ( osgGA::GUIEventAdapter::KEYDOWN ):
            {
                int key = event_adaptor.getKey();
                switch( key )
                {
                    case 'r': // speed up
                    {
                        root->removeChild( largeSphere.get() );
                        
                        handled = true;
                    }
                        break;
                    case 'a': // slow down
                    {
                        if( largeSphere->getNumParents() == 0 )
                        {
                            root->addChild( largeSphere.get() ); 
                        }
                        
                        handled = true;
                    }
                        break;
                }
            }
        }
        return( handled );
    }
    
private:
    osg::Timer timer;
    double elapsedTime;
    double scalar;
    bool paused;
};



// Make some opaque boxes to show that depth testing works properly.
osg::Group*
createSphere( double radius )
{
    osg::Geometry* geom = new osg::Geometry();
    
    osgwTools::makeAltAzSphere( radius, 4., 6., geom );
    
    osg::ref_ptr< osg::Group > grp = new osg::Group;
    osg::Geode* geode = new osg::Geode;
    grp->addChild( geode );
    
    geode->addDrawable( geom );
    
    return( grp.release() );
}

int
main( int argc,
     char ** argv )
{
    largeSphere = createSphere( 1500000.0 );
    root = new osg::Group;
    root->addChild( largeSphere.get() );
    root->addChild( createSphere( 10.0 ) );
    
    osgViewer::Viewer viewer;
    viewer.addEventHandler( new osgViewer::StatsHandler );
    viewer.setUpViewOnSingleScreen( 0 );
    viewer.setSceneData( root.get() );
    
    viewer.setCameraManipulator( new osgGA::TrackballManipulator );
    
    // Create a PlayStateHandler to track elapsed simulation time
    // and play/pause state.
    osg::ref_ptr< PlayStateHandler > psh = new PlayStateHandler;
    viewer.addEventHandler( psh.get() );
    
    while (!viewer.done())
    {
        // Get time from the PlayStateHandler.
        double simTime = psh->getCurrentTime();
        viewer.frame( simTime );
    }
    
    return( 0 );
}

