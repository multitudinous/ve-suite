/* -*-c++-*- OpenSceneGraph - Copyright (C) 1998-2006 Robert Osfield 
 *
 * This application is open source and may be redistributed and/or modified   
 * freely and without restriction, both in commericial and non commericial applications,
 * as long as this copyright notice is maintained.
 * 
 * This application is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

// oqTest01
//
// This test code demonstrates use of the PerDrawableQueryVisitor to
//   reorganize a scene graph so that large Drawables can be occlusion
//   tested. It uses the OcclusionQueryFlatVisitor to automatically
//   insert OcclusionQueryNodes where needed.

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgUtil/SmoothingVisitor>
#include <osgUtil/Optimizer>
#include <osg/BoundingSphere>
#include <osg/ShapeDrawable>

#include <osg/MatrixTransform>
#include <osg/Switch>

#include <osgViewer/Viewer>
#include <osgViewer/ViewerEventHandlers>

#include <osgGA/TrackballManipulator>
#include <osgGA/FlightManipulator>
#include <osgGA/DriveManipulator>
#include <osgGA/KeySwitchMatrixManipulator>
#include <osgGA/StateSetManipulator>
#include <osgGA/AnimationPathManipulator>
#include <osgGA/TerrainManipulator>

#include "osgOQ/OcclusionQueryVisitor.h"

#include <list>
#include <iostream>
#include <sstream>



void setOctohedronVertices( osg::Geometry& geom, const osg::Vec3& center, float radius );

class KeyHandler : public osgGA::GUIEventHandler 
{
public: 
    KeyHandler( osg::Node& node )
      : _node( node ),
        _enable( true ),
        _debug( false )
    {}

    bool handle( const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& )
    {
        switch( ea.getEventType() )
        {
            case(osgGA::GUIEventAdapter::KEYUP):
            {
                if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F6)
                {
                    // F6 -- Toggle osgOQ testing.
                    _enable = !_enable;
                    osgOQ::EnableQueryVisitor eqv( _enable );
                    _node.accept( eqv );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F7)
                {
                    // F7 -- Toggle display of OQ test bounding volumes
                    _debug = !_debug;
                    osgOQ::DebugDisplayVisitor ddv( _debug );
                    _node.accept( ddv );
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F8)
                {
                    // F8 -- Gether stats and display
                    osgOQ::StatisticsVisitor sv;
                    _node.accept( sv );
                    std::cout << "osgOQ: Stats: numOQNs " << sv.getNumOQNs() << ", numPased " << sv.getNumPassed() << std::endl;
                    return true;
                }
                else if (ea.getKey()=='o')
                {
                    if (osgDB::writeNodeFile( _node, "saved_model.osg" ))
                        osg::notify( osg::ALWAYS ) << "osgOQ: Wrote scene graph to \"saved_model.osg\"" << std::endl;
                    else
                        osg::notify( osg::ALWAYS ) << "osgOQ: Wrote failed for \"saved_model.osg\"" << std::endl;
                    return true;
                }
                return false;
            }
            default:
                break;
        }
        return false;
    }

    osg::Node& _node;

    bool _enable, _debug;
};

void setOctohedronVertices( osg::Geometry& geom, const osg::Vec3& center, float radius )
{
    osg::ref_ptr<osg::Vec3Array> v = new osg::Vec3Array;
    v->push_back( osg::Vec3( center[0], center[1]-radius, center[2] ) );
    v->push_back( osg::Vec3( center[0]+radius, center[1], center[2] ) );
    v->push_back( osg::Vec3( center[0], center[1]+radius, center[2] ) );
    v->push_back( osg::Vec3( center[0]-radius, center[1], center[2] ) );
    v->push_back( osg::Vec3( center[0], center[1], center[2]+radius ) );
    v->push_back( osg::Vec3( center[0], center[1], center[2]-radius ) );

    geom.setVertexArray( v.get() );
}

osg::ref_ptr<osg::Geometry>
createOctohedron( const osg::Vec3& center, float radius, const osg::Vec4& color )
{
    osg::ref_ptr<osg::Vec4Array> c = new osg::Vec4Array;
    c->push_back( color );

    osg::ref_ptr<osg::Vec3Array> n = new osg::Vec3Array;
    n->push_back( osg::Vec3( 1., -1., 1. ) / 1.732 );
    n->push_back( osg::Vec3( 1., -1., -1. ) / 1.732 );
    n->push_back( osg::Vec3( 1., 1., -1. ) / 1.732 );
    n->push_back( osg::Vec3( 1., 1., 1. ) / 1.732 );
    n->push_back( osg::Vec3( -1., -1., 1. ) / 1.732 );
    n->push_back( osg::Vec3( -1., -1., -1. ) / 1.732 );
    n->push_back( osg::Vec3( -1., 1., -1. ) / 1.732 );
    n->push_back( osg::Vec3( -1., 1., 1. ) / 1.732 );

    unsigned short idx[] = {
        0, 1, 4,
        1, 0, 5,
        2, 1, 5,
        1, 2, 4,
        3, 0, 4,
        0, 3, 5,
        3, 2, 5,
        2, 3, 4 };

    osg::ref_ptr<osg::Geometry> geom = new osg::Geometry;
    setOctohedronVertices( *geom, center, radius );
    geom->setColorArray( c.get() );
    geom->setColorBinding( osg::Geometry::BIND_OVERALL );
    geom->setNormalArray( n.get() );
    geom->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    geom->addPrimitiveSet( new osg::DrawElementsUShort( GL_TRIANGLES, 24, idx ) );

    return geom.get();
}

osg::ref_ptr<osg::Node>
createScene()
{
    osg::ref_ptr<osg::Group> grp = new osg::Group;

    osg::Node* node = osgDB::readNodeFile( "bigsphere.osg" );
    osg::Geode* geode = dynamic_cast< osg::Geode* >( node );
    if (!geode)
    {
        osg::notify( osg::FATAL ) << " Can't load data file." << std::endl;
        return NULL;
    }
    osg::PolygonMode* pm = new osg::PolygonMode(
        osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
    geode->getOrCreateStateSet()->setAttributeAndModes( pm, osg::StateAttribute::ON |
        osg::StateAttribute::PROTECTED);

    osg::ref_ptr<osg::Geometry> draw = createOctohedron(
        osg::Vec3( -2.5f, .5f, 1.f ), 1., osg::Vec4( 1.f, .5f, 1.f, 1.f ) );
    geode->addDrawable( draw.get() );

    draw = createOctohedron(
        osg::Vec3( 2.5f, -1.f, .5f ), 1.f, osg::Vec4( 1.f, 1.f, 1.f, 1.f ) );
    geode->addDrawable( draw.get() );

    grp->addChild( geode );



    osg::ref_ptr<osg::TessellationHints> hints = new osg::TessellationHints;
    hints->setDetailRatio( 5.f );

    geode = new osg::Geode;

    osg::ref_ptr<osg::ShapeDrawable> shape = new osg::ShapeDrawable(
    new osg::Box( osg::Vec3( 0.f, 0.f, -1.f ), 10.f, 10.f, .1f ), hints.get() );
    shape->setColor( osg::Vec4( 1.f, 1.f, 1.f, 1.f ) );
    geode->addDrawable( shape.get() );

    grp->addChild( geode );

    return (osg::Node*)grp.get();
}



int main(int argc, char** argv)
{
    // use an ArgumentParser object to manage the program arguments.
    osg::ArgumentParser arguments(&argc,argv);

    arguments.getApplicationUsage()->setApplicationName(arguments.getApplicationName());
    arguments.getApplicationUsage()->setDescription(arguments.getApplicationName()+" is the standard OpenSceneGraph example which loads and visualises 3d models.");
    arguments.getApplicationUsage()->setCommandLineUsage(arguments.getApplicationName()+" [options] filename ...");
    arguments.getApplicationUsage()->addCommandLineOption("--image <filename>","Load an image and render it on a quad");
    arguments.getApplicationUsage()->addCommandLineOption("--dem <filename>","Load an image/DEM and render it on a HeightField");
    arguments.getApplicationUsage()->addCommandLineOption("-h or --help","Display command line parameters");
    arguments.getApplicationUsage()->addCommandLineOption("--help-env","Display environmental variables available");
    arguments.getApplicationUsage()->addCommandLineOption("--help-keys","Display keyboard & mouse bindings available");
    arguments.getApplicationUsage()->addCommandLineOption("--help-all","Display all command line, env vars and keyboard & mouse bindings.");
    arguments.getApplicationUsage()->addCommandLineOption("--SingleThreaded","Select SingleThreaded threading model for viewer.");
    arguments.getApplicationUsage()->addCommandLineOption("--CullDrawThreadPerContext","Select CullDrawThreadPerContext threading model for viewer.");
    arguments.getApplicationUsage()->addCommandLineOption("--DrawThreadPerContext","Select DrawThreadPerContext threading model for viewer.");
    arguments.getApplicationUsage()->addCommandLineOption("--CullThreadPerCameraDrawThreadPerContext","Select CullThreadPerCameraDrawThreadPerContext threading model for viewer.");

    arguments.getApplicationUsage()->addCommandLineOption("--stock","Display the stock scene.");
    arguments.getApplicationUsage()->addCommandLineOption("--opt","Run the osgUtil Optimizer.");

    // if user request help write it out to cout.
    bool helpAll = arguments.read("--help-all");
    unsigned int helpType = ((helpAll || arguments.read("-h") || arguments.read("--help"))? osg::ApplicationUsage::COMMAND_LINE_OPTION : 0 ) |
                            ((helpAll ||  arguments.read("--help-env"))? osg::ApplicationUsage::ENVIRONMENTAL_VARIABLE : 0 ) |
                            ((helpAll ||  arguments.read("--help-keys"))? osg::ApplicationUsage::KEYBOARD_MOUSE_BINDING : 0 );
    if (helpType)
    {
        arguments.getApplicationUsage()->write(std::cout, helpType);
        return 1;
    }

    // report any errors if they have occurred when parsing the program arguments.
    if (arguments.errors())
    {
        arguments.writeErrorMessages(std::cout);
        return 1;
    }
    
    osgViewer::Viewer viewer( arguments );
    
    // set up the camera manipulators.
    {
        osg::ref_ptr<osgGA::KeySwitchMatrixManipulator> keyswitchManipulator = new osgGA::KeySwitchMatrixManipulator;

        keyswitchManipulator->addMatrixManipulator( '1', "Trackball", new osgGA::TrackballManipulator() );
        keyswitchManipulator->addMatrixManipulator( '2', "Flight", new osgGA::FlightManipulator() );
        keyswitchManipulator->addMatrixManipulator( '3', "Drive", new osgGA::DriveManipulator() );
        keyswitchManipulator->addMatrixManipulator( '4', "Terrain", new osgGA::TerrainManipulator() );

        std::string pathfile;
        char keyForAnimationPath = '5';
        while (arguments.read("-p",pathfile))
        {
            osgGA::AnimationPathManipulator* apm = new osgGA::AnimationPathManipulator(pathfile);
            if (apm || !apm->valid()) 
            {
                unsigned int num = keyswitchManipulator->getNumMatrixManipulators();
                keyswitchManipulator->addMatrixManipulator( keyForAnimationPath, "Path", apm );
                keyswitchManipulator->selectMatrixManipulator(num);
                ++keyForAnimationPath;
            }
        }

        viewer.setCameraManipulator( keyswitchManipulator.get() );
    }

    // add the state manipulator
    viewer.addEventHandler( new osgGA::StateSetManipulator(viewer.getCamera()->getOrCreateStateSet()) );
    
    // add the thread model handler
    viewer.addEventHandler(new osgViewer::ThreadingHandler);

    // add the window size toggle handler
    viewer.addEventHandler(new osgViewer::WindowSizeHandler);

    // add the stats handler
    viewer.addEventHandler(new osgViewer::StatsHandler);

    // add the help handler
    viewer.addEventHandler(new osgViewer::HelpHandler(arguments.getApplicationUsage()));

    // add the record camera path handler
    viewer.addEventHandler(new osgViewer::RecordCameraPathHandler);

    osg::ref_ptr<osgDB::ReaderWriter::Options> opts =
        new osgDB::ReaderWriter::Options();
    // load the specified model
    osg::ref_ptr<osg::Node> loadedModel = osgDB::readNodeFiles( arguments, opts.get() );
    if (!loadedModel) 
        loadedModel = createScene();

    // One of the Geodes in the loadedModel has three Drawables.
    //   Two are simple octohedrons, the third is the complex sphere.
    // To occlusion query only the sphere, the scene graph must be
    //   reorganized so that the sphere Drawable has its own Geode.
    osgOQ::PerDrawableQueryVisitor pdqv;
    loadedModel->accept( pdqv );
    // Then run a visitor to add OQNs, and it will only place an OQN
    //   above the Geode that owns the sphere.
    osgOQ::OcclusionQueryFlatVisitor oqfv;
    loadedModel->accept( oqfv );


    bool optimize = arguments.read( "--opt" );

    // any option left unread are converted into errors to write out later.
    arguments.reportRemainingOptionsAsUnrecognized();

    // report any errors if they have occurred when parsing the program arguments.
    if (arguments.errors())
    {
        arguments.writeErrorMessages(std::cout);
        return 1;
    }


    // optimize the scene graph, remove redundant nodes and state etc.
    if (optimize)
    {
        osgUtil::Optimizer optimizer;
        optimizer.optimize( loadedModel.get() );
    }
    viewer.addEventHandler(new KeyHandler( *loadedModel ));

    viewer.setUpViewOnSingleScreen( 0 );
    viewer.setSceneData( loadedModel.get() );
    return viewer.run();
}
