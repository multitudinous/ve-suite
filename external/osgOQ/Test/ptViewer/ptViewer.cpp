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

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgUtil/SmoothingVisitor>
#include <osgUtil/Optimizer>
#include <osg/BoundingSphere>

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


// Store a list of MatrixTransforms that the F5 key will translate.
typedef std::list< osg::ref_ptr< osg::MatrixTransform > > MatrixTransformList;
MatrixTransformList mtList;
int mtShift( 0 );

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
                if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F4)
                {
                    // F4 -- Change transforms in the stock scene.
                    MatrixTransformList::const_iterator mtIt = mtList.begin();
                    while (mtIt != mtList.end())
                    {
                        osg::MatrixTransform* mt = (*mtIt).get();
                        osg::Matrix m;
                        m.makeTranslate( (float)mtShift++, 0.f, 0.f );
                        mtShift = mtShift % 4;
                        mt->setMatrix( m );

                        mtIt++;
                    }
                    return true;
                }
                else if (ea.getKey()==osgGA::GUIEventAdapter::KEY_F6)
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

osg::ref_ptr<osg::Node>
createScene()
{
    osg::BoundingSphere bs;
    bs._radius = 2.f;

    osg::ref_ptr<osg::Group> root = new osg::Group;

    // Add complex geometry
    osg::ref_ptr<osg::Group> g = new osg::Group;
    {
        osg::ref_ptr<osg::Node> node = osgDB::readNodeFile( "bigsphere.osg" );
        if (!node.valid())
        {
            osg::notify( osg::FATAL ) << "osgOQ: Can't load \"bigsphere.osg\"." << std::endl;
            return NULL;
        }

        osg::StateSet* ss = node->getOrCreateStateSet();
        osg::PolygonMode* pm = new osg::PolygonMode(
            osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
        ss->setAttributeAndModes( pm, osg::StateAttribute::ON |
            osg::StateAttribute::PROTECTED);

        osgUtil::SmoothingVisitor sv;
        node->accept( sv );

        osg::MatrixTransform* mt = new osg::MatrixTransform;
        mt->setDataVariance( osg::Object::DYNAMIC );
        osg::Matrix m;
        m.makeTranslate( 0.f, 0.f, 0.f );
        mt->setMatrix( m );
        g->addChild( mt );
        mt->addChild( node.get() );

        mtList.push_back( mt );
    }
    root->addChild( g.get() );

    // Add box
    osg::ref_ptr<osg::Geode> box = new osg::Geode;
    osg::StateSet* state = box->getOrCreateStateSet();
    osg::PolygonMode* pm = new osg::PolygonMode( 
        osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::FILL );
    state->setAttributeAndModes( pm,
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    osg::ref_ptr<osg::Geometry> geom = new osg::Geometry;
    osg::ref_ptr<osg::Vec3Array> v = new osg::Vec3Array;
    geom->setVertexArray( v.get() );

    {
        const float x(bs._center.x());
        const float y(bs._center.y());
        const float z(bs._center.z());
        const float r(bs._radius);

        v->push_back( osg::Vec3( x-r, y-r, z-r ) ); //left -X
        v->push_back( osg::Vec3( x-r, y-r, z+r ) );
        v->push_back( osg::Vec3( x-r, y+r, z+r ) );
        v->push_back( osg::Vec3( x-r, y+r, z-r ) );

        v->push_back( osg::Vec3( x+r, y-r, z+r ) ); //right +X
        v->push_back( osg::Vec3( x+r, y-r, z-r ) );
        v->push_back( osg::Vec3( x+r, y+r, z-r ) );
        v->push_back( osg::Vec3( x+r, y+r, z+r ) );

        v->push_back( osg::Vec3( x-r, y-r, z-r ) ); // bottom -Z
        v->push_back( osg::Vec3( x-r, y+r, z-r ) );
        v->push_back( osg::Vec3( x+r, y+r, z-r ) );
        v->push_back( osg::Vec3( x+r, y-r, z-r ) );

        v->push_back( osg::Vec3( x-r, y-r, z+r ) ); // top +Z
        v->push_back( osg::Vec3( x+r, y-r, z+r ) );
        v->push_back( osg::Vec3( x+r, y+r, z+r ) );
        v->push_back( osg::Vec3( x-r, y+r, z+r ) );

        v->push_back( osg::Vec3( x-r, y+r, z-r ) ); // back +Y
        v->push_back( osg::Vec3( x-r, y+r, z+r ) );
        v->push_back( osg::Vec3( x+r, y+r, z+r ) );
        v->push_back( osg::Vec3( x+r, y+r, z-r ) );
    }

    osg::ref_ptr<osg::Vec4Array> c = new osg::Vec4Array;
    geom->setColorArray( c.get() );
    geom->setColorBinding( osg::Geometry::BIND_OVERALL );
    c->push_back( osg::Vec4( 0.f, 1.f, 1.f, 1.f ) );

    osg::ref_ptr<osg::Vec3Array> n = new osg::Vec3Array;
    geom->setNormalArray( n.get() );
    geom->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );
    n->push_back( osg::Vec3( -1.f, 0.f, 0.f ) );
    n->push_back( osg::Vec3( 1.f, 0.f, 0.f ) );
    n->push_back( osg::Vec3( 0.f, 0.f, -1.f ) );
    n->push_back( osg::Vec3( 0.f, 0.f, 1.f ) );
    n->push_back( osg::Vec3( 0.f, 1.f, 0.f ) );

    geom->addPrimitiveSet( new osg::DrawArrays( GL_QUADS, 0, 20 ) );
    box->addDrawable( geom.get() );

    root->addChild( box.get() );

    return root.get();
}

osg::ref_ptr<osg::Group>
createStockScene()
{
    osg::ref_ptr<osg::Group> root = new osg::Group();

    for (int i=0; i<3; i++)
    {
        osg::MatrixTransform* mt = new osg::MatrixTransform;
        osg::Matrix m;
        switch (i)
        {
        case 0: m.makeTranslate( 0.f, 0.f, 4.5f ); break;
        case 1: m.makeTranslate( 0.f, 0.f, -4.5f ); break;
        case 2: m.makeTranslate( 0.f, 0.f, 0.f ); break;
        }
        mt->setMatrix( m );
        mt->addChild( createScene().get() );
        root->addChild( mt );
    }

    return root.get();
}


int main(int argc, char** argv)
{
    // Force load of osgPolyTrans plugin
#ifdef _DEBUG
    const std::string pluginName( "osgdb_PolyTransd.dll" );
#else
    const std::string pluginName( "osgdb_PolyTrans.dll" );
#endif
    bool loadedLib = osgDB::Registry::instance()->loadLibrary( pluginName );
    if (!loadedLib)
    {
        osg::notify( osg::FATAL ) << "osgOQ: Can't load plugin \"" << pluginName << "\"." << std::endl;
    }
    // Now that we force load, probably don't need the extension map any longer.
    //osgDB::Registry::instance()->readPluginAliasConfigurationFile( "extmap.txt" );

    
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

#if 0
    while (arguments.read("--SingleThreaded")) viewer.setThreadingModel(osgViewer::Viewer::SingleThreaded);
    while (arguments.read("--CullDrawThreadPerContext")) viewer.setThreadingModel(osgViewer::Viewer::CullDrawThreadPerContext);
    while (arguments.read("--DrawThreadPerContext")) viewer.setThreadingModel(osgViewer::Viewer::DrawThreadPerContext);
    while (arguments.read("--CullThreadPerCameraDrawThreadPerContext")) viewer.setThreadingModel(osgViewer::Viewer::CullThreadPerCameraDrawThreadPerContext);
#endif

    std::string optStr;
#if 0
    // TBD
    // Pass command line arguments to osgdb_PolyTrans here.
    osg::ref_ptr<osgOQ::OcclusionQueryContext> oqc = new osgOQ::OcclusionQueryContext;
    int buffers = oqc->getBufferSize();
    if (arguments.read( "--BufferSize", buffers ))
    {
        oqc->setBufferSize( buffers );

        std::ostringstream oStr;
        oStr << "BufferSize " << buffers;
        optStr += oStr.str();
    }
#endif

    osg::ref_ptr<osg::Group> root;
    if (arguments.read( "--stock" ))
        // User requested the stock scene
        root = createStockScene().get();
    else
    {
        osg::ref_ptr<osgDB::ReaderWriter::Options> opts =
            new osgDB::ReaderWriter::Options( optStr );
        // load the specified model
        root = static_cast<osg::Group*>( osgDB::readNodeFiles( arguments, opts.get() ) );
        if (!root) 
        {
            std::cout << arguments.getApplicationName() <<": No data loaded" << std::endl;
            return 1;
        }
    }
    osgOQ::OcclusionQueryFlatVisitor oqfv;
    root->accept( oqfv );

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
        optimizer.optimize( root.get() );
    }

    viewer.setSceneData( root.get() );

    KeyHandler* kh = new KeyHandler( *root );
    viewer.addEventHandler( kh );

    return viewer.run();
}
