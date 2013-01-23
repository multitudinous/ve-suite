#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/config.hpp>
#ifdef BOOST_WINDOWS
# pragma warning(disable: 4275)
#else
#include <ves/util/GNUCompilerGuards.h>
DIAG_OFF( unused-parameter )
#endif

#include <boost/program_options.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

#ifdef BOOST_WINDOWS
# pragma warning(default: 4275)
#else
DIAG_ON( unused-parameter )
#endif

// --- ZMQ Includes --- //
#include "zmq.hpp"

#include <string>

#include "OPC.h"

namespace po = boost::program_options;
namespace pt = boost::property_tree;

std::string to_json()
{
    std::map< std::string, double > testMap;
    testMap[ "test1" ] = 1.0;
    testMap[ "test2" ] = 1.0;
    testMap[ "test3" ] = 1.0;
    testMap[ "test4" ] = 1.0;
    std::stringstream stm;
    pt::ptree tree;
    tree.put( "message_type", "data" );
    tree.put( "version", 1.0 );
    tree.put( "message.notification_type", "new data" );
    tree.put( "message.worker_type", "opc client" );
    tree.put( "message.worker_id", 1 );
    tree.put( "message.host_name", "localhost" );
    tree.put( "message.port", 3098 );
    pt::ptree maptree;
    for( std::map< std::string, double >::const_iterator iter = testMap.begin(); iter != testMap.end(); ++iter )
    {
        maptree.put( iter->first, iter->second );
    }
    tree.add_child( "message.payload", maptree );

    boost::property_tree::json_parser::write_json( stm, tree, true );
    return stm.str();
}

int main( int argc, char** argv )
{
    po::options_description desc( "Allowed options" );
    desc.add_options()
        ( "help", "Produce help message" )
        ( "writeConstants,w", po::bool_switch(), "Write the model constants out to the named config file" )
        ( "paintModel,m", po::value<std::string>(), "Set the paint model to use - airless, airassisted, hvlp, gravity, or electrostatic" )
        ( "debugScore,s", po::bool_switch(), "Write the image buffers to file to debug the scoring data" )
        ( "calibrateControls,c", po::bool_switch(), "Calibrate gun trigger and fan limits" )
        ( "calibratePosition,p", po::bool_switch(), "Calibrate gun position relative to screen" )
        ( "mode,d", po::value<std::string>()->default_value( "Practice" ), "Set application mode -- Practice, Competition, Kiosk, or Evaluation" )
        ( "vrpnServer,v", po::bool_switch(), "Run an integrated VRPN device server" )
        ( "vrpnLogging,l", po::value<std::string>(), "Either dump the vrpn data or playback the data for debugging - log, playback" );
    
    po::variables_map vm;
    po::store( po::parse_command_line( argc, argv, desc ), vm );
    po::notify( vm );
    
    if( vm.count( "help" ) )
    {
        std::cout << desc << std::endl;
        return 0;
    }
    
    //First connect to the opc server with the appropriate server name
    OPC opcInterface("");
    bool connectedToServer = opcInterface.ConnectToOPCServer();
    //Now get all of the current variables
    std::string xmlData = opcInterface.GetAllOPCVariables( "" );
    std::vector< std::pair< std::string, std::string > > rawDataVector = opcInterface.GetAllRawOPCData();
    
    //Init zeromq context
    zmq::context_t context( 1 );

    //Socket for worker control
    zmq::socket_t controller( context, ZMQ_PUB );
    controller.bind( "tcp://*:3098" );

    bool kill_msg( false );
    //while( !kill_msg )
    {
        //Get OPC data here
        bool valid_msg = true;

        //If our message is formatted correctly, send
        if( valid_msg )
        {
            std::string str = to_json();
            std::cout << str << std::endl;
            zmq::message_t zmq_msg;
            zmq_msg.rebuild( str.size() );
            //zmq_msg.rebuild( str.size() );
            memcpy( zmq_msg.data(), str.data(), str.size() );
            assert( controller.send( zmq_msg ) );
        }
    }

    return 0;
}
