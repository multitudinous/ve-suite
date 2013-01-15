#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>
#include <boost/config.hpp>
#ifdef BOOST_WINDOWS
# pragma warning(disable: 4275)
#else
#include <kibitz/GNUCompilerGuards.hpp>
GCC_DIAG_OFF( unused-parameter )
#endif

#include <boost/program_options.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

#ifdef BOOST_WINDOWS
# pragma warning(default: 4275)
#else
GCC_DIAG_ON( unused-parameter )
#endif

// --- ZMQ Includes --- //
#include "zmq.hpp"

#include <string>


namespace po = boost::program_options;
namespace pt = boost::property_tree;

std::string to_json()
{
    std::stringstream stm;
    pt::ptree tree;
    tree.put( "message_type", "data" );
    tree.put( "version", 1.0 );
    tree.put( "notification_type", "new data" );
    tree.put( "worker_type", "opc client" );
    tree.put( "worker_id", 1 );
    tree.put( "host_name", "localhost" );
    tree.put( "port", 3098 );
    boost::property_tree::json_parser::write_json( stm, tree );
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

    //Init zeromq context
    zmq::context_t context( 1 );

    //Socket for worker control
    zmq::socket_t controller( context, ZMQ_PUB );
    controller.bind( "tcp://*:3098" );

    bool kill_msg( false );
    while( !kill_msg )
    {
        //Get OPC data here
        bool valid_msg = true;

        //If our message is formatted correctly, send
        if( valid_msg )
        {
            std::string str = to_json();
            zmq::message_t zmq_msg;
            zmq_msg.rebuild( str.size() );
            //zmq_msg.rebuild( str.size() );
            memcpy( zmq_msg.data(), str.data(), str.size() );
            assert( controller.send( zmq_msg ) );
        }
    }

    return 0;
}
