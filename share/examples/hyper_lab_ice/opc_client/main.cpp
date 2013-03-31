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
#include <boost/property_tree/ini_parser.hpp>
#include <boost/foreach.hpp>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

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
namespace fs = boost::filesystem;

std::map< std::string, std::string > variableMap;
    
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

std::string to_json( std::vector< std::pair< std::string, std::string > >& valVector )
{
    std::stringstream stm;
    pt::ptree maptree;
    for( std::vector< std::pair< std::string, std::string > >::const_iterator iter = valVector.begin(); iter != valVector.end(); ++iter )
    {
        maptree.put( variableMap[ iter->first ], iter->second );
    }
    
    boost::property_tree::json_parser::write_json( stm, maptree, true );
    return stm.str();
}

int main( int argc, char** argv )
{
    HRESULT hr = CoInitialize(NULL);//, COINIT_MULTITHREADED);

    /*{
        pt::ptree tree;
        {
        pt::ptree maptree;
        maptree.put( "opc", "VXM00012827.A1_A13_DI.ZSC_408.BI" );
        maptree.put( "id", "HV408" );
        tree.add_child( "variables", maptree );
        }
        {
        pt::ptree maptree;
        maptree.put( "opc", "VXM00012827.A1_A07_AIO.PT_406.AI_COMBO" );
        maptree.put( "id", "PT406" );
        tree.add_child( "variables", maptree );
        }

        std::stringstream stm;
        boost::property_tree::json_parser::write_json( stm, tree, true );
        //boost::property_tree::ini_parser::write_ini( stm, tree, 0 );
        std::cout << stm.str() << std::endl;
        std::ofstream variablesFile( "variable2.conf" );
        variablesFile << stm.str() << std::endl;
        variablesFile.close();
    }*/
    po::options_description desc( "Allowed options" );
    desc.add_options()
        ( "help,h", "OPC bridge tool command line help" )
        ( "serverName,s", po::value<std::string>()->default_value( "Softing.OPCToolboxDemo_ServerDA.1" ), "The OPC server name to connect to" )
        ( "variables,v", po::value< std::vector< std::string > >(), "The fully qualified variable names to monitor" )
        ( "broadcast,b", po::bool_switch(), "Broadcast the data to port 3098" )
        ( "configFile,f", po::value<std::string>()->default_value( "variable.conf" ), "A config file with variable pairs to monitor" )
        ( "logServerVars,l", po::bool_switch(), "Log all of the variables from the OPC server to a file - vesOpc.log" )
        ( "dataSource,d", po::value<std::string>()->default_value( "device" ), "The OPC hardware data source - device or cache" );
    
    po::variables_map vm;
    po::store( po::parse_command_line( argc, argv, desc ), vm );
    po::notify( vm );
    
    if( vm.count( "help" ) )
    {
        std::cout << desc << std::endl;
        return 0;
    }
    
    const std::string serverName = vm["serverName"].as<std::string>();
    const std::string dataSource = vm["dataSource"].as<std::string>();

    //First connect to the opc server with the appropriate server name
    OPC* opcInterface = new OPC("");
    opcInterface->SetOPCServerName( serverName );
    opcInterface->SetDeviceOrHardwareFlag( dataSource );
    
    bool connectedToServer = opcInterface->ConnectToOPCServer();
    
    //Now get all of the current variables
    if( vm[ "logServerVars" ].as<bool>() )
    {
        std::string xmlData = opcInterface->GetAllOPCVariables( "" );
        std::vector< std::pair< std::string, std::string > > rawDataVector = opcInterface->GetAllRawOPCData();
        std::vector< unsigned int > stateDataVector = opcInterface->GetAllStateOPCData();
        std::ofstream opcLog( "vesOpc.log" );
        for( std::vector< std::pair< std::string, std::string > >::const_iterator iter = rawDataVector.begin(); 
            iter != rawDataVector.end(); ++iter)
        {
            opcLog << iter->first << " " << iter->second << std::endl;
        }
        
        opcLog << std::endl << std::endl << "Can write all of these variables: " << std::endl;
        for( std::vector< unsigned int >::const_iterator iter = stateDataVector.begin(); 
            iter != stateDataVector.end(); ++iter)
        {
            if( *iter & OPC::OPC_WRITEVARIABLE )
            {
                opcLog << rawDataVector[ iter - stateDataVector.begin() ].first << std::endl;
            }
        }
        opcLog.close();
    }
    
    if( vm.count( "variables" ) )
    {
        std::vector< std::string > opcVars = vm["variables"].as< std::vector< std::string > >();
        //std::cout << opcVars.size() << std::endl;
        for( size_t i = 0; i < opcVars.size(); ++i )
        {
            opcInterface->AddOPCVariable( opcVars[ i ] );
        }
    }


    if( vm.count( "configFile" ) )
    {
        fs::path file_name( vm[ "configFile" ].as< std::string >() );
        if( !fs::exists( file_name ) )
        {
            std::cout << "The config file " << vm[ "configFile" ].as< std::string >() 
                << " does not exist." << std::endl;
            exit( 1 );
        }
    
        pt::ptree variableMapTree;
        boost::property_tree::json_parser::read_json( file_name.string(), variableMapTree );
        
        //boost::program_options::parse_config_file(ifs, desc);
        //po::store( po::parse_config_file( ifs, desc ), vm );
        //po::notify( vm );
        /*ptree::const_iterator end = pt.end();
        for (ptree::const_iterator it = pt.begin(); it != end; ++it) {
            std::cout << it->first << ": " << it->second.get_value<std::string>() << std::endl;
            print(it->second);
        }*/
        BOOST_FOREACH(boost::property_tree::ptree::value_type& v, variableMapTree )
        {
            
            //opcInterface.AddOPCVariable( v.second.get_value<std::string>() );
            //std::cout << v.first << " " << v.second.get<std::string>( "opc" ) << std::endl;
            //std::cout << v.first << " " << v.second.get<std::string>( "id" ) << std::endl;
            const std::string varName = v.second.get<std::string>( "opc" );
            if( opcInterface->AddOPCVariable( varName ) )
            {
                variableMap[ varName ] = v.second.get<std::string>( "id" );
            }
        }
    }
    
    if( vm[ "broadcast" ].as<bool>() )
    {
        //Init zeromq context
        zmq::context_t context( 1 );

        //Socket for worker control
        zmq::socket_t controller( context, ZMQ_PUB );
        controller.bind( "tcp://*:3098" );

        bool kill_msg( false );
        std::vector< std::pair< std::string, std::string > > valVector;
        while( !kill_msg )
        {
            //Get OPC data here
            bool valid_msg = true;

            //If our message is formatted correctly, send
            if( valid_msg )
            {
                //std::string str = to_json();
                //std::cout << str << std::endl;
                if( !opcInterface->IsOPCVarsEmpty() )
                {
                    //int counter = 0;
                    //while( counter < 100 )
                    {
                        valVector = opcInterface->ReadVars();
                        /*for( std::vector< std::pair< std::string, std::string > >::const_iterator iter = valVector.begin(); iter != valVector.end(); ++iter)
                        {
                            if( variableMap[ iter->first ] == "PT003" )
                            {
                            std::cout << variableMap[ iter->first ] << " " << iter->first << " " << iter->second<< std::endl;
                            }
                        }*/
                        std::string jsonData = to_json( valVector );
                        valVector.resize(0);
                        //std::cout << std::endl << jsonData << std::endl << std::endl;
                        //counter += 1;
                        zmq::message_t zmq_msg;
                        zmq_msg.rebuild( jsonData.size() );
                        memcpy( zmq_msg.data(), jsonData.data(), jsonData.size() );
                        assert( controller.send( zmq_msg ) );
                    }
                }
            }
        }
    }
    else
    {
        if( !opcInterface->IsOPCVarsEmpty() )
        {
            int counter = 0;
            std::vector< std::pair< std::string, std::string > > valVector;
            while( counter < 100 )
            {
                valVector = opcInterface->ReadVars();
                /*for( std::vector< std::pair< std::string, std::string > >::const_iterator iter = valVector.begin(); iter != valVector.end(); ++iter)
                {
                    std::cout << variableMap[ iter->first ] << " " << iter->first << " " << iter->second<< std::endl;
                }*/
                std::string jsonData = to_json( valVector );
                std::cout << std::endl << jsonData << std::endl << std::endl;
                counter += 1;
            }
        }
    }
    delete opcInterface;
    
    CoUninitialize();

    return 0;
}
