/* -*- mode: C++; indent-tabs-mode: nil; -*-
 *
 * This file is a part of LEMON, a generic C++ optimization library.
 *
 * Copyright (C) 2003-2010
 * Egervary Jeno Kombinatorikus Optimalizalasi Kutatocsoport
 * (Egervary Research Group on Combinatorial Optimization, EGRES).
 *
 * Permission to use, modify and distribute this software is granted
 * provided that this copyright notice appears in all copies. For
 * precise terms see the accompanying LICENSE file.
 *
 * This software is provided "AS IS" with no warranty of any kind,
 * express or implied, and with no claim as to its suitability for any
 * purpose.
 *
 */


#include <iostream>

#include <lemon/connectivity.h>
#include <lemon/list_graph.h>
#include <lemon/smart_graph.h>
#include <lemon/lgf_reader.h>
#include <lemon/lp.h>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/StateInfo.h>
#include <ves/open/xml/StateInfoPtr.h>

#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Tag.h>
#include <ves/open/xml/model/TagPtr.h>
#include <ves/open/xml/User.h>
#include <ves/open/xml/UserPtr.h>
#include <ves/open/xml/Command.h>

#include <ves/open/xml/DOMDocumentManager.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>

using namespace lemon;


/*template <typename GR, typename CAP>
double maxFlow(const GR &g, const CAP &capacity,
		               typename GR::Node source, typename GR::Node target)
{
    TEMPLATE_DIGRAPH_TYPEDEFS(GR);

    // Create an instance of the default LP solver
    Lp lp;

    // Add a column to the problem for each arc
    typename GR::template ArcMap<Lp::Col> f(g);
    lp.addColSet(f);

    // Capacity constraints
    for (ArcIt a(g); a != INVALID; ++a) 
    {
        lp.colLowerBound(f[a], 0);
        lp.colUpperBound(f[a], capacity[a]);
    }

    // Flow conservation constraints
    for (NodeIt n(g); n != INVALID; ++n) 
    {
        if (n == source || n == target) continue;
        Lp::Expr e;
        for (OutArcIt a(g, n); a != INVALID; ++a) e += f[a];
        for (InArcIt a(g, n); a != INVALID; ++a) e -= f[a];
        lp.addRow(e == 0);
    }

    // Objective function
    Lp::Expr o;
    for (OutArcIt a(g, source); a != INVALID; ++a) o += f[a];
    for (InArcIt a(g, source); a != INVALID; ++a) o -= f[a];
    lp.max();
    lp.obj(o);

    // Solve the LP problem
    lp.solve();

    return lp.primal();
}
*/
////////////////////////////////////////////////////////////////////////////////
bool AddSubSystem( ves::open::xml::model::SystemPtr system )
{
    /*std::map< std::string, ves::open::xml::model::SystemPtr >::const_iterator 
    iter = m_systemMap.find( system->GetID() );
    if( iter != m_systemMap.end() )
    {
        std::cerr << "XMLDataBufferEngine::AddSubSystem : "
        << "System already on system map." << std::endl;
        return false;
    }
    
    m_systemMap[ system->GetID() ] = system;*/
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ParseSystem( ves::open::xml::model::SystemPtr system )
{
    //add the system to the map
    AddSubSystem( system );
    
    //Parse out the subsystems
    size_t modelCount = system->GetNumberOfModels();
    for( size_t j = 0; j < modelCount; j++ )
    {
        if( system->GetModel( j )->GetSubSystem() )
        {
            ParseSystem( system->GetModel( j )->GetSubSystem() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void LoadVESData( const std::string& filename, lemon::ListDigraph& g, lemon::ListDigraph::NodeMap< std::string >& modelIDMap )
{
    if( filename.empty() )
    {
        return;
    }
    
    //Does the file exist
    boost::filesystem::path correctedPath( filename );
    
    bool exists = boost::filesystem::exists( correctedPath );
    
    std::cout << "|\t---" << filename << "---" << correctedPath.string() << " exists " << exists << std::endl;
    
    const std::string xmlNetwork = correctedPath.string();
    
    //Clean up all the maps so that everything is set for the new network data
    //CleanUp();
    ///Map of systems
    std::map< std::string, ves::open::xml::model::SystemPtr > m_systemMap;
    ///Map of networks - backwards compatibility
    std::map< std::string, ves::open::xml::model::NetworkPtr > m_networkMap;
    
    // Just clear the design canvas
    // Start the busy cursor
    // Load from the nt file loaded through wx
    // Get a list of all the command elements
    //_fileProgress->Update( 10, _("start loading") );
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromFile();

    std::vector< std::pair< std::string, std::string > > dataToObtain;
    std::vector< std::pair< std::string, std::string > >::iterator dataIter;
    dataToObtain.push_back( std::make_pair( "Model", "veSystem" ) );
    dataToObtain.push_back( std::make_pair( "Model", "veNetwork" ) );
    dataToObtain.push_back( std::make_pair( "Model", "veModel" ) );
    dataToObtain.push_back( std::make_pair( "XML", "User" ) );
    networkWriter.ReadXMLData( xmlNetwork, dataToObtain );
    std::vector< ves::open::xml::XMLObjectPtr >::iterator objectIter;
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
    networkWriter.GetLoadedXMLObjects();
    ves::open::xml::model::SystemPtr tempSystem;
    std::string topId;
    
    // we are expecting that a network will be found
    if( !objectVector.empty() )
    {
        tempSystem = boost::dynamic_pointer_cast<ves::open::xml::model::System>( objectVector.at( 0 ) );
        m_systemMap[tempSystem->GetID()] = tempSystem;
        //get the main systems id
        topId = tempSystem->GetID();
        m_networkMap[ "Network" ] = tempSystem->GetNetwork();
        objectIter = objectVector.begin();
        objectIter = objectVector.erase( objectIter );
    }
    else
    {
        std::cerr << "Improperly formated ves file."
        << "VES File Read Error" << std::endl;
    }
    
    // now lets create a list of them
    std::ostringstream modelID;
    std::vector< ves::open::xml::model::ModelPtr >::iterator modelIter;
    std::vector< ves::open::xml::model::ModelPtr > modelVector;
    modelVector = tempSystem->GetModels();
    //Setup all of the nodes of the graph based on all of the models in the system
    for( modelIter = modelVector.begin(); modelIter != modelVector.end(); )
    {
        ves::open::xml::model::ModelPtr model = *modelIter;
        modelIter = modelVector.erase( modelIter );
        modelID << model->GetModelID();
        lemon::ListDigraph::Node u = g.addNode();
        modelIDMap[ u ] = modelID.str();
        modelID.str( "" );
    }

    
    std::vector< std::string > networkModelVector;
    std::vector< std::string >::iterator stringIter;
    long moduleID = 0;
    std::ostringstream fromID;
    std::ostringstream toID;
    ves::open::xml::model::NetworkPtr tempNetwork = m_networkMap[ "Network" ];
    //Now setup the direct links
    for( size_t i = 0; i < tempNetwork->GetNumberOfLinks(); ++i )
    {
        tempNetwork->GetLink( i )->GetFromModule()->GetData( moduleID );
        fromID << moduleID;
        tempNetwork->GetLink( i )->GetToModule()->GetData( moduleID );
        toID << moduleID;
        
        lemon::ListDigraph::Node u = lemon::mapFind( g, modelIDMap, fromID.str() );
        lemon::ListDigraph::Node v = lemon::mapFind( g, modelIDMap, toID.str() );

        ListDigraph::Arc  a = g.addArc(u, v);

        fromID.str( "" );
        toID.str( "" );
    }
    
    if( tempSystem )
    {
        //Parse out the remaining subsystems
        size_t modelCount = tempSystem->GetNumberOfModels();
        for( size_t j = 0; j < modelCount; j++ )
        {
            if( tempSystem->GetModel( j )->GetSubSystem() )
            {
                ParseSystem( tempSystem->GetModel( j )->GetSubSystem() );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch ( const XMLException &toCatch )
    {
        std::cerr << "Error during Xerces-c Initialization.\n"
        << "  Exception message:"
        << XMLString::transcode( toCatch.getMessage() ) << std::endl;
        return false;
    }
    

    ///Initialize VE-Open
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new ves::open::xml::XMLCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new ves::open::xml::shader::ShaderCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "Model", new ves::open::xml::model::ModelCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD", new ves::open::xml::cad::CADCreator() );

    // Check the arguments
    if (argc < 2) 
    {
        std::cerr << "Usage:" << std::endl;
        std::cerr << "  ves_lemon_converter <input_ves_file>" << std::endl;
        return 0;
    }

    std::string vesFilename( argv[ 1 ] );
    
    lemon::ListDigraph g;
    lemon::ListDigraph::NodeMap< std::string > modelIDMap( g );
    
    LoadVESData( vesFilename, g, modelIDMap );
    
    std::cout << "Is a DAG " << lemon::dag( g ) << std::endl;
    //std::cout << "Is a acyclic " << lemon::acyclic( g ) << std::endl;
    //std::cout << "Is a tree " << lemon::tree( g ) << std::endl;

    lemon::ListDigraph::NodeMap<int> order( g );

    lemon::topologicalSort( g, order );

    std::map< int, lemon::ListDigraph::Node > orderMap;
    for( lemon::ListDigraph::NodeIt n( g ); n != INVALID; ++n )
    {
        std::cout << "New node" << std::endl;
        std::cout << order[n] << std::endl;
        std::cout << modelIDMap[n] << std::endl;
        orderMap[ order[n] ] = n;
        std::cout << lemon::countOutArcs( g, n ) << std::endl;
        std::cout << lemon::countInArcs( g, n ) << std::endl;
    }
    
    for( std::map< int, lemon::ListDigraph::Node >::const_iterator it = orderMap.begin(); it != orderMap.end(); ++it )
    {
        std::cout << modelIDMap[it->second] << " ";
        if( lemon::countOutArcs( g, it->second ) == 0 )
        {
            std::cout << "| ";
        }
    }
    std::cout << std::endl;
    std::cout << "We have a directed graph with " 
        << lemon::countNodes(g) << " nodes "
        << "and " << lemon::countArcs(g) << " arc." << std::endl;

    /*// Read the input file
    SmartDigraph g;
    SmartDigraph::ArcMap<double> cap(g);
    SmartDigraph::Node s, t;

    digraphReader(g, argv[1])
        .arcMap("capacity", cap)
        .node("source", s)
        .node("target", t)
        .run();

    // Solve the problem and print the result
    std::cout << "Max flow value: " << maxFlow(g, cap, s, t) << std::endl;*/
    XMLPlatformUtils::Terminate();

    return 0;
}
