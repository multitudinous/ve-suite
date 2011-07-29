#include "Scheduler.h"

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/algorithm/string/find.hpp>

#include <fstream>


namespace iaf
{
namespace scheduler
{
////////////////////////////////////////////////////////////////////////////////
Scheduler::Scheduler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Scheduler::~Scheduler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::SetGraph( lemon::ListDigraph& g, std::map< lemon::ListDigraph::Node, std::string >& modelIDMap )
{
    lemon::ListDigraph::NodeMap< lemon::ListDigraph::Node > nr(m_g);
    lemon::digraphCopy( g, m_g ).nodeCrossRef(nr).run();
    for( lemon::ListDigraph::NodeIt n( m_g ); n != lemon::INVALID; ++n )
    {
        m_modelIDMap[ n ] = modelIDMap[ nr[ n ] ];
    }
}
////////////////////////////////////////////////////////////////////////////////
/*void Scheduler::SetModelIDMap( std::map< lemon::ListDigraph::Node, std::string >& modelIDMap )
{
    m_modelIDMap = modelIDMap;
}*/
////////////////////////////////////////////////////////////////////////////////
void Scheduler::EnableNodesAndArcs( lemon::ListDigraph& g, 
                                   lemon::SubDigraph<lemon::ListDigraph>& fg, 
                                   std::vector< lemon::ListDigraph::Node >& scheduleNodes, 
                                   lemon::ListDigraph::Node& n, 
                                   lemon::ListDigraph::NodeMap< bool >& nodeMap, 
                                   lemon::ListDigraph::ArcMap< bool >& arcMap )
{
    nodeMap[ n ] = true;
    
    bool isScheduler = false;
    
    for( size_t i = 0; i < scheduleNodes.size(); ++i )
    {
        if( n == scheduleNodes.at( i ) )
        {
            isScheduler = true;
            break;
        }
    }
    
    lemon::ListDigraph::Node sourceSchedulerNode;
    size_t hostScheduler = 0;
    for( lemon::ListDigraph::InArcIt m( g, n ); m != lemon::INVALID; ++m )
    {
        for( size_t i = 0; i < scheduleNodes.size(); ++i )
        {
            if( g.source( m ) == scheduleNodes.at( i ) )
            {
                hostScheduler = i;
                //sourceSchedulerNode = g.source( m );
                break;
            }
        }        
    }
    
    for( lemon::ListDigraph::OutArcIt m( g, n ); m != lemon::INVALID; ++m )
    {
        if( isScheduler )
        {
            fg.enable( m );
            lemon::ListDigraph::Node tempNode = g.target( m );
            EnableNodesAndArcs( g, fg, scheduleNodes, tempNode, nodeMap, arcMap );
        }
        else
        {
            for( size_t i = 0; i < scheduleNodes.size(); ++i )
            {
                if( hostScheduler != i )
                {
                    lemon::ListDigraph::Arc tmpArc = lemon::findArc( g, n, scheduleNodes.at( i ) );
                    if( tmpArc == lemon::INVALID )
                    {
                        fg.enable( m );
                        lemon::ListDigraph::Node tempNode = g.target( m );
                        EnableNodesAndArcs( g, fg, scheduleNodes, tempNode, nodeMap, arcMap );
                    }
                    //consider also using lemon::ArcLookUp
                }
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::MakeInfoGraph()
{
    //Setup the info blocks to enable variable looping
    lemon::ListDigraph::NodeMap<bool> filterInfoNodes(m_g, true);
    lemon::ListDigraph::ArcMap<bool> filterInfoArcs(m_g, true);
    lemon::SubDigraph<lemon::ListDigraph> infoSubgraph( m_g, filterInfoNodes, filterInfoArcs );
    
    ///Find all of the scheduler blocks and turn all of the outgoing
    ///scheduler links off for the info flow graph
    for( lemon::ListDigraph::NodeIt n( m_g ); n != lemon::INVALID; ++n )
    {
        const std::string modelName = m_modelIDMap[ n ];
        boost::iterator_range<std::string::const_iterator>::iterator stringIter = boost::find_first( modelName, "scheduler" ).begin();
        
        if( stringIter != modelName.end() )
        {
            m_schedulerNodes.push_back( n );
            for( lemon::ListDigraph::OutArcIt m( m_g, n ); m != lemon::INVALID; ++m )
            {
                filterInfoArcs[ m ] = false;
            }
        }
    }
    
    {
        lemon::StaticDigraph tmp_graph;
        lemon::StaticDigraph::NodeMap< std::string > staticm_modelIDMap( tmp_graph );
        lemon::StaticDigraph::NodeMap< lemon::ListDigraph::Node > nr(tmp_graph);
        
        lemon::digraphCopy( infoSubgraph, tmp_graph).nodeCrossRef(nr).run();
        
        lemon::StaticDigraph::NodeMap<int> orderScheduler( tmp_graph );
        
        lemon::topologicalSort( tmp_graph, orderScheduler );
        
        std::map< int, lemon::StaticDigraph::Node > infoOrderMap;
        for( lemon::StaticDigraph::NodeIt n( tmp_graph ); n != lemon::INVALID; ++n )
        {
            //std::cout << "New node " << m_modelIDMap[nr[n]] << std::endl;
            //std::cout << "Execution order " << orderScheduler[n] << std::endl;
            infoOrderMap[ orderScheduler[n] ] = n;
            //std::cout << "Out arcs = " << lemon::countOutArcs( tmp_graph, n ) << std::endl;
            //std::cout << "In arcs = " << lemon::countInArcs( tmp_graph, n ) << std::endl;
            //std::cout << std::endl;
        }
        
        for( std::map< int, lemon::StaticDigraph::Node >::const_iterator it = infoOrderMap.begin(); it != infoOrderMap.end(); ++it )
        {
            std::cout << m_modelIDMap[nr[it->second]] << " ";
            staticm_modelIDMap[ it->second ] = m_modelIDMap[nr[it->second]];
            
            if( lemon::countOutArcs( tmp_graph, it->second ) == 0 )
            {
                std::cout << "| ";
            }
        }
        
        std::cout << std::endl;
        
        {
            std::ofstream lgfFile( "info.lgf" );
            lemon::digraphWriter(tmp_graph, lgfFile).
                nodeMap( "node names", staticm_modelIDMap ).
                run();        
        }
        
        {
            std::ofstream dotFile( "info.dot" );
            dotFile << "digraph info_dot {" << std::endl;
            dotFile << "  size=\"8,6\"; ratio=fill;" << std::endl;
            dotFile << "  node [ shape=ellipse, fontname=Helvetica, fontsize=10 ];" << std::endl;
            for( lemon::StaticDigraph::NodeIt n(tmp_graph); n!=lemon::INVALID; ++n)
            {
                dotFile << "  n" << tmp_graph.id(n) 
                << " [ label=\"" << staticm_modelIDMap[n] << "\" ]; " << std::endl; 
            }
            dotFile << "  edge [ shape=ellipse, fontname=Helvetica, fontsize=10 ];" << std::endl;
            for(lemon::StaticDigraph::ArcIt e(tmp_graph); e!=lemon::INVALID; ++e) 
            {
                dotFile << "  n" << tmp_graph.id(tmp_graph.source(e)) 
                    << " -> " << " n" << tmp_graph.id(tmp_graph.target(e)) <<std::endl;
                //<< " [ label=\"" << g.id(e) 
                //<< ", length:" << length[e] << "\" ]; " << std::endl;
            } 
            dotFile << "}" << std::endl;
        }        
    }
    
    //m_infoSubgraph = lemon::subDigraph(g, filterInfoNodes, filterInfoArcs );
    lemon::digraphCopy( infoSubgraph, m_infoSubgraph ).run();

    std::cout << "Number of arcs in the info graph = " << lemon::countArcs( m_infoSubgraph ) << std::endl;        
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::MakeSchedulerGraph()
{
    //Setup the info blocks to enable variable looping
    lemon::ListDigraph::NodeMap<bool> filterm_schedulerNodes( m_g, false);
    lemon::ListDigraph::ArcMap<bool> filterSchedulerArcs( m_g, false);
    lemon::SubDigraph<lemon::ListDigraph> schedulerSubgraph( m_g, filterm_schedulerNodes, filterSchedulerArcs );
    
    ///Find all of the scheduler blocks and turn all of the outgoing
    ///scheduler links off for the info flow graph
    for( lemon::ListDigraph::NodeIt n( m_g ); n != lemon::INVALID; ++n )
    {
        const std::string modelName = m_modelIDMap[ n ];
        boost::iterator_range<std::string::const_iterator>::iterator stringIter = boost::find_first( modelName, "scheduler" ).begin();
        
        if( stringIter != modelName.end() )
        {
            EnableNodesAndArcs( m_g, schedulerSubgraph, m_schedulerNodes, n, filterm_schedulerNodes, filterSchedulerArcs );
        }
    }
    std::cout << "Number of arcs in the scheduler graph = " << lemon::countArcs( schedulerSubgraph ) << std::endl;
    std::cout << "Number of nodes in the scheduler graph = " << lemon::countNodes( schedulerSubgraph ) << std::endl;
    
    {
        lemon::StaticDigraph tmp_graph;
        lemon::StaticDigraph::NodeMap< std::string > staticm_modelIDMap( tmp_graph );
        lemon::StaticDigraph::NodeMap< lemon::ListDigraph::Node > nr(tmp_graph);
        
        lemon::digraphCopy( schedulerSubgraph, tmp_graph).nodeCrossRef(nr).run();
        
        lemon::StaticDigraph::NodeMap<int> orderScheduler( tmp_graph );
        
        lemon::topologicalSort( tmp_graph, orderScheduler );
        
        std::map< int, lemon::StaticDigraph::Node > scheduleOrderMap;
        for( lemon::StaticDigraph::NodeIt n( tmp_graph ); n != lemon::INVALID; ++n )
        {
            //std::cout << "New node " << m_modelIDMap[nr[n]] << std::endl;
            //std::cout << "Execution order " << orderScheduler[n] << std::endl;
            scheduleOrderMap[ orderScheduler[n] ] = n;
            //std::cout << "Out arcs = " << lemon::countOutArcs( tmp_graph, n ) << std::endl;
            //std::cout << "In arcs = " << lemon::countInArcs( tmp_graph, n ) << std::endl;
            //std::cout << std::endl;
        }
        
        for( std::map< int, lemon::StaticDigraph::Node >::const_iterator it = scheduleOrderMap.begin(); it != scheduleOrderMap.end(); ++it )
        {
            std::cout << m_modelIDMap[nr[it->second]] << " ";
            staticm_modelIDMap[ it->second ] = m_modelIDMap[nr[it->second]];
            if( lemon::countOutArcs( tmp_graph, it->second ) == 0 )
            {
                std::cout << "| ";
            }
        }
        
        std::cout << std::endl;
        
        {
            std::ofstream lgfFile( "schedule.lgf" );
            lemon::digraphWriter(tmp_graph, lgfFile).
                nodeMap( "node names", staticm_modelIDMap ).
                run();
        }
        
        
        {
            std::ofstream dotFile( "schedule.dot" );
            dotFile << "digraph scheduler_dot {" << std::endl;
            dotFile << "  node [ shape=ellipse, fontname=Helvetica, fontsize=10 ];" << std::endl;
            for( lemon::StaticDigraph::NodeIt n(tmp_graph); n!=lemon::INVALID; ++n)
            {
                dotFile << "  n" << tmp_graph.id(n) 
                << " [ label=\"" << m_modelIDMap[nr[n]] << "\" ]; " << std::endl; 
            }
            dotFile << "  edge [ shape=ellipse, fontname=Helvetica, fontsize=10 ];" << std::endl;
            for(lemon::StaticDigraph::ArcIt e(tmp_graph); e!=lemon::INVALID; ++e)
            {
                dotFile << "  n" << tmp_graph.id(tmp_graph.source(e)) << " -> " 
                    << " n" << tmp_graph.id(tmp_graph.target(e)) <<std::endl;
                //<< " [ label=\"" << g.id(e) 
                //<< ", length:" << length[e] << "\" ]; " << std::endl;
            } 
            dotFile << "}" << std::endl;
        }        
    }        
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::DumpCompleteGraph()
{
    std::cout << "Is a DAG " << lemon::dag( m_g ) << std::endl;
    //std::cout << "Is a acyclic " << lemon::acyclic( g ) << std::endl;
    //std::cout << "Is a tree " << lemon::tree( g ) << std::endl;
    
    lemon::ListDigraph::NodeMap<int> order( m_g );
    
    lemon::topologicalSort( m_g, order );
    
    std::map< int, lemon::ListDigraph::Node > orderMap;
    for( lemon::ListDigraph::NodeIt n( m_g ); n != lemon::INVALID; ++n )
    {
        //std::cout << "New node " << m_modelIDMap[n] << std::endl;
        //std::cout << "Execution order " << order[n] << std::endl;
        orderMap[ order[n] ] = n;
        //std::cout << "Out arcs = " << lemon::countOutArcs( g, n ) << std::endl;
        //std::cout << "In arcs = " << lemon::countInArcs( g, n ) << std::endl;
        //std::cout << std::endl;
    }
    
    for( std::map< int, lemon::ListDigraph::Node >::const_iterator it = orderMap.begin(); it != orderMap.end(); ++it )
    {
        std::cout << m_modelIDMap[it->second] << " ";
        if( lemon::countOutArcs( m_g, it->second ) == 0 )
        {
            std::cout << "| ";
        }
    }
    std::cout << std::endl;
    std::cout << "We have a directed graph with " 
    << lemon::countNodes( m_g ) << " node(s) "
    << "and " << lemon::countArcs( m_g ) << " arc(s)." << std::endl;
    
    {
        std::ofstream lgfFile( "complete.lgf" );       
        lemon::digraphWriter( m_g, lgfFile).
            //nodeMap( "node names", m_modelIDMap ).
            run();
    }
    
    {
        std::ofstream dotFile( "complete.dot" );
        dotFile << "digraph complete_dot {" << std::endl;
        dotFile << "  size=\"8,6\"; ratio=fill;" << std::endl;
        dotFile << "  node [ shape=ellipse, fontname=Helvetica, fontsize=10 ];" << std::endl;
        for( lemon::ListDigraph::NodeIt n( m_g ); n!=lemon::INVALID; ++n)
        {
            const std::string nodeName = m_modelIDMap[n];
            boost::iterator_range<std::string::const_iterator>::iterator stringIter = boost::find_first( nodeName, "scheduler" ).begin();
            
            if( stringIter != nodeName.end() )
            {
                dotFile << "  n" <<  m_g.id(n) 
                << " [ label=\"" << nodeName << "\", color=orange ]; " << std::endl;
            }
            else
            {
                dotFile << "  n" <<  m_g.id(n) 
                << " [ label=\"" << nodeName << "\" ]; " << std::endl;
            }
        }
        dotFile << "  edge [ shape=ellipse, fontname=Helvetica, fontsize=10 ];" << std::endl;
        for(lemon::ListDigraph::ArcIt e( m_g ); e!=lemon::INVALID; ++e) 
        {
            const std::string sourceName = 
            m_modelIDMap[m_g.source(e)];
            const std::string targetName = 
            m_modelIDMap[m_g.target(e)];
            
            boost::iterator_range<std::string::const_iterator>::iterator stringIter1 = boost::find_first( sourceName, "scheduler" ).begin();
            boost::iterator_range<std::string::const_iterator>::iterator stringIter2 = boost::find_first( targetName, "scheduler" ).begin();
            
            if( stringIter1 != sourceName.end() )
            {
                dotFile << "  n" << m_g.id(m_g.source(e)) << " -> " << " n" << m_g.id(m_g.target(e)) << " [ color=orange ];" <<std::endl;
            }
            else if( stringIter2 != targetName.end() )
            {
                dotFile << "  n" << m_g.id(m_g.source(e)) << " -> " << " n" << m_g.id(m_g.target(e)) << " [ color=blue ];" <<std::endl;
            }
            else
            {
                dotFile << "  n" << m_g.id(m_g.source(e)) << " -> " << " n" << m_g.id(m_g.target(e)) <<std::endl;
            }
        } 
        dotFile << "}" << std::endl;
    }          
}
////////////////////////////////////////////////////////////////////////////////
}
}
