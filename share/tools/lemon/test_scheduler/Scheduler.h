#pragma once

#include <lemon/connectivity.h>
#include <lemon/list_graph.h>
#include <lemon/smart_graph.h>
#include <lemon/lgf_reader.h>
#include <lemon/static_graph.h>
#include <lemon/lp.h>
#include <lemon/adaptors.h>

#include <vector>

namespace iaf
{
namespace scheduler
{
class Scheduler
{
public:
    Scheduler();
    ~Scheduler();
    
    ///Set the graph to be used by the scheduler
    void SetGraph( lemon::ListDigraph& g, std::map< lemon::ListDigraph::Node, std::string >& modelIDMap );
    ///Set the model name map
    //void SetModelIDMap( std::map< lemon::ListDigraph::Node, std::string >& modelIDMap );

    void DumpCompleteGraph();
    void MakeSchedulerGraph();
    void MakeInfoGraph();

private:
    void EnableNodesAndArcs( lemon::ListDigraph& g, 
                            lemon::SubDigraph<lemon::ListDigraph>& fg, 
                            std::vector< lemon::ListDigraph::Node >& scheduleNodes, 
                            lemon::ListDigraph::Node& n, 
                            lemon::ListDigraph::NodeMap< bool >& nodeMap, 
                            lemon::ListDigraph::ArcMap< bool >& arcMap );

    std::vector< lemon::ListDigraph::Node > m_schedulerNodes;
    lemon::ListDigraph m_infoSubgraph;
    lemon::ListDigraph m_g;
    std::map< lemon::ListDigraph::Node, std::string > m_modelIDMap;
};
}
}
