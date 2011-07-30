#pragma once

#include <lemon/connectivity.h>
#include <lemon/list_graph.h>
#include <lemon/smart_graph.h>
#include <lemon/lgf_reader.h>
#include <lemon/static_graph.h>
#include <lemon/lp.h>
#include <lemon/adaptors.h>

#include <vector>

#include "ModelNode.h"

namespace iaf
{
namespace scheduler
{
class Scheduler
{
public:
    ///Constructor
    Scheduler();
    ///Destructor
    ~Scheduler();
    
    ///Set the graph to be used by the scheduler
    void SetGraph( lemon::ListDigraph& g, std::map< lemon::ListDigraph::Node, std::string >& modelIDMap, std::map< std::string, iaf::scheduler::ModelNode* >& modelMap );
    ///Dump the complete graph of the users to a file
    void DumpCompleteGraph();
    ///Must run this before the scheduler graph -- only because we need the scheduler nodes defined
    void MakeInfoGraph();
    ///Make the model execution graph
    void MakeSchedulerGraph();

    void RunModels();

private:
    ///Helps break down the graph the user provides
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
    
    std::map< std::string, iaf::scheduler::ModelNode* > m_modelMap;
    std::map< int, lemon::ListDigraph::Node > m_scheduleModelMap;
    std::map< std::string, lemon::ListDigraph::Node > m_infoNameMap;
    std::map< lemon::ListDigraph::Node, lemon::ListDigraph::Node > m_infoToGNameMap;

};
}
}
