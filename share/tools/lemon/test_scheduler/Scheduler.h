/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#pragma once

#include <lemon/connectivity.h>
#include <lemon/list_graph.h>
#include <lemon/smart_graph.h>
#include <lemon/lgf_reader.h>
#include <lemon/static_graph.h>
#include <lemon/lp.h>
#include <lemon/adaptors.h>

#include <boost/bimap.hpp>

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
    
    typedef boost::bimap< std::string, iaf::scheduler::ModelNode > model_bimap;
    typedef model_bimap::value_type position;
    model_bimap m_modelBimap;
};
}
}
