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

// --- Boost Includes --- //
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/identity.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/mem_fun.hpp>

// --- STL Includes --- //
#include <string>

#include <lemon/list_graph.h>

///Structs to help Boost multi index work better
struct INFOID{};
struct NODEID{};

namespace iaf
{
namespace scheduler
{
class ModelNode : public lemon::ListDigraph::Node
{
public:
    ModelNode(){;}
    ModelNode( lemon::ListDigraph::Node& node );
    
    ///Copy Construstor
    ModelNode( ModelNode const&, lemon::ListDigraph::Node const& node );
    
    ///equal operator
    ModelNode& operator= ( ModelNode const& );
    
    virtual ~ModelNode();
    
    void Preprocess();

    void RunModel();
        
    void Postprocess();

    void SetModelName( std::string const& name );
    
    std::string GetResults();
    
private:
    std::string m_modelName;
    std::string m_results;
};
/*
typedef boost::multi_index_container<
iaf::scheduler::ModelNode*,
boost::multi_index::indexed_by<
//Sort by the nodes m_infoGraphId
boost::multi_index::ordered_non_unique< 
boost::multi_index::tag< INFOID >,
BOOST_MULTI_INDEX_CONST_MEM_FUN( ModelNode, int const&, GetInfoGraphId ) >,

//Sort by the nodes lemon node id
boost::multi_index::ordered_unique< 
boost::multi_index::tag< NODEID >,
BOOST_MULTI_INDEX_CONST_MEM_FUN( ModelNode, int const&, GetId ) >
>
> ScheduleNetwork;

///Iterator for sorting through by the info id
typedef boost::multi_index::nth_index< ScheduleNetwork, 0 >::type InfoGraphSort;
///Iterator for sorting through by the node id
typedef boost::multi_index::nth_index< ScheduleNetwork, 1 >::type MainGraphSort;
///Sort by tags
typedef ScheduleNetwork::index< INFOID >::type ModelByInfoId;
///Sort by tags
typedef ScheduleNetwork::index< NODEID >::type ModelByNodeId;
*/
}
}
