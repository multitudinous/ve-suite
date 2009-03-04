/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/ce/util/node_loop.h>
#include <ves/ce/util/node_module.h>
#include <ves/ce/util/Module.h>
#include <ves/ce/util/Network.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
#include <iostream>
#include <algorithm>

using namespace VE_CE::Utilities;
////////////////////////////////////////////////////////////////////////////////

node_loop::node_loop( VE_CE::Utilities::Network *n )
        : node_base( n, 1 )
{}

/////////////

node_loop::node_loop( const node_loop &nl )
        : node_base( nl._net, 1 )
{
    clear();

    for( int i = 0; i < ( int )nl._nodes.size(); i++ )
        if( nl._nodes[i]->_type == 0 ) add_node(( node_module* )nl._nodes[i] );
        else                       add_node(( node_loop* )nl._nodes[i] );
}

/////////////

node_loop::~node_loop()
{
    clear();
}

/////////////

void node_loop::clear()
{
    for( int i = 0; i < ( int )_nodes.size(); i++ )
        delete _nodes[i];
    _nodes.clear();
}

/////////////

void node_loop::add_node( const node_module *nm )
{
    _nodes.push_back( new node_module( *nm ) );
}

/////////////

void node_loop::add_node( const node_loop *nl )
{
    _nodes.push_back( new node_loop( *nl ) );

}

/////////////

void node_loop::get_mods( std::set<int> &mods )
{
    mods.clear();

    std::set<int> temp_mods;
    std::set<int> new_mods;

    for( int i = 0; i < ( int )_nodes.size(); i++ )
    {
        _nodes[i]->get_mods( temp_mods );
        std::set_union( mods.begin(), mods.end(),
                        temp_mods.begin(), temp_mods.end(),
                        inserter( new_mods, new_mods.begin() ) );
        mods = new_mods;
    }
}

/////////////

void node_loop::get_ins( std::set<int> &ins, std::set<int> connid_ignore )
    {
        ins.clear();

        std::set<int> mods;
        get_mods( mods );

        std::set<int> temp_ins;
        std::set<int> new_ins;

        for( int i = 0; i < ( int )_nodes.size(); i++ )
        {
            _nodes[i]->get_ins( temp_ins, connid_ignore );
            std::set_union( ins.begin(), ins.end(),
                            temp_ins.begin(), temp_ins.end(),
                            inserter( new_ins, new_ins.begin() ) );
            ins = new_ins;
        }

        new_ins.clear();

        std::set_difference( ins.begin(), ins.end(),
                             mods.begin(), mods.end(),
                             inserter( new_ins, new_ins.begin() ) );
        ins = new_ins;
    }

/////////////

void node_loop::get_outs( std::set<int> &outs, std::set<int> connid_ignore )
    {
        outs.clear();

        std::set<int> mods;
        get_mods( mods );

        std::set<int> temp_outs;
        std::set<int> new_outs;

        for( int i = 0; i < ( int )_nodes.size(); i++ )
        {
            _nodes[i]->get_outs( temp_outs, connid_ignore );
            std::set_union( outs.begin(), outs.end(),
                            temp_outs.begin(), temp_outs.end(),
                            inserter( new_outs, new_outs.begin() ) );
            outs = new_outs;
        }

        new_outs.clear();

        std::set_difference( outs.begin(), outs.end(),
                             mods.begin(), mods.end(),
                             inserter( new_outs, new_outs.begin() ) );
        outs = new_outs;
    }

/////////////

void node_loop::print_mods()
{
    std::cerr << " (";
    for( int i = 0; i < ( int )_nodes.size(); i++ )
        _nodes[i]->print_mods();
    std::cerr << " )";
}

/////////////

int node_loop::execute_mods( int mod, bool running )
{
    //R is the execution order of the modules
    int r;

    for( int i = 0; i < ( int )_nodes.size() - 1; i++ )
    {
        r = _nodes[i]->execute_mods( mod, running );
        if( r > 0 )
            return r;
    }

    r = _nodes[( int )_nodes.size()-1]->execute_mods( mod, running );

    bool is_fb = false;
    if( _nodes[0]->_type == 0 )
    {
        if( _net->GetModule((( node_module* )_nodes[0] )->_module - 1 )->_is_feedback )
        {
            is_fb = true;
        }
    }

    if (( r > 0 ) && is_fb )
    {
        if( _net->GetModule((( node_module* )_nodes[0] )->_module - 1 )->_return_state == 3 )
        {
            _net->GetModule((( node_module* )_nodes[0] )->_module - 1 )->_return_state = 0;
            // erase _node[0]'s feedback input
            //mike, _net->module(((node_module*)_nodes[0])->_module-1)->getIPort(1)->reset();
            //mike, while(_net->module(((node_module*)_nodes[0])->_module-1)->getIPort(1)->have_data())
            //mike, _net->module(((node_module*)_nodes[0])->_module-1)->getIPort(1)->finish();
        }
        else
        {
            // erase all outputs to any nodes outside of this loop
            std::set<int> mods;
            get_mods( mods );

            std::set<int> c_ignore;

            std::set<int> outs;
            get_outs( outs, c_ignore );

            std::set<int> dif;
            std::set_difference( outs.begin(), outs.end(),
                                 mods.begin(), mods.end(),
                                 inserter( dif, dif.begin() ) );

            // test
            //std::set<int>::iterator iter;
            //cerr << "REMOVING INPUTS TO: \n";
            //for(iter=dif.begin(); iter!=dif.end(); iter++)
            // cerr << _net->module((*iter)-1)->get_id() << "\n";

            clear_out_to( dif );

            // reschedule loop nodes
            need_execute();
        }
    }
    return r;
}

/////////////

void node_loop::need_execute()
{
    for( int i = 0; i < ( int )_nodes.size(); i++ )
        _nodes[i]->need_execute();
}

/////////////

void node_loop::clear_out_to( std::set<int> mods )
{
    for( int i = 0; i < ( int )_nodes.size(); i++ )
        _nodes[i]->clear_out_to( mods );
}

