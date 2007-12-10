/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef DBCONVERTER_H
#define DBCONVERTER_H

#include <vector>
#include <string>
#include <map>
#include <set>

using namespace std;

/*---------------------------
 * --------- HEADER ---------
 ----------------------------*/

class DBConverter
{

public:
    DBConverter();
    DBConverter( std::string db_file );
    ~DBConverter();

    int read_header( std::string db_file );

    int makeVTK( std::string db_path, std::string wr_path, std::set<std::string> scalars );
    int makeDX( std::string path, std::string dx_file, std::string dxb_file,
                std::set<std::string> scalars,
                    bool mark_invalids, bool incl_wireframe );

private:
    void free_memory();

    int read_functions( std::set<std::string> scalars,
                            std::map<std::string, int>& func_map,
                            std::vector<std::vector<float> > & func,
                            std::vector<float>& vel_func );

    void swap_4_range( char *mem_ptr1, int num );
    void swap_4( char* data );

    int  check_edge( int i, int j, int k, int dir );
    void make_connection( int i, int j, int k, int dir, int index );
    int  edge_in_dir( int me, int direction );
    void get_ijk_point( int index, int &i, int &j, int &k );

    std::map<int, int>                _map_points;
    std::vector<std::pair<int, int> > _connections;
    std::vector<int> _positions_i;
    std::vector<int> _positions_j;
    std::vector<int> _positions_k;

    float *_x_coord, *_y_coord, *_z_coord;
    float *_x_edge,  *_y_edge,  *_z_edge;

    int *_cell_type;
    int _ni, _nj, _nk, _ns, _nv;

    std::string _db_file;
    std::vector<std::string> _names;
};

#endif
