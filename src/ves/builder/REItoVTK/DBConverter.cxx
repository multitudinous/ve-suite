/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <cstdio>
#include <fstream>
#include <iostream>

#include <DBConverter.h>

#undef WANTASCII

//namespace REI {

DBConverter::DBConverter():
        _x_coord( NULL ),
        _y_coord( NULL ),
        _z_coord( NULL ),
        _x_edge( NULL ),
        _y_edge( NULL ),
        _z_edge( NULL ),
        _cell_type( NULL )
{}

/*-----------------------------------------------------------------------*/

DBConverter::DBConverter( std::string db_file ):
        _x_coord( NULL ),
        _y_coord( NULL ),
        _z_coord( NULL ),
        _x_edge( NULL ),
        _y_edge( NULL ),
        _z_edge( NULL ),
        _cell_type( NULL )
{
    read_header( db_file );
}

/*-----------------------------------------------------------------------*/

DBConverter::~DBConverter()
{
    free_memory();
}

/*-----------------------------------------------------------------------*/

void DBConverter::free_memory()
{
    if( _x_coord )   delete( _x_coord );
    if( _y_coord )   delete( _y_coord );
    if( _z_coord )   delete( _z_coord );
    if( _cell_type ) delete( _cell_type );
    if( _x_edge )    delete( _x_edge );
    if( _y_edge )    delete( _y_edge );
    if( _z_edge )    delete( _z_edge );
}

/*-----------------------------------------------------------------------*/

int DBConverter::read_header( std::string db_file )
{
    FILE *s1;

    char header[100], name[9], nname[9];
    float val;
    int i, j, k, l;

    _db_file = db_file;

    //# open db file
    if (( s1 = fopen( db_file.c_str(), "rb" ) ) == NULL )
    {
        cerr << "Failed to open " << db_file << endl;
        return 0;
    }

    //# read first line
    fseek( s1, 4L, SEEK_SET );
    fread( header, sizeof( char ), 80, s1 );

    //# second line
    fseek( s1, 8L, SEEK_CUR );
    fread( header, sizeof( char ), 80, s1 );

    //# determine the grid type
    int grdtype;
    if( strncmp( header, "cartesian_rectangular", 21 ) == 0 )             grdtype = 0;
    else if( strncmp( header, "body_fitted_structured_grid", 27 ) == 0 )  grdtype = 1;
    else if( strncmp( header, "cartesian_cylindrical", 21 ) == 0 )        grdtype = 2;
    else grdtype = 3;

    //# third line
    fseek( s1, 8L, SEEK_CUR );
    fread( header, sizeof( char ), 80, s1 );

    //# read dimensions
    int dims;
    fseek( s1, 8L, SEEK_CUR );
    fread( &dims, sizeof( int ), 1, s1 );

    fseek( s1, 8L, SEEK_CUR );
    fread( &_ni, sizeof( int ), 1, s1 );
    fread( &_nj, sizeof( int ), 1, s1 );
    fread( &_nk, sizeof( int ), 1, s1 );

    fseek( s1, 8L, SEEK_CUR );
    fread( &_ns, sizeof( int ), 1, s1 );
    fread( &_nv, sizeof( int ), 1, s1 );

    //# Read scalar and vector names
    fseek( s1, 8L, SEEK_CUR );
    name[8] = '\0';
    for( i = 0; i < _ns; i++ )
    {
        fread( name, sizeof( char ), 8, s1 );
        sscanf( name, "%s", nname ); //# hack
        _names.push_back( nname );
    }
    fseek( s1, 8L, SEEK_CUR );
    for( i = _ns; i < ( _ns + _nv ); i++ )
    {
        fread( name, sizeof( char ), 8, s1 );
        sscanf( name, "%s", nname ); //# hack
        _names.push_back( nname );
    }

    fseek( s1, 8L, SEEK_CUR );

    //# Allocate memory for xyz information
    _x_coord = new float[_ni];
    _y_coord = new float[_nj];
    _z_coord = new float[_nk];

    if( _x_coord == NULL || _y_coord == NULL || _z_coord == NULL )
    {
        cerr << "Memory alloc error coords.\n";
        return 0;
    }

    //# Read the grid
    fread( _x_coord, sizeof( float ), _ni, s1 );
    fseek( s1, 8L, SEEK_CUR );
    fread( _y_coord, sizeof( float ), _nj, s1 );
    fseek( s1, 8L, SEEK_CUR );
    fread( _z_coord, sizeof( float ), _nk, s1 );

    //# Allocate information for x, y, z edges
    _x_edge = new float[_ni+1];
    _y_edge = new float[_nj+1];
    _z_edge = new float[_nk+1];

    if( _x_edge == NULL || _y_edge == NULL || _z_edge == NULL )
    {
        cerr << "Memory allocation error\n";
        return( 0 );
    }

    //# Edges are halfway between cell centers
    for( i = 1; i < _ni; i++ )
        _x_edge[i] = ( _x_coord[i-1] + _x_coord[i] ) / 2;
    _x_edge[0] = _x_coord[0] - ( _x_coord[1] - _x_coord[0] ) / 2;
    _x_edge[_ni] = _x_coord[_ni-1] + ( _x_coord[_ni-1] - _x_coord[_ni-2] ) / 2;

    for( j = 1; j < _nj; j++ )
        _y_edge[j] = ( _y_coord[j-1] + _y_coord[j] ) / 2;
    _y_edge[0] = _y_coord[0] - ( _y_coord[1] - _y_coord[0] ) / 2;
    _y_edge[_nj] = _y_coord[_nj-1] + ( _y_coord[_nj-1] - _y_coord[_nj-2] ) / 2;

    for( k = 1; k < _nk; k++ )
        _z_edge[k] = ( _z_coord[k-1] + _z_coord[k] ) / 2;
    _z_edge[0] = _z_coord[0] - ( _z_coord[1] - _z_coord[0] ) / 2;
    _z_edge[_nk] = _z_coord[_nk-1] + ( _z_coord[_nk-1] - _z_coord[_nk-2] ) / 2;

    int numelements = _ni * _nj * _nk;

    //# Allocate memory for xyz information
    _cell_type = new int[numelements];
    if( _cell_type == NULL )
    {
        cerr << "Memory alloc error cell_type.\n";
        return 0;
    }

    fseek( s1, 8L, SEEK_CUR );

    //# Read in Cell types

    for( l = 0; l < _ns + _nv; l++ )
    {
        if( _names[l] == "walls" )
        {
            for( k = 0; k < _nk; k++ )
            {
                for( j = 0; j < _nj; j++ )
                {
                    for( i = 0; i < _ni; i++ )
                    {
                        fread( &val, sizeof( float ), 1, s1 );
                        _cell_type[i *( _nj * _nk ) + j * _nk + k] = ( int )((( int )val == 8 ) ? 1 : 0 );
                    }
                }
            }
        }
        else
        {
            if( _names[l] == "vel" )
                fseek( s1, 3*numelements*sizeof( float ) + 3*8L, SEEK_CUR );
            else
                fseek( s1, numelements*sizeof( float ), SEEK_CUR );
        }
        fseek( s1, 8L, SEEK_CUR );
    }

    // Close DB file
    fclose( s1 );

    //# Create wireframe
    int n;
    //# do it -x
    for( j = 0; j < ( _nj + 1 ); j++ )
        for( k = 0; k < ( _nk + 1 ); i = 0, k++ )
        {
            while( i < ( _ni + 1 ) )
            {
                n = check_edge( i, j, k, 0 );
                if( n > i )
                {
                    make_connection( i, j, k, 0, n );
                    i = n + 1;
                }
                else
                    i++;
            }
        }

    //# do it - y
    for( i = 0; i < ( _ni + 1 ); i++ )
        for( k = 0; k < ( _nk + 1 ); j = 0, k++ )
        {
            while( j < ( _nj + 1 ) )
            {
                n = check_edge( i, j, k, 2 );
                if( n > j )
                {
                    make_connection( i, j, k, 2, n );
                    j = n + 1;
                }
                else
                    j++;
            }
        }

    //# do it - z
    for( i = 0; i < ( _ni + 1 ); i++ )
        for( j = 0; j < ( _nj + 1 ); k = 0, j++ )
        {
            while( k < ( _nk + 1 ) )
            {
                n = check_edge( i, j, k, 4 );
                if( n > k )
                {
                    make_connection( i, j, k, 4, n );
                    k = n + 1;
                }
                else
                    k++;
            }
        }

    return 1;
}

/*-----------------------------------------------------------------------*/

void DBConverter::swap_4_range( char *mem_ptr1, int num )
{
    char *pos;

    pos = mem_ptr1;

    for( int i = 0; i < num; i++ )
    {
        swap_4( pos );
        pos += 4;
    }
}

/*-----------------------------------------------------------------------*/

void DBConverter::swap_4( char* data )
{
    char b;
    b = data[0];
    data[0] = data[3];
    data[3] = b;
    b = data[1];
    data[1] = data[2];
    data[2] = b;
}

/*-----------------------------------------------------------------------*/

int DBConverter::read_functions( std::set<std::string> scalars,
                                     std::map<std::string, int>& func_map,
                                     std::vector<std::vector<float> > & func,
                                     std::vector<float>& vel_func )
{
    int num = _names.size(), n;
    for( n = 0; n < num; n++ ) scalars.insert( _names[n] );

    FILE *s1;

    int i, j, k, l, m;

    int numelements = _ni * _nj * _nk;

    func_map.clear();
    func.clear();
    vel_func.clear();

    //# open db file
    if (( s1 = fopen( _db_file.c_str(), "rb" ) ) == NULL )
    {
        cerr << "Failed to open " << _db_file << endl;
        return 0;
    }

    // Offset to beginning of function data
    fseek( s1, 0x14C + 0x8*( _ns + _nv ) + ( _ni + _nj + _nk )*sizeof( float ) + 24, SEEK_SET );

    std::set<std::string>::iterator iter;
    int index, floatSize = sizeof( float );
    float val;

    for( l = 0; l < _ns + _nv; l++ )
    {
        iter = scalars.find( _names[l] );
        if( _names[l] != "vel" )
        {
            if( iter != scalars.end() )
            {
                //std::vector<float> func_now(numelements);
                int sz = func.size();
                func.resize( sz + 1 );
                func[sz].resize( numelements );
                // scalar function
                for( k = 0; k < _nk; k++ )
                {
                    for( j = 0; j < _nj; j++ )
                    {
                        for( i = 0; i < _ni; i++ )
                        {
                            fread( &func[sz][i *( _nj * _nk ) + j * _nk + k], floatSize, 1, s1 );
                            //fread(&val, floatSize, 1, s1);
                            //func_now[i * (_nj * _nk) + j * _nk + k] = val;
                        }
                    }
                }
                //index = func.size();
                //func_map[_names[l]] = index;
                func_map[_names[l]] = sz;
                //func.push_back(func_now);
            }
            else
            {
                fseek( s1, numelements*floatSize, SEEK_CUR );
            }
            fseek( s1, 8L, SEEK_CUR );
        }
        else
        {
            if( iter != scalars.end() )
            {
                vel_func.resize( numelements*3 );
                // vector function
                for( m = 0; m < 3; m++ )
                {
                    for( k = 0; k < _nk; k++ )
                    {
                        for( j = 0; j < _nj; j++ )
                        {
                            for( i = 0; i < _ni; i++ )
                            {
                                fread( &vel_func[( i *( _nj * _nk ) + j * _nk + k ) * 3 + m], floatSize, 1, s1 );
                                //fread(&val, floatSize, 1, s1);
                                //vel_func[(i * (_nj * _nk) + j * _nk + k) * 3 + m] = val;
                            }
                        }
                    }
                    fseek( s1, 8L, SEEK_CUR );
                }
            }
            else
            {
                fseek( s1, 3*numelements*floatSize + 3*8L, SEEK_CUR );
            }
            fseek( s1, 8L, SEEK_CUR );
        }
    }

    if( vel_func.size() == 0 ) vel_func.resize( numelements*3 ); // MIKE, a hack

    // Close DB file
    fclose( s1 );

    return 1;
}

/*-----------------------------------------------------------------------*/

int DBConverter::makeVTK( std::string db_path, std::string wr_path, std::set<std::string> scalars )
{
    FILE *fptr;
    fptr = fopen( _db_file.c_str(), "rb" );

    if( fptr == NULL ) return 0;
    else fclose( fptr );

    FILE *VTK_FILE;

#ifdef WANTASCII
    ofstream VTK_AFILE;
    VTK_AFILE.open( "db_ascii.vtk" );
#endif

    int floatSize = sizeof( float );
    int i, j, k, m;

    int numelements = _ni * _nj * _nk;

    std::map<std::string, int>       func_map;
    std::vector<std::vector<float> > func;
    std::vector<float>               vel_func;

    //# Read in functions
    if( !read_functions( scalars, func_map, func, vel_func ) )
    {
        cerr << "Error in extracting scalars from " << _db_file << endl;
        return 0;
    }

    //# Open vtk binary file
    if (( VTK_FILE = fopen( db_path.c_str(), "wb" ) ) == NULL )
    {
        cerr << "Failed to open " << db_path << endl;
        return 0;
    }

    //# Write out HEADER
    fprintf( VTK_FILE, "# vtk DataFile Version 4.0\n" );
    fprintf( VTK_FILE, "Data from %s\n", _db_file.c_str() );
    fprintf( VTK_FILE, "BINARY\n" );
    fprintf( VTK_FILE, "DATASET STRUCTURED_GRID\n" );
    fprintf( VTK_FILE, "DIMENSIONS %d %d %d\n", _ni, _nj, _nk );
    fprintf( VTK_FILE, "POINTS %d float\n", numelements );

#ifdef WANTASCII
    VTK_AFILE << "# vtk DataFile Version 4.0\n";
    VTK_AFILE << "Data from %s" << _db_file.c_str() << endl;
    VTK_AFILE << "ASCII\n";
    VTK_AFILE << "DATASET STRUCTURED_GRID\n";
    VTK_AFILE << "DIMENSIONS " << _ni << " " << _nj << " " << _nk << endl;
    VTK_AFILE << "POINTS " << numelements << " float\n";

    //# Write out POINTS
    for( k = 0; k < _nk; k++ )
        for( j = 0; j < _nj; j++ )
            for( i = 0; i < _ni; i++ )
                VTK_AFILE << _x_coord[i] << " " << _y_coord[j] << " " << _z_coord[k] << endl;
#endif

    //# Assinine byte swapping for vtk
    swap_4_range(( char* )_x_coord, _ni );
    swap_4_range(( char* )_y_coord, _nj );
    swap_4_range(( char* )_z_coord, _nk );

    //# Write out POINTS
    for( k = 0; k < _nk; k++ )
        for( j = 0; j < _nj; j++ )
            for( i = 0; i < _ni; i++ )
            {
                fwrite(( char* )&_x_coord[i], floatSize, 1, VTK_FILE );
                fwrite(( char* )&_y_coord[j], floatSize, 1, VTK_FILE );
                fwrite(( char* )&_z_coord[k], floatSize, 1, VTK_FILE );
            }

    //# Swap back
    swap_4_range(( char* )_x_coord, _ni );
    swap_4_range(( char* )_y_coord, _nj );
    swap_4_range(( char* )_z_coord, _nk );

    //# Write out BLANKING
#if 0
#ifdef WANTASCII
    VTK_AFILE << "BLANKING " << numelements << " unsigned_char\n";
#endif

    fprintf( VTK_FILE, "\nBLANKING %d unsigned_char\n", numelements );
    for( k = 0; k < _nk; k++ )
        for( j = 0; j < _nj; j++ )
            for( i = 0; i < _ni; i++ )
            {
                char ct = !_cell_type[i*( _nj*_nk )+j*_nk+k];
                fwrite( &ct, sizeof( unsigned char ), 1, VTK_FILE );

#ifdef WANTASCII
                if( !_cell_type[i*( _nj*_nk )+j*_nk+k] ) VTK_AFILE << "1\n";
                else VTK_AFILE << "0\n";
#endif

            }
#endif



    std::map<std::string, int>::iterator mapIter;
    std::map<std::string, int>::iterator it;
    it = func_map.find( "walls" );
    if( it != func_map.end() )
    {
        ofstream minmax;
        minmax.open( wr_path.c_str() );
        for( mapIter = func_map.begin(); mapIter != func_map.end(); mapIter++ )
        {
            float min = 1.0e30, max = -1.0e30;
            if( mapIter->first == "wall_tmp" || mapIter->first == "hconv" )
            {
                for( k = 0; k < _nk; k++ )
                    for( j = 0; j < _nj; j++ )
                        for( i = 0; i < _ni; i++ )
                        {
                            int icell = ( int )func[it->second][i*( _nj*_nk )+j*_nk+k];
                            int icellnbr[6] = { -1, -1, -1, -1, -1, -1};
                            if( i != _ni - 1 ) icellnbr[0] = ( int )func[it->second][( i+1 )*( _nj*_nk )+j*_nk+k];
                            if( i != 0 ) icellnbr[1] = ( int )func[it->second][( i-1 )*( _nj*_nk )+j*_nk+k];
                            if( j != _nj - 1 ) icellnbr[2] = ( int )func[it->second][i*( _nj*_nk )+( j+1 )*_nk+k];
                            if( j != 0 ) icellnbr[3] = ( int )func[it->second][i*( _nj*_nk )+( j-1 )*_nk+k];
                            if( k != _nk - 1 ) icellnbr[4] = ( int )func[it->second][i*( _nj*_nk )+j*_nk+k+1];
                            if( k != 0 ) icellnbr[5] = ( int )func[it->second][i*( _nj*_nk )+j*_nk+k-1];
                            int l;
                            bool nbrflow = false;
                            for( l = 0; l < 6; l++ ) if( icellnbr[l] == 7 ) nbrflow = true;
                            if( icell == 8 && nbrflow )
                            {
                                if( func[mapIter->second][i*( _nj*_nk )+j*_nk+k] < min ) min = func[mapIter->second][i*( _nj*_nk )+j*_nk+k];
                                if( func[mapIter->second][i*( _nj*_nk )+j*_nk+k] > max ) max = func[mapIter->second][i*( _nj*_nk )+j*_nk+k];
                            }
                        }
            }
            else if( mapIter->first == "inc_wall" || mapIter->first == "net_wall" )
            {
                for( k = 0; k < _nk; k++ )
                    for( j = 0; j < _nj; j++ )
                        for( i = 0; i < _ni; i++ )
                        {
                            int icell = ( int )func[it->second][i*( _nj*_nk )+j*_nk+k];
                            int icellnbr[6] = { -1, -1, -1, -1, -1, -1};
                            if( i != _ni - 1 ) icellnbr[0] = ( int )func[it->second][( i+1 )*( _nj*_nk )+j*_nk+k];
                            if( i != 0 ) icellnbr[1] = ( int )func[it->second][( i-1 )*( _nj*_nk )+j*_nk+k];
                            if( j != _nj - 1 ) icellnbr[2] = ( int )func[it->second][i*( _nj*_nk )+( j+1 )*_nk+k];
                            if( j != 0 ) icellnbr[3] = ( int )func[it->second][i*( _nj*_nk )+( j-1 )*_nk+k];
                            if( k != _nk - 1 ) icellnbr[4] = ( int )func[it->second][i*( _nj*_nk )+j*_nk+k+1];
                            if( k != 0 ) icellnbr[5] = ( int )func[it->second][i*( _nj*_nk )+j*_nk+k-1];
                            int l;
                            bool nbrflow = false;
                            for( l = 0; l < 6; l++ ) if( icellnbr[l] == 7 ) nbrflow = true;
                            if (( icell == 8 && nbrflow ) || icell == 1 || icell == 2 || icell == 3 )
                            {
                                if( func[mapIter->second][i*( _nj*_nk )+j*_nk+k] < min ) min = func[mapIter->second][i*( _nj*_nk )+j*_nk+k];
                                if( func[mapIter->second][i*( _nj*_nk )+j*_nk+k] > max ) max = func[mapIter->second][i*( _nj*_nk )+j*_nk+k];
                            }
                        }
            }
            else
            {
                for( i = 0; i < numelements; i++ )
                {
                    int icell = ( int )func[it->second][i];
                    if( icell == 7 || icell == 1 || icell == 2 || icell == 3 )
                    {
                        if( func[mapIter->second][i] < min ) min = func[mapIter->second][i];
                        if( func[mapIter->second][i] > max ) max = func[mapIter->second][i];
                    }
                }
            }
            //cout << mapIter->first <<" "<<min<<" "<<max<<endl;
            minmax << mapIter->first << " " << min << " " << max << endl;
        }
        minmax.close();
    }

#ifdef WANTASCII
    VTK_AFILE << "POINT_DATA " << numelements << endl;
    //# Write out SCALARS
    for( mapIter = func_map.begin(); mapIter != func_map.end(); mapIter++ )
    {
        VTK_AFILE << "SCALARS " << mapIter->first.c_str() << " float 1\n";
        VTK_AFILE << "LOOKUP_TABLE default\n";
        for( k = 0; k < _nk; k++ )
            for( j = 0; j < _nj; j++ )
                for( i = 0; i < _ni; i++ )
                    VTK_AFILE << func[mapIter->second][i*( _nj*_nk )+j*_nk+k] << endl;
    }

    //# Write out VECTORS
    VTK_AFILE << "VECTORS vel float\n";
    for( k = 0; k < _nk; k++ )
        for( j = 0; j < _nj; j++ )
            for( i = 0; i < _ni; i++ )
                for( m = 0; m < 3; m++ )
                    VTK_AFILE << vel_func[( i*( _nj*_nk )+j*_nk+k )*3+m] << endl;
#endif


    //# Assinine byte swapping for vtk
    for( mapIter = func_map.begin(); mapIter != func_map.end(); mapIter++ )
        for( i = 0; i < numelements; i++ )
            swap_4(( char * )&func[mapIter->second][i] );
    for( i = 0; i < numelements*3; i++ )
        swap_4(( char * )&vel_func[i] );

    fprintf( VTK_FILE, "\nPOINT_DATA %d", numelements );

    //# Write out SCALARS
    for( mapIter = func_map.begin(); mapIter != func_map.end(); mapIter++ )
    {
        fprintf( VTK_FILE, "\nSCALARS %s float 1", mapIter->first.c_str() );
        fprintf( VTK_FILE, "\nLOOKUP_TABLE default\n" );
        for( k = 0; k < _nk; k++ )
            for( j = 0; j < _nj; j++ )
                for( i = 0; i < _ni; i++ )
                    fwrite(( char* )&func[mapIter->second][i*( _nj*_nk )+j*_nk+k], floatSize, 1, VTK_FILE );
    }

    //# Write out VECTORS
    fprintf( VTK_FILE, "\nVECTORS vel float\n" );
    for( k = 0; k < _nk; k++ )
        for( j = 0; j < _nj; j++ )
            for( i = 0; i < _ni; i++ )
                for( m = 0; m < 3; m++ )
                    fwrite(( char* )&vel_func[( i*( _nj*_nk )+j*_nk+k )*3+m], floatSize, 1, VTK_FILE );

    fclose( VTK_FILE );

#ifdef WANTASCII
    VTK_AFILE.close();
#endif

    //# Swap back
    //for(mapIter=func_map.begin(); mapIter!=func_map.end(); mapIter++)
    //  for(i=0; i<numelements; i++)
    //    swap_4((char *)&func[mapIter->second][i]);
    //for(i=0; i<numelements*3; i++)
    //  swap_4((char *)&vel_func[i]);

    //# Write out a WIREFRAME
    /*ofstream WIRE_FILE;
    WIRE_FILE.open(wr_path.c_str());

    WIRE_FILE << "# vtk DataFile Version 1.0\n"
      << "Wireframe for " << _db_file << endl
      << "ASCII\n\n"
      << "DATASET POLYDATA\n"
      << "POINTS " << _positions_i.size() << " float\n\n";

    for(i=0; i<(int)_positions_i.size(); i++)
      WIRE_FILE << _x_edge[_positions_i[i]] << " "
         << _y_edge[_positions_j[i]] << " "
         << _z_edge[_positions_k[i]] << endl;
     
    WIRE_FILE << "\nLINES"
      << " " << _connections.size()
      << " " << _connections.size()*3 << endl;

    for(i=0; i<(int)_connections.size(); i++)
      WIRE_FILE << "2"
        << " " << _connections[i].first
        << " " << _connections[i].second << endl;
     
    WIRE_FILE.close();*/

    return 1;
}

/*-----------------------------------------------------------------------*/

int DBConverter::makeDX( std::string path, std::string dx_file, std::string dxb_file,
                         std::set<std::string> scalars,
                             bool mark_invalids, bool incl_wireframe )
{
    FILE *s2;
    ofstream DX_FILE;

    int i, j, k, m;

    long int offset      = 0;
    float    zero        = 0.0;
    int      numelements = _ni * _nj * _nk;
    int      floatSize   = sizeof( float );

    std::map<std::string, int>       func_map;
    std::vector<std::vector<float> > func;
    std::vector<float>               vel_func;

    //# Read in functions
    if( !read_functions( scalars, func_map, func, vel_func ) )
    {
        cerr << "Error in extracting scalars from " << _db_file << endl;
        return 0;
    }

    //# Open DX binary file
    if (( s2 = fopen(( path + "/" + dxb_file ).c_str(), "wb" ) ) == NULL )
    {
        cerr << "Failed to open " << dxb_file << endl;
        return 0;
    }

    //# Open DX ASCII file
    DX_FILE.open(( path + "/" + dx_file ).c_str() );

    //# X-Coordinates
    DX_FILE << "\nobject 1 class array type float rank 1 shape 3 items "
    << _ni << " lsb binary\n"
    << "data file " << dxb_file << ", " << offset << endl;;
    for( i = 0; i < _ni; i++ )
    {
        fwrite( &_x_coord[i], floatSize, 1, s2 );
        fwrite( &zero, floatSize, 1, s2 );
        fwrite( &zero, floatSize, 1, s2 );
    }
    offset += _ni * 3 * floatSize;

    //# Y-Coordinates
    DX_FILE << "\nobject 2 class array type float rank 1 shape 3 items "
    << _nj << " lsb binary\n"
    << "data file " << dxb_file << ", " << offset << endl;;
    for( j = 0; j < _nj; j++ )
    {
        fwrite( &zero, floatSize, 1, s2 );
        fwrite( &_y_coord[j], floatSize, 1, s2 );
        fwrite( &zero, floatSize, 1, s2 );
    }
    offset += _nj * 3 * floatSize;

    //# Z-Coordinates
    DX_FILE << "\nobject 3 class array type float rank 1 shape 3 items "
    << _nk << " lsb binary\n"
    << "data file " << dxb_file << ", " << offset << endl;;
    for( k = 0; k < _nk; k++ )
    {
        fwrite( &zero, floatSize, 1, s2 );
        fwrite( &zero, floatSize, 1, s2 );
        fwrite( &_z_coord[k], floatSize, 1, s2 );
    }
    offset += _nk * 3 * floatSize;

    DX_FILE << "\nobject 4 class productarray \n"
    << "  term 1\n"
    << "  term 2\n"
    << "  term 3\n";

    DX_FILE << "\nobject 5 class gridconnections counts "
    << _ni << " " << _nj << " " << _nk << endl;

    int obj = 6;

#if 0
    //# IBLANKING
    if( mark_invalids )
    {
        DX_FILE << "\nobject " << obj << " class array type byte rank 0 items "
        << numelements << " lsb binary\n"
        << "data file " << dxb_file << ", " << offset << endl;
        for( i = 0; i < _ni; i++ )
            for( j = 0; j < _nj; j++ )
                for( k = 0; k < _nk; k++ )
                {
                    char ct = ( char )_cell_type[i * ( _nj * _nk ) + j * _nk + k];
                    fwrite( &ct, sizeof( char ), 1, s2 );
                }
        DX_FILE << "attribute \"dep\" string \"positions\"\n";

        offset += numelements * sizeof( char );
        obj++;
    }
#endif

    //# SCALARS
    std::map<std::string, int>::iterator mapIter;
    for( mapIter = func_map.begin(); mapIter != func_map.end(); mapIter++ )
    {
        DX_FILE << "\nobject " << obj << " class array type float rank 0 items "
        << numelements << " lsb binary" << endl
        << "data file " << dxb_file << ", " << offset << endl;
        DX_FILE << "\nobject \"" << mapIter->first << "\" class field\n"
        << "component \"positions\" value 4\n"
        << "component \"connections\" value 5\n"
        << "component \"data\" value " << obj << endl;
        if( mark_invalids )
            DX_FILE << "component \"invalid positions\" value 6\n";

        for( i = 0; i < _ni; i++ )
        {
            for( j = 0; j < _nj; j++ )
            {
                for( k = 0; k < _nk; k++ )
                {
                    fwrite( &func[mapIter->second][i*( _nj*_nk )+j*_nk+k], floatSize, 1, s2 );
                }
            }
        }
        offset += numelements * floatSize;
        obj++;
    }

    //# VECTORS
    DX_FILE << "\nobject " << obj << " class array type float rank 1 shape 3 items "
    << numelements << " lsb binary\n"
    << "data file " << dxb_file << ", " << offset << endl;
    DX_FILE << "\nobject \"vel\" class field\n"
    << "component \"positions\" value 4\n"
    << "component \"connections\" value 5\n"
    << "component \"data\" value " << obj << endl;
    if( mark_invalids )
        DX_FILE << "component \"invalid positions\" value 6\n";

    for( i = 0; i < _ni; i++ )
    {
        for( j = 0; j < _nj; j++ )
        {
            for( k = 0; k < _nk; k++ )
            {
                for( m = 0; m < 3; m++ )
                {
                    fwrite( &vel_func[( i*( _nj*_nk )+j*_nk+k )*3+m], floatSize, 1, s2 );
                }
            }
        }
    }
    offset += numelements * 3 * floatSize;
    obj++;

    //# WIREFRAME
    if( incl_wireframe )
    {
        //# Wireframe positions and  connections
        DX_FILE << "\nobject " << obj << " class array type float category real rank 1 shape 3 items "
        << _positions_i.size() << " lsb binary\n"
        << "data file " << dxb_file << ", " << offset << endl;
        for( i = 0; i < ( int )_positions_i.size(); i++ )
        {
            fwrite( &_x_edge[_positions_i[i]], floatSize, 1, s2 );
            fwrite( &_y_edge[_positions_j[i]], floatSize, 1, s2 );
            fwrite( &_z_edge[_positions_k[i]], floatSize, 1, s2 );
        }
        DX_FILE << "attribute \"dep\" string \"positions\"\n";

        offset += _positions_i.size() * 3 * floatSize;
        obj++;

        DX_FILE << "\nobject " << obj << " class array type int category real rank 1 shape 2 items "
        << _connections.size() << " lsb binary\n"
        << "data file " << dxb_file << ", " << offset << endl;
        for( i = 0; i < ( int )_connections.size(); i++ )
        {
            fwrite( &_connections[i].first, floatSize, 1, s2 );
            fwrite( &_connections[i].second, floatSize, 1, s2 );
        }

        DX_FILE << "attribute \"ref\" string \"positions\"\n"
        << "attribute \"element type\" string \"lines\"\n";
        DX_FILE << "\nobject \"wireframe\" class field\n"
        << "  component \"positions\" value " << obj - 1 << endl
        << "  component \"connections\" value " << obj << endl;

        offset += _connections.size() * 2 * floatSize;
        obj++;
    }

    //# GROUP
    DX_FILE << "\nobject \"all 3D fields\" class group\n";

    if( incl_wireframe )
        DX_FILE << "member \"wireframe\" \"wireframe\"\n";

    for( mapIter = func_map.begin(); mapIter != func_map.end(); mapIter++ )
        DX_FILE << "member \"" << mapIter->first << "\" \"" << mapIter->first << "\"\n";

    DX_FILE << "member \"vel\" \"vel\"\n";

    //# Close files
    DX_FILE.close();
    fclose( s2 );

    return 1;
}

/*-----------------------------------------------------------------------*/

int DBConverter::check_edge( int i, int j, int k, int dir )
{
    int n;
    int out  = 0;
    int wall = 0;

    int me       = i * ( _nj + 1 ) * ( _nk + 1 ) + j * ( _nk + 1 ) + k;
    int neighbor = edge_in_dir( me, dir );

    if( neighbor < 0 ) return -1;

    switch ( dir )
    {

        case 0:
            if( i >= _ni )           return -1;
            if( j == _nj || k == _nk ) out++;
            else                 wall =  _cell_type[i*_nj*_nk+j*_nk+k];
            if( j == 0 || k == _nk )   out++;
            else                 wall += _cell_type[i*_nj*_nk+( j-1 )*_nk+k];
            if( k == 0 || j == _nj )   out++;
            else                 wall += _cell_type[i*_nj*_nk+j*_nk+( k-1 )];
            if( j == 0 || k == 0 )     out++;
            else                 wall += _cell_type[i*_nj*_nk+( j-1 )*_nk+( k-1 )];

            if( wall % 2 && out != 3 )
            {
                n = check_edge( i + 1, j, k, 0 );
                if( n > 0 ) return n;
                else    return i + 1;
            }

            break;

        case 2:
            if( j >= _nj )           return -1;
            if( i == _ni || k == _nk ) out++;
            else                 wall =  _cell_type[i*_nj*_nk+j*_nk+k];
            if( i == 0 || k == _nk )   out++;
            else                 wall += _cell_type[( i-1 )*_nj*_nk+j*_nk+k];
            if( k == 0 || i == _ni )   out++;
            else                 wall += _cell_type[i*_nj*_nk+j*_nk+( k-1 )];
            if( i == 0 || k == 0 )     out++;
            else                 wall += _cell_type[( i-1 )*_nj*_nk+j*_nk+( k-1 )];

            if( wall % 2 && out != 3 )
            {
                n = check_edge( i, j + 1, k, 2 );
                if( n > 0 ) return n;
                else    return j + 1;
            }

            break;

        case 4:
            if( k >= _nk )           return -1;
            if( i == _ni || j == _nj ) out++;
            else                 wall =  _cell_type[i*_nj*_nk+j*_nk+k];
            if( i == 0 || j == _nj )   out++;
            else                 wall += _cell_type[( i-1 )*_nj*_nk+j*_nk+k];
            if( j == 0 || i == _ni )   out++;
            else                 wall += _cell_type[i*_nj*_nk+( j-1 )*_nk+k];
            if( i == 0 || j == 0 )     out++;
            else                 wall += _cell_type[( i-1 )*_nj*_nk+( j-1 )*_nk+k];

            if( wall % 2 && out != 3 )
            {
                n = check_edge( i, j, k + 1, 4 );
                if( n > 0 ) return n;
                else    return k + 1;
            }

            break;

        default:
            cerr << "check_edge says: huh?\n";
            break;
    }

    return -1;
}

/*-----------------------------------------------------------------------*/

void DBConverter::make_connection( int i, int j, int k, int dir, int index )
{
    int me, neighbor, sz;
    int _i = i, _j = j, _k = k;

    switch ( dir )
    {

        case 0:
            _i = index;
            if( _i <= i ) return;
            break;
        case 2:
            _j = index;
            if( _j <= j ) return;
            break;
        case 4:
            _k = index;
            if( _k <= k ) return;
            break;
        default:
            cerr << "make_connection says: huh?\n";
            return;
            break;
    }

    me       =   i * ( _nj + 1 ) * ( _nk + 1 ) +  j * ( _nk + 1 ) +  k;
    neighbor =  _i * ( _nj + 1 ) * ( _nk + 1 ) + _j * ( _nk + 1 ) + _k;

    if( _map_points.find( me ) == _map_points.end() )
    {
        _positions_i.push_back( i );
        _positions_j.push_back( j );
        _positions_k.push_back( k );
        sz = _map_points.size();
        _map_points[me] = sz;
    }
    if( _map_points.find( neighbor ) == _map_points.end() )
    {
        _positions_i.push_back( _i );
        _positions_j.push_back( _j );
        _positions_k.push_back( _k );
        sz = _map_points.size();
        _map_points[neighbor] = sz;
    }

    _connections.push_back( std::make_pair( _map_points[me], _map_points[neighbor] ) );
}
/*-----------------------------------------------------------------------*/

int DBConverter::edge_in_dir( int me, int direction )
{
    int index = 0;
    int nip = _ni + 1;
    int njp = _nj + 1;
    int nkp = _nk + 1;

    /* returns index of edge from me in direction direction */
    switch ( direction )
    {
        case 0: // x pos
            index = me + njp * nkp;
            break;
        case 1: // x neg
            index = me - njp * nkp;
            break;
        case 2: // y pos
            index = me + nkp;
            break;
        case 3: // y neg
            index = me - nkp;
            break;
        case 4: // z pos
            index = me + 1;
            break;
        case 5: // z neg
            index = me - 1;
            break;
        default: // huh?
            cerr << "edge_in_dir says: huh?\n";
            break;
    }

    if( index >= ( nip*njp*nkp ) || index < 0 )
        return -1;

    return index;
}

/*-----------------------------------------------------------------------*/

void DBConverter::get_ijk_point( int index, int &i, int &j, int &k )
{
    int njp = _nj + 1;
    int nkp = _nk + 1;

    i = ( int )( index / ( njp * nkp ) );
    j = ( int )(( index - i * njp * nkp ) / nkp );
    k = index - j * nkp - i * njp * nkp;
}

//} // End namespace REI
