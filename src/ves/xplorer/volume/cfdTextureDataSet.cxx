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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef _OSG
#include <ves/xplorer/volume/cfdTextureDataSet.h>
#include <ves/xplorer/volume/cfdVolumeVisualization.h>
#include <ves/xplorer/volume/cfdTextureManager.h>
#include <ves/xplorer/volume/TBVolumeSlices.h>

#include <ves/xplorer/util/fileIO.h>

#include <iostream>
#include <fstream>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>

using namespace ves::xplorer::volume;
//////////////////////////////////
//Constructor                   //
//////////////////////////////////
TextureDataInfo::TextureDataInfo()
        : _name( " " ), _tm( 0 )
{}
////////////////////////////////////////////////////////////
TextureDataInfo::TextureDataInfo( const TextureDataInfo& tdi )
        : _name( tdi._name ), _tm( new cfdTextureManager( *tdi._tm ) )
{}
///////////////////////////////////
TextureDataInfo::~TextureDataInfo()
{
    _name.clear();
    if( _tm )
    {
        delete _tm;
        _tm = 0;
    }
}
///////////////////////////////////////////////
void TextureDataInfo::SetName( std::string name )
{
    _name = name;
}
//////////////////////////////////////////////////////////////
void TextureDataInfo::SetTextureManager( cfdTextureManager* tm )
{
    _tm = tm;
}
//////////////////////////////////////
std::string TextureDataInfo::GetName()
{
    return _name;
}
///////////////////////////////////////////////////////
cfdTextureManager* TextureDataInfo::GetTextureManager()
{
    if( _tm )
    {
        return _tm;
    }
    return 0;
}
///////////////////////////////////////////////////////////////////////
TextureDataInfo& TextureDataInfo::operator=( const TextureDataInfo& tdi )
{
    if( this != &tdi )
    {
        _name = tdi._name;
        _tm = tdi._tm;
    }
    return *this;
}
//////////////////////////////////////
//Constructor                       //
//////////////////////////////////////
cfdTextureDataSet::cfdTextureDataSet()
        : _volVisNode( new cfdVolumeVisualization ), _nScalars( 0 ),
        _nVectors( 0 ), _activeTM( 0 ), _activeDataType( SCALAR )
{}
///////////////////////////////////////
//Destructor                         //
///////////////////////////////////////
cfdTextureDataSet::~cfdTextureDataSet()
{
    if( !_fileName.empty() )
    {
        _fileName.clear();
    }
    if( _volVisNode )
    {
        delete _volVisNode;
        _volVisNode = 0;
    }

    for( unsigned int i = 0; i < _nScalars; i++ )
    {
        delete _scalars[i];
    }
    _scalars.clear();
    for( unsigned int i = 0; i < _nVectors; i++ )
    {
        delete _vectors[i];
    }
    _vectors.clear();
}
//////////////////////////////////////////////////////////
void cfdTextureDataSet::SetActiveScalar( std::string name )
{
    int whichScalar = FindScalar( name );
    if( whichScalar >= 0 )
    {
        _activeTM = _scalars.at( whichScalar )->GetTextureManager();
        _activeDataType = SCALAR;
    }
}
//////////////////////////////////////////////////////////
void cfdTextureDataSet::SetActiveVector( std::string name )
{
    int whichVector = FindVector( name );
    if( whichVector >= 0 )
    {
        _activeTM = _vectors.at( whichVector )->GetTextureManager();
        _activeDataType = VECTOR;
    }
}
/////////////////////////////////////////////////////
void cfdTextureDataSet::SetFileName( std::string name )
{
    _fileName = name;
}
/////////////////////////////////////////////////
unsigned int cfdTextureDataSet::NumberOfScalars()
{
    return _nScalars;
}
/////////////////////////////////////////////////
unsigned int cfdTextureDataSet::NumberOfVectors()
{
    return _nVectors;
}
/////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfdTextureDataSet::GetVolumeVisNode()
{
    if( _volVisNode )
    {
        if( _activeTM )
        {
            _volVisNode->SetBoundingBox( _activeTM->getBoundingBox() );
            if( _volVisNode->GetGeometryProxyNode() )
            {
                _volVisNode->GetGeometryProxyNode()->SetTextureDimensions( _activeTM->fieldResolution()[0], _activeTM->fieldResolution()[1], _activeTM->fieldResolution()[2] );
            }
        }
        return _volVisNode;
    }
    return 0;
}
///////////////////////////////////////////////////////////////
cfdTextureDataSet::DataType cfdTextureDataSet::ActiveDataType()
{
    return _activeDataType;
}
////////////////////////////////////////////////////////////////
cfdTextureManager* cfdTextureDataSet::GetActiveTextureManager()
{
    return _activeTM;
}
//////////////////////////////////////////////////////////////
std::string cfdTextureDataSet::ScalarName( unsigned int index )
{
    return _scalarNames.at( index ).c_str();
}
/////////////////////////////////////////////////////////////
std::string cfdTextureDataSet::VectorName( unsigned int index )
{
    return _vectorNames.at( index ).c_str();
}
////////////////////////////////////////////////////////////////////////////////
void cfdTextureDataSet::CreateTextureManager( std::string textureDescriptionFile )
{
    cfdTextureManager* tm = new cfdTextureManager();
    tm->SetUseShaders( true );

    boost::filesystem::path scalarPath( textureDescriptionFile, boost::filesystem::no_check );
    try
    {
        boost::filesystem::is_directory( scalarPath );
    }
    catch ( const std::exception& ex )
    {
        std::cout << ex.what() << std::endl;
        return;
    }

    std::cout << "|\tReading texture description file: "
    << textureDescriptionFile << std::endl;

    std::vector< std::string > files = ves::xplorer::util::fileIO::GetFilesInDirectory( textureDescriptionFile, std::string( "vti" ) );
    for( size_t i = 0; i < files.size(); ++i )
    {
        std::cout << "|\tLoading texture time step file: " << i << " " << files.at( i ) << std::endl;
        tm->addFieldTextureFromFile( files.at( i ) );
    }

    std::cout << "|\tFinished reading texture description file." << std::endl;

    if( tm->GetDataType( 0 ) == cfdTextureManager::SCALAR )
    {
        AddScalarTextureManager( tm );
    }
    else
    {
        AddVectorTextureManager( tm );
    }
}
/////////////////////////////////////////////////////////////////////
void cfdTextureDataSet::AddScalarTextureManager( cfdTextureManager* tm )
{
    TextureDataInfo* td = new TextureDataInfo();
    td->SetName( tm->GetDataName() );
    td->SetTextureManager( tm );
    _scalars.push_back( td );
    _nScalars = _scalars.size();
    _activeTM = tm;
    _scalarNames.push_back( tm->GetDataName() );
}
/////////////////////////////////////////////////////////////////////
void cfdTextureDataSet::AddVectorTextureManager( cfdTextureManager* tm )
{
    TextureDataInfo* td = new TextureDataInfo();
    td->SetName( tm->GetDataName() );
    td->SetTextureManager( tm );
    _vectors.push_back( td );
    _nVectors = _vectors.size();
    _vectorNames.push_back( tm->GetDataName() );
}
///////////////////////////////////////////////////
int cfdTextureDataSet::FindScalar( std::string name )
{
    for( unsigned int i = 0; i < _nScalars; i++ )
    {
        if( _scalars.at( i )->GetName().find( name ) != std::string::npos )
        {
            return i;
        }
    }
    return -1;
}
///////////////////////////////////////////////////
int cfdTextureDataSet::FindVector( std::string name )
{
    for( unsigned int i = 0; i < _nVectors; i++ )
    {
        if( _vectors.at( i )->GetName().find( name ) != std::string::npos )
        {
            return i;
        }
    }
    return -1;
}
#endif
