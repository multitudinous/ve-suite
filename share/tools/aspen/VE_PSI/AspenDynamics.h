/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#ifndef ASPENDYNAMICS_H
#define ASPENDYNAMICS_H

#include <fstream>
#include <iostream>
#include <sstream>
#include <map>
#include <vector>
#include <string>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
#include <AspenDynamicsInterface.h>

class AspenDynamics
{

private:
    std::vector<float> xCoords;
    std::vector<float> yCoords;
    std::map< std::string, std::map< std::string, std::pair< float, float > > > iconLocations;
    
    typedef struct
    {
        std::string id;
        std::string type;
        std::string icon;
        float scale;
        float rotation;
        int mirror;
        bool hierarchical;
        bool iconHidden;
    }BlockInfo;

    std::map< std::string, std::map< std::string, BlockInfo > > BlockInfoList;

    typedef struct
    {    
        std::string streamId;
        int streamType;
        std::vector< std::pair< float, float > > value;    
    }streamXY;

    streamXY xy;
    streamXY tempXY;
    std::string currentLevelName;
    int levelCount;
    std::vector< std::string > m_adVariables;
    std::vector< streamXY > streamCoordList;
    std::map< std::string, std::map< std::string, std::vector< std::pair< float, float > > > > linkPoints;
    std::map< std::string, std::map< std::string, int > > linkTypes;
    std::map< std::string, std::map< std::string, std::string > > inLinkToModel;
    std::map< std::string, std::map< std::string, std::string > > outLinkToModel;
    std::map<std::string, std::map< std::string, int > >models;
    std::map< std::string, std::pair< int, int > > streamPortIDS;
    int redundantID;
    std::string m_workingDir;
    std::string m_unitName;

    void CreateNetworkLinks( ves::open::xml::model::NetworkPtr subNetwork, std::string hierName );
    void ParseSubSystem(ves::open::xml::model::ModelPtr model, std::string networkName);
    void NewParseFile(const char * dynFile);
    void ReadHeader( std::ifstream &file );
    void ReadEncrypted( std::ifstream &file );
    void FindSystemData( std::ifstream &file );
    void ReadSystemData( std::ifstream &file );
    void ReadFlowsheetComponents( std::ifstream &file );
    void ReadConstraints( std::ifstream &file );
    void ReadGraphicsInformation( std::ifstream &file );
    bool PeekFlowsheet( std::ifstream &file );
    //void NormalizeForWX();
    void FindNextEntry( std::ifstream &file );

public:
    AspenDynamics( std::string workingDir, std::string unitName );
    ~AspenDynamics();
    void ParseFile( const char * file);
    std::string CreateNetwork( void );    
    void SetWorkingDir( std::string dir );
    void OpenFile( const char * file );
    void CloseFile( );
    void SaveFile( );
    void SaveAs( const char * filename );
    void SetVisibility( bool show );
    void ReinitDynamics();
    std::string GetModuleParams( std::string modname, bool block );
    void SetValue( std::string modname, std::string paramname,
        std::string value, bool block );
    AspenDynamicsInterface::AspenDynamicsInterface * dyndoc;
    void AddADVariable( const std::string& var );
    std::string GetADValues( );
    bool IsADVarsEmpty();
    int NumADVars();
};

#endif