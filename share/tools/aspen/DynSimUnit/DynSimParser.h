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

#ifndef DYNSIMPARSER_H
#define DYNSIMPARSER_H

#include <string>
#include <map>
#include <vector>
#include <iostream>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <atlsafe.h>
#include <atlbase.h>
#include <atlcom.h>
#include <comdef.h>
#import "gbda_aut.tlb"
using namespace GBDAAutomation;

class DynSimParser
{
public:
    DynSimParser();
    std::string CreateNetwork( std::string filename );
    int OpenFile( std::string filename );
    //bool ConnectToOPC();
    std::vector< std::pair< std::string, std::string > > ReadVars();
	//std::string GetOPCValue( const std::string& );
	std::string GetOPCValues( );
	//void ConnectWithList( std::vector< std::string > list );
	bool ConnectToOPCServer( );
	std::string GetAllOPCVariables( const std::string& );
	void AddOPCVariable( const std::string& );

private:
	void ParseTreeFile( std::string );
	void DynSimParser::UpdateOPCList( );
    void InitializeParser();
    void ParseFlowsheets();
    void ParseObjects( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* );
    void PopulateBlocks( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* );
    std::string CreateVESNetwork();
    std::string GetDynSimIconPath( std::string, std::string );

    std::string m_fileName;
    XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument *mCommandDocument;
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* root_elem;
    XERCES_CPP_NAMESPACE_QUALIFIER XercesDOMParser* mParser;
    XERCES_CPP_NAMESPACE_QUALIFIER ErrorHandler* mErrHandler;
    
    typedef struct
    {
        int id;
        int x; //relative
        int y; //relative
        std::string streamName;
        std::string dataFlow;
        std::string modelName;
        int modelId;
    }port;

    typedef struct
    {
        int id;
        std::string name;
        std::string cls;
        std::string imageType;
        int x;
        int y;
        int width;
        int height;
        std::vector< port > ports;
    }block;

    typedef struct
    {
        std::string name;
        std::string cls;
        int x;
        int y;
        int width;
        int height;
        std::vector< std::pair< int, int > > vtx;
        std::vector< port > ports;
    }stream;

    typedef struct
    {
        int id;
        std::string name;
        std::string cls;
        std::map< std::string, block > blocks;
        std::map< std::string, block > others;
        std::map< std::string, stream > streams;
    }flowsheet;

    std::map< std::string, block > blocks;
    std::map< std::string, stream > streams;
    std::map< std::string, block > others;
    std::map< std::string, flowsheet > flowsheets;
    std::vector< std::pair< std::string, std::string > > nameAndValues;
	std::string m_opcFlowsheetName;
	std::vector< std::string > m_opcBlocks;
	std::vector< std::string > m_opcVariables;

    
    CComSafeArray<long> * serverID;
    CComSafeArray<BSTR> * itemIDs;
    CComSafeArray<long> * clientID;
	IOPCAutoServerPtr m_server;
    OPCItemsPtr items;
    IOPCGroupPtr group;
    IOPCGroupsPtr groups;
};
#endif
