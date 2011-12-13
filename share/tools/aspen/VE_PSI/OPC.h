/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef OPC_H
#define OPC_H
#include <string>
#include <map>
#include <vector>
#include <iostream>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <atlsafe.h>
#include <atlbase.h>
#include <atlcom.h>
#include <objbase.h>
#include <comdef.h>
#include <fstream>

#import "opcproxy.dll"
using namespace OPCDA;
typedef DWORD OPCHANDLE;
typedef tagOPCITEMDEF OPCITEMDEF;
typedef tagOPCITEMRESULT OPCITEMRESULT;
typedef tagOPCITEMSTATE OPCITEMSTATE;
typedef tagOPCBROWSEELEMENT OPCBROWSEELEMENT;
typedef tagOPCITEMVQT OPCITEMVQT;

class OPC
{
public:
    OPC( std::string unitName );
    ///Open the supplied file name in dynsim
    ///\param filename This must be a fully qualified path reference to a file.
    std::vector< std::pair< std::string, std::string > > ReadVars();
    std::string GetOPCValues();
    void SetOPCValues( std::vector< std::pair < std::string, std::string > > );
    bool ConnectToOPCServer();
    std::string GetAllOPCVariables( const std::string& );
    void AddOPCVariable( const std::string& );
    bool IsOPCVarsEmpty();

private:
    //void UpdateOPCList( );
    
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
    std::vector< std::pair< std::string, std::string > > m_MonitorVarsAndVals;
    std::string m_opcFlowsheetName;
    std::vector< std::string > m_opcBlocks;
    std::vector< std::string > m_opcVariables;
    std::string m_unitName;
    std::vector< std::pair< std::string, std::string > > m_AllVarsAndVals;

    
    CComSafeArray<long> * serverID;
    CComSafeArray<BSTR> * itemIDs;
    CComSafeArray<long> * clientID;
    IOPCServer* m_Server;
    IOPCBrowse* browse;  
    IOPCBrowse* browser;
    std::vector< std::string > tempVars;
    std::vector< BSTR > bItemIDs;
    std::ofstream logFile;
    std::vector< LPWSTR > m_szItemID;
        
	IOPCItemMgt* m_AllItemMgt;
	OPCHANDLE m_AllGroup;
	IOPCItemMgt* m_SetItemMgt;
	OPCHANDLE m_SetGroup;
	IOPCItemMgt* m_MonitorItemMgt;
	OPCHANDLE m_MonitorGroup;
    std::vector<OPCHANDLE> m_MonitorServerItem;
    
    void ParseBranch(  _bstr_t name, std::string prefix );

    IOPCServer* InstantiateServer(wchar_t ServerName[]);
    void AddGroup(IOPCServer* pIOPCServer, IOPCItemMgt* &pIOPCItemMgt, OPCHANDLE& hServerGroup, char* name);
    void Parse ( std::string name );
};
#endif
