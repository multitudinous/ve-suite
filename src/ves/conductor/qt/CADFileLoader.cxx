#if 1
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

#include <ves/conductor/qt/CADFileLoader.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADPartPtr.h>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>

namespace ves
{
namespace conductor
{

CADFileLoader::CADFileLoader(  )
{
}

CADFileLoader::~CADFileLoader()
{
}

void CADFileLoader::LoadCADFile( const std::string& fileName, const std::string& parentID )
{
    namespace bfs = boost::filesystem;
    using namespace ves::open::xml;
    using namespace ves::open::xml::cad;
    
    // The filename
    //wxFileName vegFileName( fileName );
    bfs::path vegFileName( fileName );
    
    // Re-write the path as being relative to CWD
    //vegFileName.MakeRelativeTo( ::wxGetCwd() );
    vegFileName = bfs::system_complete( vegFileName );
    
    // Get the entire file path
    //wxString vegFileNamePath( vegFileName.GetFullPath() );
    std::string vegFileNamePath = vegFileName.string();
    
    // Replace backslashes with single forward slash
    //vegFileNamePath.Replace( _( "\\" ), _( "/" ), true );
    while( vegFileNamePath.find( "\\" ) != std::string::npos )
    {
        vegFileNamePath.replace( vegFileNamePath.find( "\\" ), 2, "/" );
    }
    
    // ?
    //wxFileName cadFileName( vegFileNamePath.c_str() );
    bfs::path cadFileName( vegFileNamePath );

    ////////////////////////////////// 
    // Ask user for a part name. Name is stored in partName
    //pop a text dialog to enter the name of the new assembly
// This dialog will no longer happen. Instead, the part will automatically be
// named New_fileBase, where fileBase corresponds to the filename with no path
// and no extension. The user can then rename this part by selecting the name
// in the CADTree and renaming it.
//     wxTextEntryDialog partNameDlg( this,
//                                    _( "New Part Name" ),
//                                    _( "Enter name for new part:" ),
//                                    cadFileName.GetName(), wxOK );
//
//    partNameDlg.CentreOnParent();
//    partNameDlg.ShowModal();
//    std::string partName = ConvertUnicode( partNameDlg.GetValue().GetData() );
    std::string partName = "New_";
    partName.append( bfs::basename( cadFileName ) );
    //////////////////////////////////

    CADPartPtr newCADPart( new CADPart( partName  ) );
    newCADPart->SetCADFileName( vegFileNamePath );
    newCADPart->SetVisibility( true );

    // TODO: Need to have a CADTree before this part can do anything meaningful
    //CADAssemblyPtr tempAssembly = boost::dynamic_pointer_cast<CADAssembly>( _activeCADNode );
    //tempAssembly->AddChild( newCADPart );

    //_geometryTree->AppendItem( _activeTreeNode->GetId(), wxString( newCADPart->GetNodeName().c_str(), wxConvUTF8 ),
    //                           0, 1, new CADTreeBuilder::TreeNodeData( newCADPart ) );
    //_commandName = std::string( "CAD_ADD_NODE" );

    
    //newCADPart->SetParent( _activeCADNode->GetID() );
    if( parentID != "" )
    {
        newCADPart->SetParent( parentID );
    }
    
    
    //_loadedCAD[fileName] = newCADPart;
    
    ves::open::xml::DataValuePairPtr cadNode( new ves::open::xml::DataValuePair() );
    cadNode->SetDataType( std::string( "XMLOBJECT" ) );
    cadNode->SetData( "New Node", newCADPart );
    //_dataValuePairList.push_back( cadNode );
    
    ves::open::xml::CommandPtr cadCommand( new ves::open::xml::Command() );
    cadCommand->SetCommandName( std::string( "CAD_ADD_NODE" ) );
    cadCommand->AddDataValuePair( cadNode );

    //_sendCommandsToXplorer();
    ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( cadCommand );

}

} // namespace conductor
} // namespace ves
#endif