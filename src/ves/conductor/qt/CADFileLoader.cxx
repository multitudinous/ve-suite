#if 1
/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

#include <ves/conductor/qt/CADFileLoader.h>
#include <ves/conductor/qt/XMLDataBufferEngine.h>

#include <ves/xplorer/command/CommandManager.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADPartPtr.h>
#include <ves/open/xml/model/Model.h>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>

#include <iostream>

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
    //std::cout << "LoadCADFile " << fileName << " with parent " << parentID << std::endl << std::flush;
    namespace bfs = boost::filesystem;
    using namespace ves::open::xml;
    using namespace ves::open::xml::cad;
    
    // The filename
    bfs::path vegFileName( fileName );
    
    // Re-write the path as being relative to CWD
    //vegFileName = bfs::system_complete( vegFileName );
    
    // Get the entire file path
    std::string vegFileNamePath = vegFileName.string();
    
    // Replace backslashes with single forward slash
    while( vegFileNamePath.find( "\\" ) != std::string::npos )
    {
        vegFileNamePath.replace( vegFileNamePath.find( "\\" ), 2, "/" );
    }
    
    // ?
    bfs::path cadFileName( vegFileNamePath );

    // Part will automatically be named New_fileBase, where fileBase corresponds
    // to the filename with no path and no extension. The user can then rename
    // this part by selecting the name in the CADTree and renaming it.
    std::string partName = "New_";
    partName.append( bfs::basename( cadFileName ) );

    CADPartPtr newCADPart( new CADPart( partName  ) );
    newCADPart->SetCADFileName( vegFileNamePath );
    newCADPart->SetVisibility( true );

    // Make this node the child of the top assembly, and tell it who its parent is.
    ves::xplorer::Model* model =
            ves::xplorer::ModelHandler::instance()->GetActiveModel();

    if( model == NULL )
    {
        std::cerr << "Error loading CAD file " << fileName << ": No model present "
                "to which to add CAD. Please select New File from the file menu before "
                "attempting to load CAD." << std::endl << std::flush;
        return;
    }

//    if( model->GetModelCADHandler()->AssemblyExists( parentID ) )
    {
//        CADNodePtr cad = model->GetModelData()->GetGeometry();
        ves::xplorer::scenegraph::DCS* cad =
                model->GetModelCADHandler()->GetAssembly( parentID );
        CADAssemblyPtr assembly = boost::dynamic_pointer_cast<CADAssembly>( cad->GetCADPart() );
//        CADAssemblyPtr assembly = boost::dynamic_pointer_cast<CADAssembly>( cad );
        assembly->AddChild( newCADPart );
        //std::cout << "Assembly ID = " << assembly->GetID() << std::endl << std::flush;
        newCADPart->SetParent( assembly->GetID() );
    }
    
    // Send node off to xplorer
    ves::open::xml::DataValuePairPtr cadNode( new ves::open::xml::DataValuePair() );
    cadNode->SetDataType( std::string( "XMLOBJECT" ) );
    cadNode->SetData( "New Node", newCADPart );
    ves::open::xml::CommandPtr cadCommand( new ves::open::xml::Command() );
    cadCommand->SetCommandName( std::string( "CAD_ADD_NODE" ) );
    cadCommand->AddDataValuePair( cadNode );
    ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( cadCommand );
}

} // namespace conductor
} // namespace ves
#endif
