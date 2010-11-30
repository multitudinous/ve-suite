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

#ifndef WARRANTY_TOOL_GP_H
#define WARRANTY_TOOL_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

#include <Poco/Tuple.h>
#include <Poco/Data/Statement.h>
#include <Poco/Data/RecordSet.h>

#include <map>
#include <vector>
#include <utility>
#include <string>

namespace ves
{
namespace xplorer
{
namespace device
{
    class KeyboardMouse;
}
namespace scenegraph
{
    class CADEntity;
    class TextTexture;
    class GroupedTextTextures;
}
}
}
namespace warrantytool
{
class VE_USER_PLUGIN_EXPORTS DynamicVehicleSimToolGP :
    public ves::xplorer::plugin::PluginBase
{
public:
    DynamicVehicleSimToolGP();
    virtual ~DynamicVehicleSimToolGP();

    virtual void InitializeNode( osg::Group* veworldDCS );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );
    virtual void RemoveSelfFromSG();

protected:

private:
    ///Create the db for the tool to grab dat from
    void CreateDB();
    ///Create the list of textures
    void CreateTextTextures();
    ///Strip characters from datafile
    void StripCharacters( std::string& data, const std::string& character );
    ///PArse the csv file
    //void ParseDataFile( const std::string& csvFilename );
    ///Parse the db file selected by the user
    void ParseDataBase( const std::string& csvFilename );
    ///Render the displays
    void RenderTextualDisplay( bool onOff );
    ///Create a db query from the ui
    void CreateDBQuery( ves::open::xml::DataValuePairPtr dvp );
    ///Strip dollar signs from a string
    void StripDollarCharacters( std::string& data );
    ///Replace spaces in a string with under scores
    void ReplaceSpacesCharacters( std::string& data );
    ///Find a list of nodes with part number names
    bool FindPartNodeAndHighlightNode();
    ///Get the part number from the node name
    void GetPartNumberFromNodeName( std::string& nodeName );
    ///Change text textures
    void PickTextTextures();
    ///Clear the db of all the user defined tables
    void ClearDatabaseUserTables();
    ///Query specifically for the join command
    void QueryTableAndHighlightParts( const std::string& tableName, 
                                     osg::Vec3& glowColor );
    ///Basic user defined custom query
    void QueryUserDefinedAndHighlightParts( const std::string& queryString );
    ///Query and highlight for a join query 
    void QueryInnerJoinAndHighlightParts( const std::string& queryString );
    ///Query 3 sets of data to create 3 highlighted group of parts
    void HighlightPartsInJoinedTabled( const std::string& queryString );
    ///Write out the current query to a file
    void SaveCurrentQuery( const std::string& filename );

    std::vector< std::string > mPartNumberList;
    ///PArt numbers loaded from the csv files
    std::vector< std::string > mLoadedPartNumbers;
    ///Description of part numbers loaded from csv files
    std::vector< std::string > mPartNumberDescriptions;
    
    std::string m_lastPartNumber;
    ves::xplorer::scenegraph::CADEntity* cadEntity;
    ///Adding parts
    bool mAddingParts;
    std::map< std::string, std::vector< std::pair< std::string, std::string > > > m_dataMap;
    osg::ref_ptr< ves::xplorer::scenegraph::TextTexture > mModelText;
    ves::xplorer::device::KeyboardMouse* m_keyboard;
    osg::ref_ptr< ves::xplorer::scenegraph::GroupedTextTextures > m_groupedTextTextures;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_textTrans;

    typedef Poco::Tuple< std::string, std::string, int, double, double, double, std::string > Part;
	typedef std::vector<Part> Assembly;
    ///All of the currently highlighted parts
    std::vector< std::string > m_assemblyPartNumbers;
    ///All of the currently highlighted joined parts
    std::vector< std::string > m_joinedPartNumbers;
    ///insert some rows
	Assembly m_selectedAssembly;
    ///Command being processed
    ves::open::xml::CommandPtr m_currentCommand;
    ///db filename
    std::string m_dbFilename;
    ///Root of the CAD models
    ves::xplorer::scenegraph::DCS* m_cadRootNode;
    ///Column number for the promise date
    size_t m_promiseDateColumn;
    ///Determine if we have a promise date column
    bool m_hasPromiseDate;
    //Column number for the part numbers
    size_t m_partNumberColumn;
    ///Vector map to be used to create the DB
    std::map< int, std::vector< std::string > > m_csvDataMap;
    ///Control mouse selection
    bool m_mouseSelection;
    ///Container for the two currently active table names
    std::pair< std::string, std::string > m_tableNames;
    ///The current select statement
    Poco::Data::Statement* m_currentStatement;
    //Poco::Data::RecordSet m_currentStatement;
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( DynamicVehicleSimToolGP )

} //end warrantytool

#endif //WARRANTY_TOOL_GP_H
