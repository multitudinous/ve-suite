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

#ifndef WARRANTY_TOOL_GP_H
#define WARRANTY_TOOL_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

#include <Poco/Tuple.h>

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
class VE_USER_PLUGIN_EXPORTS WarrantyToolGP :
    public ves::xplorer::plugin::PluginBase
{
public:
    WarrantyToolGP();
    virtual ~WarrantyToolGP();

    virtual void InitializeNode( osg::Group* veworldDCS );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );
    virtual void RemoveSelfFromSG();

protected:

private:
    void CreateDB();
    void CreateTextTextures();
    ///Strip characters from datafile
    void StripCharacters( std::string& data, const std::string& character );
    ///PArse the csv file
    void ParseDataFile( const std::string& csvFilename );
    ///Parse the db file selected by the user
    void ParseDataBase( const std::string& csvFilename );
    void RenderTextualDisplay( bool onOff );
    void CreateDBQuery( ves::open::xml::DataValuePairPtr dvp );
    void StripDollarCharacters( std::string& data );
    void ReplaceSpacesCharacters( std::string& data );

    std::vector< std::string > mPartNumberList;
    ///PArt numbers loaded from the csv files
    std::vector< std::string > mLoadedPartNumbers;
    ///Description of part numbers loaded from csv files
    std::vector< std::string > mPartNumberDescriptions;
    
    std::string m_lastPartNumber;
    ves::xplorer::scenegraph::CADEntity* cadEntity;
    bool mAddingParts;
    std::map< std::string, std::vector< std::pair< std::string, std::string > > > m_dataMap;
    osg::ref_ptr< ves::xplorer::scenegraph::TextTexture > mModelText;
    ves::xplorer::device::KeyboardMouse* m_keyboard;
    osg::ref_ptr< ves::xplorer::scenegraph::GroupedTextTextures > m_groupedTextTextures;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_textTrans;

    typedef Poco::Tuple< std::string, std::string, int, double, double, double, std::string > Part;
	typedef std::vector<Part> Assembly;
    std::vector< std::string > m_assemblyPartNumbers;
    // insert some rows
	Assembly m_selectedAssembly;
    ///Command being processed
    ves::open::xml::CommandPtr m_currentCommand;
    ///db filename
    std::string m_dbFilename;
    ///Root of the CAD models
    ves::xplorer::scenegraph::DCS* m_cadRootNode;
    
    int m_promiseDateColumn;
    
    size_t m_partNumberColumn;
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( WarrantyToolGP )

} //end warrantytool

#endif //WARRANTY_TOOL_GP_H
