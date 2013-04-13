/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/communication/CommunicationHandler.h>

#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>

#include <ves/xplorer/scenegraph/util/MaterialInitializer.h>
#include <ves/xplorer/scenegraph/util/FindChildWithNameVisitor.h>
#include <ves/xplorer/scenegraph/util/ToggleNodesVisitor.h>

#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>
#include <ves/xplorer/scenegraph/FindParentWithNameVisitor.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/TextTexture.h>
#include <ves/xplorer/scenegraph/GroupedTextTextures.h>
#include <ves/xplorer/scenegraph/HeadPositionCallback.h>
#include <ves/xplorer/scenegraph/HeadsUpDisplay.h>
#include <ves/xplorer/scenegraph/Geode.h>


#include <ves/xplorer/environment/TextTextureCallback.h>

#include <ves/xplorer/EnvironmentHandler.h>

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkPolyDataMapper.h>
#include <vtkPolyData.h>
#include <vtkCellDataToPointData.h>
#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkCellArray.h>
#include <vtkDoubleArray.h>
#include <vtkPointData.h>

#include <osgUtil/LineSegmentIntersector>
#include <osg/Depth>

#include <sstream>
#include <iostream>
#include <fstream>
#include <algorithm>

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/find.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <boost/algorithm/string.hpp>
#include <boost/bind.hpp>

#include <vpr/vpr.h>
#include <vpr/IO/Socket/SocketDatagram.h>
#include <vpr/IO/Socket/InetAddr.h>
#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/IO/TimeoutException.h>
#include <vpr/Util/Interval.h>

#include "ImageManipulationPluginGP.h"

#include <Poco/SharedPtr.h>
#include <Poco/Tuple.h>
#include <Poco/DateTimeFormatter.h>
#include <Poco/DateTimeParser.h>

#include <Poco/Data/SessionFactory.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/SQLite/Connector.h>


#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/device/KeyboardMouse.h>

using namespace Poco::Data;
using namespace ves::xplorer::scenegraph;
using namespace warrantytool;
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
ImageManipulationPluginGP::ImageManipulationPluginGP()
    :
    PluginBase(),
    mAddingParts( false ),
    m_keyboard( 0 ),
    m_groupedTextTextures( 0 ),
    m_cadRootNode( 0 ),
    m_hasPromiseDate( false ),
    m_mouseSelection( false ),
    m_currentStatement( 0 ),
m_sampleThread( 0 ),
m_computerName( "" ),
m_computerPort( "" ),
m_runSampleThread( false )

{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "ImageManipulationPlugin";
    m_dbFilename = "sample.db";
}
////////////////////////////////////////////////////////////////////////////////
ImageManipulationPluginGP::~ImageManipulationPluginGP()
{
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
    
    m_keyboard = 
        dynamic_cast< ves::xplorer::device::KeyboardMouse* >( mDevice );

    {
        m_textTrans = new ves::xplorer::scenegraph::DCS();
        std::vector< double > data;
        data.push_back( -1.43 );
        data.push_back(   6.5 );
        data.push_back( -0.70 );
        m_textTrans->SetTranslationArray( data );
        
        mDCS->addChild( m_textTrans.get() );
    }

    CONNECTSIGNAL_2( "%ConnectToSensorServer",
                    void( std::string const&, std::string const& ),
                    &ImageManipulationPluginGP::SetComputerInfo,
                    m_connections, normal_Priority );

    CreateSensorGrid();
    
    LoadModels();
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::PreFrameUpdate()
{
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::SetComputerInfo( std::string const& ip, std::string const& port )
{
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::StripCharacters( std::string& data, const std::string& character )
{
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::RenderTextualDisplay( bool onOff )
{
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::CreateDB()
{
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::CreateTextTextures()
{
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::CreateDBQuery( ves::open::xml::DataValuePairPtr dvp )
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::RemoveSelfFromSG()
{

    
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::StripDollarCharacters( std::string& data )
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::ReplaceSpacesCharacters( std::string& data )
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::ParseDataBase( const std::string& csvFilename )
{

}
////////////////////////////////////////////////////////////////////////////////
bool ImageManipulationPluginGP::FindPartNodeAndHighlightNode()
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::GetPartNumberFromNodeName( std::string& nodeName )
{
  
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::PickTextTextures()
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::ClearDatabaseUserTables()
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::HighlightPartsInJoinedTabled( const std::string& queryString )
{
 
}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::QueryTableAndHighlightParts( 
    const std::string& tableName, osg::Vec3& glowColor )
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::QueryInnerJoinAndHighlightParts( const std::string& queryString )
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::QueryUserDefinedAndHighlightParts( const std::string& queryString )
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::SaveCurrentQuery( const std::string& filename )
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::SimulatorCaptureThread()
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::SetPositionData( std::vector< double >& temp )
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::GetPositionData( std::vector< double >& temp )
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::CreateSensorGrid()
{

}
////////////////////////////////////////////////////////////////////////////////
void ImageManipulationPluginGP::LoadModels()
{
}
////////////////////////////////////////////////////////////////////////////////
