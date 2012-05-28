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
#include <ves/xplorer/event/viz/StreamlineFeatureMaker.h>

#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/StreamlinePropertySet.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

//#include <QtGui/QMessageBox>

#include <boost/any.hpp>

using namespace ves::conductor;
using namespace ves;
////////////////////////////////////////////////////////////////////////////////
StreamlineFeatureMaker::StreamlineFeatureMaker()
{
    m_commandName = "UPDATE_STREAMLINE_SETTINGS";
}
////////////////////////////////////////////////////////////////////////////////
StreamlineFeatureMaker::StreamlineFeatureMaker( const StreamlineFeatureMaker& orig )
    :
    VisFeatureMakerBase( orig )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
StreamlineFeatureMaker::~StreamlineFeatureMaker()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void StreamlineFeatureMaker::Update( const::std::string& recordUUID )
{
    // For now we won't worry about how to discover an existing plane that needs
    // to be deleted, moved, etc. We will just create a new one
    xplorer::data::PropertySetPtr ptr = xplorer::data::PropertySetPtr( new xplorer::data::StreamlinePropertySet() );
    ptr->SetUUID( recordUUID );
    ptr->LoadFromDatabase();
    //AddPlane( static_cast < xplorer::data::PropertySet& > ( vectorSet ) );
    Execute( ptr );
}
////////////////////////////////////////////////////////////////////////////////
void StreamlineFeatureMaker::UpdateContourInformation( xplorer::data::PropertySet& set )
{
    m_vectorInformation.clear();

    ves::open::xml::DataValuePairPtr streamlineDirection( new ves::open::xml::DataValuePair() );
    streamlineDirection->SetData( std::string( "Cursor Direction" ),
                                  boost::any_cast<std::string >( set.GetPropertyAttribute( "CursorDirection", "enumCurrentString" ) ) );

    m_vectorInformation.push_back( streamlineDirection );

    std::string tempString = boost::any_cast<std::string>( set.GetPropertyAttribute( "CursortType", "enumCurrentString" ) );
    ves::open::xml::DataValuePairPtr cursorSelection = ves::open::xml::MakeDVP( "Cursor Type", tempString );

    m_vectorInformation.push_back( cursorSelection );

    ves::open::xml::DataValuePairPtr integrationDirection( new ves::open::xml::DataValuePair() );
    integrationDirection->SetData( std::string( "Integration Direction" ),
                                   boost::any_cast<std::string >( set.GetPropertyAttribute( "IntegrationDirection", "enumCurrentString" ) ) );

    m_vectorInformation.push_back( integrationDirection );

    ves::open::xml::DataValuePairPtr streamSize( new ves::open::xml::DataValuePair() );
    streamSize->SetData( "Size", boost::any_cast<double>( set.GetPropertyValue( "StreamlineSize" ) ) );

    m_vectorInformation.push_back( streamSize );

    ves::open::xml::DataValuePairPtr nPointsPerPlane( new ves::open::xml::DataValuePair() );
    nPointsPerPlane->SetData( "Number Of Points Per Plane", boost::any_cast<double>( set.GetPropertyValue( "NumberOfPointsPerPlane" ) ) );

    m_vectorInformation.push_back( nPointsPerPlane );
}
////////////////////////////////////////////////////////////////////////////////
void StreamlineFeatureMaker::AddPlane( xplorer::data::PropertySet& set )
{
    UpdateContourInformation( set );
    UpdateAdvancedSettings( set );

    ves::open::xml::CommandPtr displaySeedPoint( new ves::open::xml::Command() );
    displaySeedPoint->SetCommandName( "Display Seed Points" );
    ves::open::xml::DataValuePairPtr seedPointDVP( new ves::open::xml::DataValuePair() );
    seedPointDVP->SetData( "OnOff", static_cast< unsigned int >( 0 ) );
    displaySeedPoint->AddDataValuePair( seedPointDVP );

    ves::open::xml::DataValuePairPtr activeDataset( new ves::open::xml::DataValuePair() );
    std::string datasetname = boost::any_cast<std::string>( set.GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    activeDataset->SetData( "Active Dataset", datasetname );
    displaySeedPoint->AddDataValuePair( activeDataset );
    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( displaySeedPoint );

    ves::open::xml::CommandPtr boundsCommand( new ves::open::xml::Command() );
    boundsCommand->SetCommandName( "Seed Points Bounds" );
    ///6 numbers to define all the mins and maxs, goes like x, y, z
    std::vector<double> seedPointBounds;
    seedPointBounds.push_back( boost::any_cast<double>( set.GetPropertyValue( "SeedPoints_Bounds_XMin" ) ) );
    seedPointBounds.push_back( boost::any_cast<double>( set.GetPropertyValue( "SeedPoints_Bounds_XMax" ) ) );
    seedPointBounds.push_back( boost::any_cast<double>( set.GetPropertyValue( "SeedPoints_Bounds_YMin" ) ) );
    seedPointBounds.push_back( boost::any_cast<double>( set.GetPropertyValue( "SeedPoints_Bounds_YMax" ) ) );
    seedPointBounds.push_back( boost::any_cast<double>( set.GetPropertyValue( "SeedPoints_Bounds_ZMin" ) ) );
    seedPointBounds.push_back( boost::any_cast<double>( set.GetPropertyValue( "SeedPoints_Bounds_ZMax" ) ) );
    ves::open::xml::DataValuePairPtr coordinate( new ves::open::xml::DataValuePair() );
    coordinate->SetData( "Coordinate", "All Bounds" );
    boundsCommand->AddDataValuePair( coordinate );

    ves::open::xml::DataValuePairPtr seedPointBoundsDVP( new ves::open::xml::DataValuePair() );
    seedPointBoundsDVP->SetData( "Bounds", seedPointBounds );
    boundsCommand->AddDataValuePair( seedPointBoundsDVP );

    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( boundsCommand );

    ves::open::xml::CommandPtr dimensionsCommand( new ves::open::xml::Command() );
    dimensionsCommand->SetCommandName( "Seed Points Dimensions" );
    ///Thre numbers for the number of seed points in each direction
    std::vector<long> seedPointDims;
    long tempLongData = boost::any_cast<int>( set.GetPropertyValue( "SeedPoints_NumberOfPointsInX" ) );
    seedPointDims.push_back( tempLongData );
    tempLongData = boost::any_cast<int>( set.GetPropertyValue( "SeedPoints_NumberOfPointsInY" ) );
    seedPointDims.push_back( tempLongData );
    tempLongData = boost::any_cast<int>( set.GetPropertyValue( "SeedPoints_NumberOfPointsInZ" ) );
    seedPointDims.push_back( tempLongData );
    ves::open::xml::DataValuePairPtr dimensions( new ves::open::xml::DataValuePair() );
    dimensions->SetData( "Dimensions", seedPointDims );
    dimensionsCommand->AddDataValuePair( dimensions );

    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( dimensionsCommand );


    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );
    newCommand->SetCommandName( "UPDATE_STREAMLINE_SETTINGS" );

    for( size_t i = 0; i < m_vectorInformation.size(); ++i )
    {
        newCommand->AddDataValuePair( m_vectorInformation.at( i ) );
    }

    //The advanced settings command
    ves::open::xml::CommandPtr advancedSettings( new ves::open::xml::Command() );
    advancedSettings->SetCommandName( "ADVANCED_STREAMLINE_SETTINGS" );
    for( size_t i = 0; i < m_advancedSettings.size(); ++i )
    {
        advancedSettings->AddDataValuePair( m_advancedSettings.at( i ) );
    }
    std::string typeName = "Advanced Streamline Settings";

    //dvp representing the advanced settings within the contours information
    ves::open::xml::DataValuePairPtr advancedContourSettings( new ves::open::xml::DataValuePair() );
    advancedContourSettings->SetData( typeName, advancedSettings );
    newCommand->AddDataValuePair( advancedContourSettings );

    try
    {
        SendUpdatedSettingsToXplorer( newCommand, set );
    }
    catch( ... )
    {
        //QMessageBox msg;
        //msg.setText( "Invalid Parent" );
        //msg.setIcon( QMessageBox::Information );
        //msg.exec();
    }
}
////////////////////////////////////////////////////////////////////////////////
void StreamlineFeatureMaker::UpdateAdvancedSettings( xplorer::data::PropertySet& set )
{
    m_advancedSettings.clear();

    // With a bit of re-thinking here and some normalization of names, we may be
    // able to convert much of the following code to something like:
    //
    // std::vector<std::string> propList = set.GetPropertyList();
    // std::vector<std::sting>::iterator iter;
    // for( iter = propList.begin(); iter != propList.end(); iter++ )
    // {
    //      ...1. Search for substring "Advanced_" in (*iter)
    //      ...2. Extract remainder of name in (*iter)
    //      ...3. Create a DVP, set its name to string from step 2, set
    //              its value to cast value from set.GetPropertyValue( *iter ).
    //              Casting operation could look at property type to determine
    //              appropriate cast -- esp for enums, to use associated enumCurrentString
    //      ...4. Add DVP to _advancedSettings
    // }

    ves::open::xml::DataValuePairPtr propagationTime( new ves::open::xml::DataValuePair() );
    propagationTime->SetData( "Propagation Time", boost::any_cast<double>( set.GetPropertyValue( "Advanced_PropogationTime" ) ) );
    m_advancedSettings.push_back( propagationTime );

    ves::open::xml::DataValuePairPtr integrationStep( new ves::open::xml::DataValuePair() );
    integrationStep->SetData( "Integration Step Size", boost::any_cast<double>( set.GetPropertyValue( "Advanced_IntegrationStepSize" ) ) );
    m_advancedSettings.push_back( integrationStep );

    ves::open::xml::DataValuePairPtr lineDiameter( new ves::open::xml::DataValuePair() );
    lineDiameter->SetData( "Diameter", boost::any_cast<double>( set.GetPropertyValue( "Advanced_Diameter" ) ) );
    m_advancedSettings.push_back( lineDiameter );

    ves::open::xml::DataValuePairPtr sphereArrowParticles( new ves::open::xml::DataValuePair() );
    sphereArrowParticles->SetData( "Sphere/Arrow/Particle Size", boost::any_cast<double>( set.GetPropertyValue( "Advanced_SphereArrowParticleSize" ) ) );
    m_advancedSettings.push_back( sphereArrowParticles );

    ves::open::xml::DataValuePairPtr seedPtFlag( new ves::open::xml::DataValuePair() );
    seedPtFlag->SetDataBool( "Use Last Seed Pt", boost::any_cast<bool>( set.GetPropertyValue( "UseLastSeedPoints" ) ) );
    m_advancedSettings.push_back( seedPtFlag );

    ves::open::xml::DataValuePairPtr streamArrow( new ves::open::xml::DataValuePair() );
    streamArrow->SetDataBool( "Use Stream Arrows", boost::any_cast<bool>( set.GetPropertyValue( "UseStreamArrows" ) ) );
    m_advancedSettings.push_back( streamArrow );

    ves::open::xml::DataValuePairPtr streamRibbon( new ves::open::xml::DataValuePair() );
    streamRibbon->SetDataBool( "Use Stream Ribbons", boost::any_cast<bool>( set.GetPropertyValue( "UseStreamRibbons" ) ) );
    m_advancedSettings.push_back( streamRibbon );

    ves::open::xml::DataValuePairPtr gpuToolsDVP( new ves::open::xml::DataValuePair() );
    gpuToolsDVP->SetDataBool( "GPU Tools", boost::any_cast<bool>( set.GetPropertyValue( "UseGPUTools" ) ) );
    m_advancedSettings.push_back( gpuToolsDVP );
}
////////////////////////////////////////////////////////////////////////////////
