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
#include <ves/xplorer/event/viz/ContourFeatureMaker.h>

#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/ContourPlanePropertySet.h>
#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <QtGui/QMessageBox>

#include <boost/any.hpp>

using namespace ves::conductor;
using namespace ves;
////////////////////////////////////////////////////////////////////////////////
ContourFeatureMaker::ContourFeatureMaker()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ContourFeatureMaker::ContourFeatureMaker( const ContourFeatureMaker& orig )
    :
    VisFeatureMakerBase( orig )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ContourFeatureMaker::~ContourFeatureMaker()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ContourFeatureMaker::Update( const::std::string& recordUUID )
{
    // For now we won't worry about how to discover an existing plane that needs
    // to be deleted, moved, etc. We will just create a new one
    xplorer::data::ContourPlanePropertySet contourSet;
    contourSet.SetUUID( recordUUID );
    contourSet.LoadFromDatabase();
    AddPlane( static_cast < xplorer::data::PropertySet& > ( contourSet ) );
}
////////////////////////////////////////////////////////////////////////////////
void ContourFeatureMaker::UpdateContourInformation( xplorer::data::PropertySet& set )
{
    m_contourInformation.clear();

    // Plane direction
    ves::open::xml::DataValuePairPtr contourDirection( new ves::open::xml::DataValuePair() );
    contourDirection->SetDataType( "STRING" );
    contourDirection->SetDataName( std::string( "Direction" ) );
    std::string value = boost::any_cast<std::string > ( set.GetPropertyAttribute( "Direction", "enumCurrentString" ) );
    contourDirection->SetDataString( value );
    m_contourInformation.push_back( contourDirection );

    ves::open::xml::DataValuePairPtr selectvecorscalrDisp( new ves::open::xml::DataValuePair() );
    selectvecorscalrDisp->SetDataType( "STRING" );
    selectvecorscalrDisp->SetDataName( "Select Data Mapping" );
    selectvecorscalrDisp->SetDataString( boost::any_cast<std::string > ( set.GetPropertyAttribute( "DataMapping", "enumCurrentString" ) ));
    m_contourInformation.push_back( selectvecorscalrDisp );

    // Mode: Single or Multiple
    ves::open::xml::DataValuePairPtr numberOfPlanes( new ves::open::xml::DataValuePair() );
    numberOfPlanes->SetDataType( "STRING" );
    numberOfPlanes->SetDataName( std::string( "Number of Planes" ) );
    std::string numberOfPlanesOption( "Single" );
    int mode = boost::any_cast<int>( set.GetPropertyValue( "Mode" ) );
    if( mode == 0 )
    {
        numberOfPlanesOption = "Single";
    }
    else if( mode == 1 )
    {
        numberOfPlanesOption = "Multiple";
    }
    numberOfPlanes->SetDataString( numberOfPlanesOption );
    m_contourInformation.push_back( numberOfPlanes );

    // Plane location
    ves::open::xml::DataValuePairPtr planePosition( new ves::open::xml::DataValuePair() );
    planePosition->SetData( "Position", boost::any_cast<double>( set.GetPropertyValue( "PlaneLocation" ) ) );
    m_contourInformation.push_back( planePosition );

    // Use Nearest or Cycle Precomputed
    std::string planeOption;
    if( boost::any_cast<bool>( set.GetPropertyValue( "Mode_UseNearestPrecomputedPlane" ) ) )
    {
        planeOption = "Use Nearest Precomputed Plane";
    }
    else if( boost::any_cast<bool>( set.GetPropertyValue( "Mode_CyclePrecomputedSurfaces" ) ) )
    {
        planeOption = "Cycle Precomputed Surfaces";
    }

    if( !planeOption.empty() )
    {
        ves::open::xml::DataValuePairPtr planeOptionDVP( new ves::open::xml::DataValuePair() );
        planeOptionDVP->SetDataType( "STRING" );
        planeOptionDVP->SetDataName( std::string( "Plane Option" ) );
        planeOptionDVP->SetDataString( planeOption );

        m_contourInformation.push_back( planeOptionDVP );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ContourFeatureMaker::AddPlane( xplorer::data::PropertySet& set )
{
    UpdateContourInformation( set );
    UpdateAdvancedSettings( set );

    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );

    newCommand->SetCommandName( "UPDATE_SCALAR_SETTINGS" );

    for( size_t i = 0; i < m_contourInformation.size(); ++i )
    {
        newCommand->AddDataValuePair( m_contourInformation.at( i ) );
    }

    //The advanced settings command
    ves::open::xml::CommandPtr advancedSettings( new ves::open::xml::Command() );
    advancedSettings->SetCommandName( "ADVANCED_CONTOUR_SETTINGS" );
    for( size_t i = 0; i < m_advancedSettings.size(); ++i )
    {
        advancedSettings->AddDataValuePair( m_advancedSettings.at( i ) );
    }
    std::string typeName = "Advanced Scalar Settings";

    //dvp representing the advanced settings within the contours information
    ves::open::xml::DataValuePairPtr advancedContourSettings( new ves::open::xml::DataValuePair() );
    advancedContourSettings->SetData( typeName, advancedSettings );
    newCommand->AddDataValuePair( advancedContourSettings );

    try
    {
        SendUpdatedSettingsToXplorer( newCommand, set );
    }
    catch ( ... )
    {
        QMessageBox msg;
        msg.setText( "Invalid Parent" );
        msg.setIcon( QMessageBox::Information );
        msg.exec();
    }
}
////////////////////////////////////////////////////////////////////////////////
void ContourFeatureMaker::UpdateAdvancedSettings( xplorer::data::PropertySet& set )
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
    
    if( set.PropertyExists( "Advanced_Opacity" ) )
    {
        ves::open::xml::DataValuePairPtr contourOpacity( new ves::open::xml::DataValuePair() );
        contourOpacity->SetData( "Contour Opacity",
                                boost::any_cast<double>
                                ( set.GetPropertyValue( "Advanced_Opacity" ) ) );
        m_advancedSettings.push_back( contourOpacity );
    }
    
    if( set.PropertyExists( "Advanced_WarpedContourScale" ) )
    {
        ves::open::xml::DataValuePairPtr warpedScale( new ves::open::xml::DataValuePair() );
        warpedScale->SetData( "Warped Contour Scale",
                             boost::any_cast<double>
                             ( set.GetPropertyValue( "Advanced_WarpedContourScale" ) ) );
        m_advancedSettings.push_back( warpedScale );
    }
    
    if( set.PropertyExists( "Advanced_ContourLOD" ) )
    {
        ves::open::xml::DataValuePairPtr LODSetting( new ves::open::xml::DataValuePair() );
        LODSetting->SetData( "Contour LOD",
                            boost::any_cast<double>
                            ( set.GetPropertyValue( "Advanced_ContourLOD" ) ) );
        m_advancedSettings.push_back( LODSetting );
    }
    
    if( set.PropertyExists( "Advanced_ContourType" ) )
    {
        ves::open::xml::DataValuePairPtr contourType( new ves::open::xml::DataValuePair() );
        contourType->SetDataType( "STRING" );
        contourType->SetDataName( std::string( "Type" ) );
        std::string _planeType = boost::any_cast<std::string >
        ( set.GetPropertyAttribute
         ( "Advanced_ContourType", "enumCurrentString" ) );
        contourType->SetDataString( _planeType );
        m_advancedSettings.push_back( contourType );
    }
    
    if( set.PropertyExists( "Advanced_WarpOption" ) )
    {
        ves::open::xml::DataValuePairPtr warpOptionFlag( new ves::open::xml::DataValuePair() );
        warpOptionFlag->SetDataName( "Warp Option" );
        warpOptionFlag->SetDataType( "UNSIGNED INT" );
        if( boost::any_cast<bool>( set.GetPropertyValue( "Advanced_WarpOption" ) ) )
        {
            warpOptionFlag->SetDataValue( static_cast < unsigned int > ( 1 ) );
        }
        else
        {
            warpOptionFlag->SetDataValue( static_cast < unsigned int > ( 0 ) );
        }
        m_advancedSettings.push_back( warpOptionFlag );
    }
    
    if( set.PropertyExists( "UseGPUTools" ) )
    {
        unsigned int checkBox = 
            boost::any_cast<bool>( set.GetPropertyValue( "UseGPUTools" ) );
        ves::open::xml::DataValuePairPtr gpuToolsDVP( new ves::open::xml::DataValuePair() );
        gpuToolsDVP->SetData( "GPU Tools", checkBox );
        m_advancedSettings.push_back( gpuToolsDVP );
    }
    
    //    if( m_datasetSelection->IsEnabled() )
    //    {
    //        ves::open::xml::DataValuePairPtr surfToolsDVP( new ves::open::xml::DataValuePair() );
    //        surfToolsDVP->SetData( "SURF Tools", ConvertUnicode( m_datasetSelection->GetValue().c_str() ) );
    //        _advancedSettings.push_back( surfToolsDVP );
    //    }
    
}
////////////////////////////////////////////////////////////////////////////////
