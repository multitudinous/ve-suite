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
#include <ves/xplorer/event/viz/IsosurfaceFeatureMaker.h>

#include <propertystore/PropertySet.h>
#include <ves/xplorer/data/IsosurfacePropertySet.h>
#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <boost/any.hpp>

using namespace ves::conductor;
using namespace ves;
////////////////////////////////////////////////////////////////////////////////
IsosurfaceFeatureMaker::IsosurfaceFeatureMaker()
{
    m_commandName = "UPDATE_ISOSURFACE_SETTINGS";
}
////////////////////////////////////////////////////////////////////////////////
IsosurfaceFeatureMaker::IsosurfaceFeatureMaker( const IsosurfaceFeatureMaker& orig )
    :
    VisFeatureMakerBase( orig )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
IsosurfaceFeatureMaker::~IsosurfaceFeatureMaker()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void IsosurfaceFeatureMaker::Update( const::std::string& recordUUID )
{
    // For now we won't worry about how to discover an existing plane that needs
    // to be deleted, moved, etc. We will just create a new one
    propertystore::PropertySetPtr ptr = propertystore::PropertySetPtr( new xplorer::data::IsosurfacePropertySet() );
    ptr->SetUUID( recordUUID );
    ptr->Load();
    //AddPlane( static_cast < propertystore::PropertySet& > ( vectorSet ) );
    Execute( ptr );
}
////////////////////////////////////////////////////////////////////////////////
void IsosurfaceFeatureMaker::UpdateContourInformation( propertystore::PropertySet& set )
{
    m_contourInformation.clear();

    ves::open::xml::DataValuePairPtr isosurfaceValue( new ves::open::xml::DataValuePair() );
    isosurfaceValue->SetData( "Iso-Surface Value", boost::any_cast<double>
                              ( set.GetPropertyValue( "IsosurfaceValue" ) ) );
    m_contourInformation.push_back( isosurfaceValue );

    ves::open::xml::DataValuePairPtr colorByScalar( new ves::open::xml::DataValuePair() );
    colorByScalar->SetData( "Color By Scalar", boost::any_cast<std::string >
                            ( set.GetPropertyValue( "ColorByScalar" ) ) );
    m_contourInformation.push_back( colorByScalar );

    ves::open::xml::DataValuePairPtr minValue( new ves::open::xml::DataValuePair() );
    minValue->SetData( "Minimum Scalar Value", boost::any_cast<double>
                       ( set.GetPropertyValue( "ColorByScalar_ScalarRange_Min" ) ) );
    m_contourInformation.push_back( minValue );

    ves::open::xml::DataValuePairPtr maxValue( new ves::open::xml::DataValuePair() );
    maxValue->SetData( "Maximum Scalar Value", boost::any_cast<double>
                       ( set.GetPropertyValue( "ColorByScalar_ScalarRange_Max" ) ) );
    m_contourInformation.push_back( maxValue );

    ves::open::xml::DataValuePairPtr nearestPrecomputed( new ves::open::xml::DataValuePair() );
    nearestPrecomputed->SetData( "Use Nearest Precomputed", static_cast<unsigned int>( 0 ) );
    m_contourInformation.push_back( nearestPrecomputed );

    ves::open::xml::DataValuePairPtr gpuTools( new ves::open::xml::DataValuePair() );
    gpuTools->SetDataBool( "Use GPU Tools", boost::any_cast<bool>
                           ( set.GetPropertyValue( "UseGPUTools" ) ) );
    m_contourInformation.push_back( gpuTools );
}
////////////////////////////////////////////////////////////////////////////////
void IsosurfaceFeatureMaker::AddPlane( propertystore::PropertySet& set )
{
    UpdateContourInformation( set );

    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );

    newCommand->SetCommandName( "UPDATE_ISOSURFACE_SETTINGS" );

    for( size_t i = 0; i < m_contourInformation.size(); ++i )
    {
        newCommand->AddDataValuePair( m_contourInformation.at( i ) );
    }

    try
    {
        SendUpdatedSettingsToXplorer( newCommand, set );
    }
    catch( ... )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void IsosurfaceFeatureMaker::UpdateAdvancedSettings( propertystore::PropertySet& set )
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
    //              appropriate cast.
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
                                 ( set.GetPropertyValue( "Advanced_ContourType" ) );
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
            warpOptionFlag->SetDataValue( static_cast < unsigned int >( 1 ) );
        }
        else
        {
            warpOptionFlag->SetDataValue( static_cast < unsigned int >( 0 ) );
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
