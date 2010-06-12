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
#include <ves/conductor/qt/ContourFeatureMaker.h>

#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/ContourPlanePropertySet.h>
#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <QtGui/QMessageBox>

#include <boost/any.hpp>

using namespace ves::conductor;
using namespace ves;

ContourFeatureMaker::ContourFeatureMaker( )
{
}
////////////////////////////////////////////////////////////////////////////////

ContourFeatureMaker::ContourFeatureMaker( const ContourFeatureMaker& orig )
{
}
////////////////////////////////////////////////////////////////////////////////

ContourFeatureMaker::~ContourFeatureMaker( )
{
}
////////////////////////////////////////////////////////////////////////////////

void ContourFeatureMaker::update( unsigned int recordID )
{
    // For now we won't worry about how to discover an existing plane that needs
    // to be deleted, moved, etc. We will just create a new one
    xplorer::data::ContourPlanePropertySet contourSet;
    contourSet.SetRecordID( recordID );
    contourSet.LoadFromDatabase(  );
    _addPlane( static_cast < xplorer::data::PropertySet& > ( contourSet ) );
}
////////////////////////////////////////////////////////////////////////////////

void ContourFeatureMaker::_updateContourInformation( xplorer::data::PropertySet& set )
{
    _contourInformation.clear( );

    // Plane direction
    ves::open::xml::DataValuePairPtr contourDirection( new ves::open::xml::DataValuePair( ) );
    contourDirection->SetDataType( "STRING" );
    contourDirection->SetDataName( std::string( "Direction" ) );
    std::string value = boost::any_cast<std::string > ( set.GetPropertyAttribute( "Direction", "enumCurrentString" ) );
    contourDirection->SetDataString( value );
    _contourInformation.push_back( contourDirection );

    ves::open::xml::DataValuePairPtr selectvecorscalrDisp( new ves::open::xml::DataValuePair() );
    selectvecorscalrDisp->SetDataType( "STRING" );
    selectvecorscalrDisp->SetDataName( "Select Data Mapping" );
    selectvecorscalrDisp->SetDataString( boost::any_cast<std::string > ( set.GetPropertyAttribute( "DataMapping", "enumCurrentString" ) ));
    _contourInformation.push_back( selectvecorscalrDisp );

    // Mode: Single or Multiple
    ves::open::xml::DataValuePairPtr numberOfPlanes( new ves::open::xml::DataValuePair( ) );
    numberOfPlanes->SetDataType( "STRING" );
    numberOfPlanes->SetDataName( std::string( "Number of Planes" ) );
    std::string _numberOfPlanesOption( "Single" );
    int mode = boost::any_cast<int>( set.GetPropertyValue( "Mode" ) );
    if( mode == 0 )
    {
        _numberOfPlanesOption = "Single";
    }
    else if( mode == 1 )
    {
        _numberOfPlanesOption = "Multiple";
    }
    numberOfPlanes->SetDataString( _numberOfPlanesOption );
    _contourInformation.push_back( numberOfPlanes );

    // Plane location
    ves::open::xml::DataValuePairPtr planePosition( new ves::open::xml::DataValuePair( ) );
    planePosition->SetData( "Position", boost::any_cast<double>( set.GetPropertyValue( "PlaneLocation" ) ) );
    _contourInformation.push_back( planePosition );

    // Use Nearest or Cycle Precomputed
    std::string _planeOption( "" );
    if( boost::any_cast<bool>( set.GetPropertyValue( "Mode_UseNearestPrecomputedPlane" ) ) )
    {
        _planeOption = "Use Nearest Precomputed Plane";
    }
    else if( boost::any_cast<bool>( set.GetPropertyValue( "Mode_CyclePrecomputedSurfaces" ) ) )
    {
        _planeOption = "Cycle Precomputed Surfaces";
    }

    if( !_planeOption.empty( ) )
    {
        ves::open::xml::DataValuePairPtr planeOption( new ves::open::xml::DataValuePair( ) );
        planeOption->SetDataType( "STRING" );
        planeOption->SetDataName( std::string( "Plane Option" ) );
        planeOption->SetDataString( _planeOption );

        _contourInformation.push_back( planeOption );
    }
}
////////////////////////////////////////////////////////////////////////////////

void ContourFeatureMaker::_addPlane( xplorer::data::PropertySet& set )
{
    _updateContourInformation( set );
    _updateAdvancedSettings( set );

    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command( ) );

    newCommand->SetCommandName( "UPDATE_SCALAR_SETTINGS" );

    for ( size_t i = 0; i < _contourInformation.size( ); i++ )
    {
        newCommand->AddDataValuePair( _contourInformation.at( i ) );
    }

    //The advanced settings command
    ves::open::xml::CommandPtr advancedSettings( new ves::open::xml::Command( ) );
    advancedSettings->SetCommandName( "ADVANCED_CONTOUR_SETTINGS" );
    for ( size_t i = 0; i < _advancedSettings.size( ); i++ )
    {
        advancedSettings->AddDataValuePair( _advancedSettings.at( i ) );
    }
    std::string typeName = "Advanced Scalar Settings";

    //dvp representing the advanced settings within the contours information
    ves::open::xml::DataValuePairPtr advancedContourSettings( new ves::open::xml::DataValuePair( ) );
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
        msg.exec( );
    }
}
