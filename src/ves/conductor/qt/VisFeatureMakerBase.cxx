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
#include <ves/conductor/qt/VisFeatureMakerBase.h>
#include <ves/xplorer/command/CommandManager.h>
#include <ves/xplorer/data/DatasetPropertySet.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <iostream>

#include <boost/concept_check.hpp>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
VisFeatureMakerBase::VisFeatureMakerBase()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
VisFeatureMakerBase::VisFeatureMakerBase( const VisFeatureMakerBase& orig )
{
    boost::ignore_unused_variable_warning( orig );
}
////////////////////////////////////////////////////////////////////////////////
VisFeatureMakerBase::~VisFeatureMakerBase()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::Update( unsigned int recordID )
{
    boost::ignore_unused_variable_warning( recordID );
    // Does nothing, but don't want pure virtual f'n so that this class *can*
    // be instantiated alone.
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::UpdateAdvancedSettings( ves::xplorer::data::PropertySet& set )
{
    boost::ignore_unused_variable_warning( set );
    // Does nothing, but don't want pure virtual f'n so that this class *can*
    // be instantiated alone.
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::UpdateBaseInformation( xplorer::data::PropertySet& set )
{
    m_vistabBaseInformation.clear();
    m_commandName.clear();

    ///This is the default. Other dialogs actions will set the command name to the specific value if they are launched
    m_commandName = "VISUALIZATION_SETTINGS";

    ves::open::xml::DataValuePairPtr activeScalar( new ves::open::xml::DataValuePair() );
    activeScalar->SetDataType( "STRING" );
    activeScalar->SetDataName( std::string( "Active Scalar" ) );
    activeScalar->SetDataString( boost::any_cast<std::string >
                                 ( set.GetPropertyAttribute
                                 ( "DataSet_ScalarData", "enumCurrentString" )
                                 ) );
    m_vistabBaseInformation.push_back( activeScalar );

    ves::open::xml::DataValuePairPtr activeVector( new ves::open::xml::DataValuePair() );
    activeVector->SetDataType( "STRING" );
    activeVector->SetDataName( std::string( "Active Vector" ) );
    activeVector->SetDataString( boost::any_cast<std::string >
                                 ( set.GetPropertyAttribute
                                 ( "DataSet_VectorData", "enumCurrentString" )
                                 ) );
    m_vistabBaseInformation.push_back( activeVector );

    ves::open::xml::DataValuePairPtr activeDataset( new ves::open::xml::DataValuePair() );
    activeDataset->SetDataType( "STRING" );
    activeDataset->SetDataName( std::string( "Active Dataset" ) );
    activeDataset->SetDataString( boost::any_cast<std::string >
                                  ( set.GetPropertyAttribute
                                  ( "DataSet", "enumCurrentString" )
                                  ) );
    m_vistabBaseInformation.push_back( activeDataset );

    ves::open::xml::DataValuePairPtr scalarMin( new ves::open::xml::DataValuePair() );
    double minimumValue = boost::any_cast<double>( set.GetPropertyValue( "DataSet_ScalarRange_Min" ) );
    scalarMin->SetData( "Scalar Min", minimumValue );
    m_vistabBaseInformation.push_back( scalarMin );

    ves::open::xml::DataValuePairPtr scalarMax( new ves::open::xml::DataValuePair() );
    double maximumValue = boost::any_cast<double>( set.GetPropertyValue( "DataSet_ScalarRange_Max" ) );
    scalarMax->SetData( "Scalar Max", maximumValue );
    m_vistabBaseInformation.push_back( scalarMax );

    // Load instance of selected DataSet from database and get relevant properties
    // from it.
    xplorer::data::DatasetPropertySet dataset;
    dataset.LoadByKey( "Filename", boost::any_cast<std::string >
                       ( set.GetPropertyAttribute
                       ( "DataSet", "enumCurrentString" )
                       ) );

    //Bounding box display value
    ves::open::xml::DataValuePairPtr bbox( new ves::open::xml::DataValuePair() );
    bbox->SetData( std::string( "Show Bounding Box" ), static_cast < unsigned int > (
                   boost::any_cast<bool>( dataset.GetPropertyValue( "BoundingBox" ) )
                   ) );
    //bbox->SetData( std::string( "Show Bounding Box" ), static_cast < unsigned int > ( 0 ) );
    m_vistabBaseInformation.push_back( bbox );

    //Surface wrap display value
    ves::open::xml::DataValuePairPtr wireMesh( new ves::open::xml::DataValuePair() );
    wireMesh->SetData( std::string( "Show Wire Mesh" ), static_cast < unsigned int > (
                       boost::any_cast<bool>( dataset.GetPropertyValue( "SurfaceWrap" ) )
                       ) );
    //wireMesh->SetData( std::string( "Show Wire Mesh" ), static_cast < unsigned int > ( 0 ) );
    m_vistabBaseInformation.push_back( wireMesh );

    //set scalar bar state
    ves::open::xml::DataValuePairPtr scalarBarDVP( new ves::open::xml::DataValuePair() );
    scalarBarDVP->SetData( "Scalar Bar State", static_cast < unsigned int > (
                           boost::any_cast<bool>( dataset.GetPropertyValue( "ScalarBar" ) )
                           ) );
    //scalarBarDVP->SetData( "Scalar Bar State", static_cast < unsigned int > ( 0 ) );
    m_vistabBaseInformation.push_back( scalarBarDVP );
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureMakerBase::SendUpdatedSettingsToXplorer( ves::open::xml::CommandPtr subDialogCommand,
                                                        xplorer::data::PropertySet& set )
{
    UpdateBaseInformation( set );
    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );

    for( size_t i = 0; i < m_vistabBaseInformation.size(); ++i )
    {
        newCommand->AddDataValuePair( m_vistabBaseInformation.at( i ) );
    }
    if( subDialogCommand )
    {
        ves::open::xml::DataValuePairPtr subDialogSettings( new ves::open::xml::DataValuePair() );
        subDialogSettings->SetData( "Sub-Dialog Settings", subDialogCommand );
        newCommand->AddDataValuePair( subDialogSettings );
    }
    newCommand->SetCommandName( m_commandName );

    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( newCommand );
}
////////////////////////////////////////////////////////////////////////////////
} // namespace conductor
} // namespace ves
