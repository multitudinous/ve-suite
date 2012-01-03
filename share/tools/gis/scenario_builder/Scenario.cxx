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
#include "Scenario.h"

#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/DataValuePair.h>

namespace iaf
{
namespace scenario
{
////////////////////////////////////////////////////////////////////////////////
Scenario::Scenario()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Scenario::~Scenario()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Scenario::Scenario( const Scenario& input )
{
    m_tillage = input.m_tillage;
    m_rotation = input.m_rotation;
    m_location = input.m_location;
    m_climateModel = input.m_climateModel;
    m_removalRate = input.m_removalRate;
}
////////////////////////////////////////////////////////////////////////////////
Scenario& Scenario::operator=( const Scenario& input )
{
    if( this != &input )
    {
        m_tillage = input.m_tillage;
        m_rotation = input.m_rotation;
        m_location = input.m_location;
        m_climateModel = input.m_climateModel;
        m_removalRate = input.m_removalRate;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void Scenario::SetTillage( std::string const& tillage )
{
    m_tillage = tillage;
}
////////////////////////////////////////////////////////////////////////////////
std::string& Scenario::GetTillage()
{
    return m_tillage;
}
////////////////////////////////////////////////////////////////////////////////
void Scenario::SetRotation( std::string const& rotation )
{
    m_rotation = rotation;
}
////////////////////////////////////////////////////////////////////////////////
std::string& Scenario::GetRotation()
{
    return m_rotation;
}
////////////////////////////////////////////////////////////////////////////////
void Scenario::SetRemovalRate( std::string const& rate )
{
    m_removalRate = rate;
}
////////////////////////////////////////////////////////////////////////////////
std::string& Scenario::GetRemovalRate()
{
    return m_removalRate;
}
////////////////////////////////////////////////////////////////////////////////
void Scenario::SetClimateModel( std::string const& model )
{
    m_climateModel = model;
}
////////////////////////////////////////////////////////////////////////////////
std::string& Scenario::GetClimateModel()
{
    return m_climateModel;
}
////////////////////////////////////////////////////////////////////////////////
void Scenario::SetLocation( std::string const& location )
{
    m_location = location;
}
////////////////////////////////////////////////////////////////////////////////
std::string& Scenario::GetLocation()
{
    return m_location;
}
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::CommandPtr Scenario::GetXMLScenario()
{
    ves::open::xml::CommandPtr scenarioCommand( new ves::open::xml::Command() );
    scenarioCommand->SetCommandName( "iaf scenario" );
    
    {
        ///Set the climate model
        ///I am not sure of the possible model choices here
        ves::open::xml::DataValuePairPtr climateDVP( new ves::open::xml::DataValuePair() );
        climateDVP->SetData( "climateModel", m_climateModel );
        scenarioCommand->AddDataValuePair( climateDVP );
    }
    
    {
        ///Set the GIS location
        ves::open::xml::DataValuePairPtr gisDVP( new ves::open::xml::DataValuePair() );
        gisDVP->SetData( "location", m_location );
        scenarioCommand->AddDataValuePair( gisDVP );
    }
    
    {
        ///Set the management tools
        ves::open::xml::CommandPtr managementCommand( new ves::open::xml::Command() );
        managementCommand->SetCommandName( "managementInputs" );
        
        ///Rotation choices
        ///CG, SB, RCSB, RCHSB
        ///This shoudl actually be an enum OR'ed together
        ves::open::xml::DataValuePairPtr rotationDVP( new ves::open::xml::DataValuePair() );
        rotationDVP->SetData( "rotation", m_rotation );
        managementCommand->AddDataValuePair( rotationDVP );
        
        ///Tillage choices
        ///CH+FC, NT, MP+FC
        ///This shoudl actually be an enum OR'ed together
        ves::open::xml::DataValuePairPtr tillageDVP( new ves::open::xml::DataValuePair() );
        tillageDVP->SetData( "tillage", m_tillage );
        managementCommand->AddDataValuePair( tillageDVP );
        
        ///Removal rate
        ///25-35, 40-50, 55-65, 70-80
        ///Maybe make this one an double?
        ves::open::xml::DataValuePairPtr removalRateDVP( new ves::open::xml::DataValuePair() );
        removalRateDVP->SetData( "removalRate", m_removalRate );
        managementCommand->AddDataValuePair( removalRateDVP );
        
        ///Now add back into the scenarioCommand
        ves::open::xml::DataValuePairPtr managementDVP( new ves::open::xml::DataValuePair() );
        managementDVP->SetData( "management", managementCommand );
        scenarioCommand->AddDataValuePair( managementDVP );
    }
    
    
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( 
                                                                            scenarioCommand, "Command" ) );
    
    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    std::string filename( "test.vec" );
    netowrkWriter.WriteXMLDocument( nodes, filename, "Command" );
    
    return scenarioCommand;
}
////////////////////////////////////////////////////////////////////////////////
}
}
