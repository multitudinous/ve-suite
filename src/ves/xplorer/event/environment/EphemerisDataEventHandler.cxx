/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/xplorer/event/environment/EphemerisDataEventHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <osgEphemeris/EphemerisModel.h>
#include <osgEphemeris/DateTime.h>

using namespace ves::xplorer::event;
//////////////////////////////////////////////////////
EphemerisDataEventHandler::EphemerisDataEventHandler()
{
    m_activeModel = 0;
}
//////////////////////////////////////////////////////////////////////////////////////////
EphemerisDataEventHandler::EphemerisDataEventHandler( const EphemerisDataEventHandler& ceh )
{
    m_activeModel = ceh.m_activeModel;
}
///////////////////////////////////////////////////////
EphemerisDataEventHandler::~EphemerisDataEventHandler()
{}
///////////////////////////////////////////////////////////////////////////
EphemerisDataEventHandler&
EphemerisDataEventHandler::operator=( const EphemerisDataEventHandler& rhs )
{
    if( this != &rhs )
    {
        m_activeModel = rhs.m_activeModel;
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////
void EphemerisDataEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    try
    {
        ves::open::xml::CommandPtr ephemerisInfo = boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject );
        if( ephemerisInfo )
        {
            //std::cout<<"Got ephemeris data"<<std::endl;
            ves::open::xml::DataValuePairPtr latitude =
                ephemerisInfo->GetDataValuePair( "Latitude" );
            double latitudeData = 0;
            latitude->GetData( latitudeData );
            ves::open::xml::DataValuePairPtr latitudeDir =
                ephemerisInfo->GetDataValuePair( "Latitude Direction" );
            std::string northSouth;
            latitudeDir->GetData( northSouth );
            ves::open::xml::DataValuePairPtr longitude =
                ephemerisInfo->GetDataValuePair( "Longitude" );
            double longitudeData = 0;
            ves::open::xml::DataValuePairPtr longitudeDir =
                ephemerisInfo->GetDataValuePair( "Longitude Direction" );
            longitude->GetData( longitudeData );
            std::string eastWest;
            longitudeDir->GetData( eastWest );
            //std::cout<<"Latitude: "<<latitudeData<<std::endl;
            //std::cout<<"Longitude: "<<longitudeData<<std::endl;
            osgEphemeris::EphemerisModel* ephemerisModel =
                ves::xplorer::EnvironmentHandler::instance()->GetEphemerisModel( true );
            ephemerisModel->setLatitudeLongitude(( northSouth == "South" ) ? -1*latitudeData : latitudeData,
                                                 ( eastWest == "West" ) ? -1*longitudeData : longitudeData );
            std::vector<long> dateTimeInfo;
            ves::open::xml::DataValuePairPtr dateTimeData =
                ephemerisInfo->GetDataValuePair( "Date and Time Info" );
            dateTimeData->GetData( dateTimeInfo );
            //std::cout<<"Date: "<<dateTimeInfo[0]<<" "<<dateTimeInfo[1]<<" "
            //                   <<dateTimeInfo[2]<<std::endl;
            //std::cout<<"Time: "<<dateTimeInfo[3]<<":"<<dateTimeInfo[4]<<std::endl;

            ephemerisModel->setDateTime( osgEphemeris::DateTime( dateTimeInfo[0],
                                                                 dateTimeInfo[1],
                                                                 dateTimeInfo[2],
                                                                 dateTimeInfo[3],
                                                                 dateTimeInfo[4] ) );
        }
    }
    catch ( ... )
    {
        m_activeModel = 0;
        std::cout << "Invalid command passed to EphemerisDataEventHandler!!" << std::endl;
    }

}
//////////////////////////////////////////////////////////////////////////////
void EphemerisDataEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase*
                                                     baseObject )
{
    try
    {
        if( baseObject )
        {
            m_activeModel = dynamic_cast<ves::xplorer::Model*>( baseObject );
        }
        else
        {
            m_activeModel =
                ves::xplorer::ModelHandler::instance()->GetActiveModel();
        }
    }
    catch ( ... )
    {
        m_activeModel = 0;
    }
}
