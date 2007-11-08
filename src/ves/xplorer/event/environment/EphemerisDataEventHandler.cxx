/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date: 2007-10-13 22:40:37 -0500 (Sat, 13 Oct 2007) $
 * Version:       $Rev: 9483 $
 * Author:        $Author: mccdo $
 * Id:            $Id: SoundActivateEH.h 9483 2007-10-14 03:40:37Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/event/environment/EphemerisDataEventHandler.h>
#include <ves/xplorer/cfdEnvironmentHandler.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <osgEphemeris/EphemerisModel>
using namespace ves::xplorer::event;
//////////////////////////////////////////////////////
EphemerisDataEventHandler::EphemerisDataEventHandler()
{
    m_activeModel = 0;
}
//////////////////////////////////////////////////////////////////////////////////////////
EphemerisDataEventHandler::EphemerisDataEventHandler(const EphemerisDataEventHandler& ceh)
{
    m_activeModel = ceh.m_activeModel;
}
///////////////////////////////////////////////////////
EphemerisDataEventHandler::~EphemerisDataEventHandler()
{
}
///////////////////////////////////////////////////////////////////////////
EphemerisDataEventHandler&
EphemerisDataEventHandler::operator=(const EphemerisDataEventHandler& rhs)
{
    if(this != &rhs)
    {
        m_activeModel = rhs.m_activeModel;    
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////
void EphemerisDataEventHandler::Execute(ves::open::xml::XMLObject* xmlObject)
{
    try
    {
        ves::open::xml::Command* ephemerisInfo = dynamic_cast<ves::open::xml::Command*>(xmlObject);
        if( ephemerisInfo )
        {
            std::cout<<"Got ephemeris data"<<std::endl;
            ves::open::xml::DataValuePairWeakPtr latitude =
                                 ephemerisInfo->GetDataValuePair( "Latitude" );
            double latitudeData = 0;
            latitude->GetData(latitudeData);
            ves::open::xml::DataValuePairWeakPtr latitudeDir=
                                 ephemerisInfo->GetDataValuePair( "Latitude Direction" );
            std::string northSouth;
            latitudeDir->GetData(northSouth);
            ves::open::xml::DataValuePairWeakPtr longitude =
                                 ephemerisInfo->GetDataValuePair( "Longitude" );
            double longitudeData = 0; 
            ves::open::xml::DataValuePairWeakPtr longitudeDir =
                                 ephemerisInfo->GetDataValuePair( "Longitude Direction" );
            longitudeDir->GetData(longitudeData);
            std::string eastWest;
            longitudeDir->GetData(eastWest);
            osgEphemeris::EphemerisModel* ephemerisModel = 
                  VE_Xplorer::cfdEnvironmentHandler::instance()->GetEphemerisModel(true);
            ephemerisModel->setLatitudeLongitude((eastWest == "West")?-1*latitudeData:latitudeData,
                                                 (northSouth == "South")?-1*longitudeData:longitudeData);
            
        }
    }
    catch(...)
    {
        m_activeModel = 0;
	std::cout<<"Invalid command passed to EphemerisDataEventHandler!!"<<std::endl;
    }
   
}
//////////////////////////////////////////////////////////////////////////////
void EphemerisDataEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase*
                                                     baseObject)
{
    try
    {
        if(baseObject)
	{
            m_activeModel = dynamic_cast<VE_Xplorer::cfdModel*>(baseObject);
	}
	else
	{
            m_activeModel =
		    VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
	}
    }
    catch(...)
    {
        m_activeModel = 0;
    }
}
