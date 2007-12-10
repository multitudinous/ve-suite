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
 * Date modified: $Date: 2007-11-14 15:54:23 -0600 (Wed, 14 Nov 2007) $
 * Version:       $Rev: 9904 $
 * Author:        $Author: dshipton $
 * Id:            $Id: EphemerisAutoDateTimeEventHandler.h 9904 2007-11-14 21:54:23Z dshipton $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <ves/xplorer/event/environment/EphemerisAutoDateTimeEventHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <osgEphemeris/EphemerisModel>

using namespace ves::xplorer::event;
//////////////////////////////////////////////////////////////////////
EphemerisAutoDateTimeEventHandler::EphemerisAutoDateTimeEventHandler()
{
    m_activeModel = 0;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
EphemerisAutoDateTimeEventHandler::EphemerisAutoDateTimeEventHandler( const EphemerisAutoDateTimeEventHandler& ceh )
{
    m_activeModel = ceh.m_activeModel;
}
///////////////////////////////////////////////////////////////////////
EphemerisAutoDateTimeEventHandler::~EphemerisAutoDateTimeEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////////
EphemerisAutoDateTimeEventHandler&
EphemerisAutoDateTimeEventHandler::operator=( const EphemerisAutoDateTimeEventHandler& rhs )
{
    if( this != &rhs )
    {
        m_activeModel = rhs.m_activeModel;
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////////////
void EphemerisAutoDateTimeEventHandler::Execute( ves::open::xml::XMLObject* xmlObject )
{
    try
    {
        ves::open::xml::Command* ephemerisAutoDateTime = dynamic_cast<ves::open::xml::Command*>( xmlObject );
        if( ephemerisAutoDateTime )
        {
            ves::open::xml::DataValuePairWeakPtr autoDateTime =
                ephemerisAutoDateTime->GetDataValuePair( "Auto Date Time" );
            long int onOff = 0;
            autoDateTime->GetData( onOff );

            osgEphemeris::EphemerisModel* ephemerisModel =
                ves::xplorer::EnvironmentHandler::instance()->GetEphemerisModel( true );

            ephemerisModel->setAutoDateTime(( onOff == 0 ) ? false : true );
        }
    }
    catch ( ... )
    {
        m_activeModel = 0;
        std::cout << "Invalid command passed to EphemerisAutoDateTimeEventHandler!!" << std::endl;


    }

}
/////////////////////////////////////////////////////////////////////////////////////////////////
void EphemerisAutoDateTimeEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject )
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


