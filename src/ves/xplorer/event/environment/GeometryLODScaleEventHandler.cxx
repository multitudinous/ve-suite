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
#include <ves/xplorer/event/environment/GeometryLODScaleEventHandler.h>

#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>

#include <osgEphemeris/EphemerisModel.h>

using namespace ves::xplorer::event;

////////////////////////////////////////////////////////////////////////////////
GeometryLODScaleEventHandler::GeometryLODScaleEventHandler()
{
    m_activeModel = 0;
    CONNECTSIGNALS_1( "%GeometryLODScale",
                      void ( const double lodScale ),
                      &GeometryLODScaleEventHandler::UpdateLODScale,
                      m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
GeometryLODScaleEventHandler::GeometryLODScaleEventHandler( const GeometryLODScaleEventHandler& ceh )
    :
    ves::xplorer::event::EventHandler( ceh )
{
    m_activeModel = ceh.m_activeModel;
}
////////////////////////////////////////////////////////////////////////////////
GeometryLODScaleEventHandler::~GeometryLODScaleEventHandler()
{}
////////////////////////////////////////////////////////////////////////////////
GeometryLODScaleEventHandler&
GeometryLODScaleEventHandler::operator=( const GeometryLODScaleEventHandler& rhs )
{
    if( this != &rhs )
    {
        m_activeModel = rhs.m_activeModel;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void GeometryLODScaleEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    try
    {
        ves::open::xml::CommandPtr geometryLODScaleCmd = boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject );
        if( geometryLODScaleCmd )
        {
            ves::open::xml::DataValuePairPtr scaleValue =
                geometryLODScaleCmd->GetDataValuePair( "Geometry LOD Scale" );
            long alpha = 0;
            scaleValue->GetData( alpha );
            UpdateLODScale( alpha );
        }
    }
    catch( ... )
    {
        m_activeModel = 0;
        std::cout << "Invalid command passed to EphemerisAutoDateTimeEventHandler!!" << std::endl;


    }

}
////////////////////////////////////////////////////////////////////////////////
void GeometryLODScaleEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject )
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
    catch( ... )
    {
        m_activeModel = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GeometryLODScaleEventHandler::UpdateLODScale( const double lodScale )
{
    double scale = .0001 * exp( lodScale * .18420680745 );
    ves::xplorer::EnvironmentHandler::instance()->SetGlobalLODScale( scale );
}
////////////////////////////////////////////////////////////////////////////////
