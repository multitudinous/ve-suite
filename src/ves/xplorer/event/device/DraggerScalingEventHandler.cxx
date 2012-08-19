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

#include <ves/xplorer/event/device/DraggerScalingEventHandler.h>

#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;

////////////////////////////////////////////////////////////////////////////////
DraggerScalingEventHandler::DraggerScalingEventHandler()
{
    m_activeModel = 0;

    CONNECTSIGNALS_2( "%DraggerScaling",
                      void ( bool const & enable, double const & draggerScale ),
                      &DraggerScalingEventHandler::UpdateDraggerScaling,
                      m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
DraggerScalingEventHandler::DraggerScalingEventHandler( const DraggerScalingEventHandler& ceh )
    :
    EventHandler( ceh )
{
    m_activeModel = ceh.m_activeModel;
}
////////////////////////////////////////////////////////////////////////////////
DraggerScalingEventHandler::~DraggerScalingEventHandler()
{}
////////////////////////////////////////////////////////////////////////////////
DraggerScalingEventHandler&
DraggerScalingEventHandler::operator=( const DraggerScalingEventHandler& rhs )
{
    if( this != &rhs )
    {
        m_activeModel = rhs.m_activeModel;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void DraggerScalingEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    try
    {
        ves::open::xml::CommandPtr geometryLODScaleCmd =
            boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject );
        if( geometryLODScaleCmd )
        {
            ves::open::xml::DataValuePairPtr scaleValue =
                geometryLODScaleCmd->GetDataValuePair( "Dragger Scaling Toggle Value" );
            double alpha = 0;
            scaleValue->GetData( alpha );
            UpdateDraggerScaling( true, alpha );
        }
    }
    catch( ... )
    {
        m_activeModel = 0;
        std::cout << "Invalid command passed to DraggerScalingEventHandler!!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void DraggerScalingEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject )
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
void DraggerScalingEventHandler::UpdateDraggerScaling( bool const&, double const& draggerScaling )
{
    scenegraph::SceneManager::instance()->
    GetManipulatorManager().SetDraggerScale( draggerScaling );
    scenegraph::SceneManager::instance()->
    GetManipulatorManager().GetSceneManipulator()->SetScale( draggerScaling );
}
////////////////////////////////////////////////////////////////////////////////


