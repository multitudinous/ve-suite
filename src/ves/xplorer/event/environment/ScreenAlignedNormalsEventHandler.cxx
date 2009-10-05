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

#include <ves/xplorer/event/environment/ScreenAlignedNormalsEventHandler.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;

//////////////////////////////////////////////////////////////////////
ScreenAlignedNormalsEventHandler::ScreenAlignedNormalsEventHandler()
{
    m_activeModel = 0;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ScreenAlignedNormalsEventHandler::ScreenAlignedNormalsEventHandler( const ScreenAlignedNormalsEventHandler& ceh )
{
    m_activeModel = ceh.m_activeModel;
}
///////////////////////////////////////////////////////////////////////
ScreenAlignedNormalsEventHandler::~ScreenAlignedNormalsEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////////
ScreenAlignedNormalsEventHandler&
ScreenAlignedNormalsEventHandler::operator=( const ScreenAlignedNormalsEventHandler& rhs )
{
    if( this != &rhs )
    {
        m_activeModel = rhs.m_activeModel;
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////////////
void ScreenAlignedNormalsEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    /*try
    {
        ves::open::xml::CommandPtr geometryLODScaleCmd = boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject );
        if(  geometryLODScaleCmd )
        {
            ves::open::xml::DataValuePairPtr scaleValue =
                geometryLODScaleCmd->GetDataValuePair( "Geometry LOD Scale" );
            long alpha = 0;
            scaleValue->GetData( alpha );

            double scale = .0001*exp( alpha * .18420680745);
            ves::xplorer::EnvironmentHandler::instance()->SetGlobalLODScale( scale );
        }
    }
    catch ( ... )
    {
        m_activeModel = 0;
        std::cout << "Invalid command passed to EphemerisAutoDateTimeEventHandler!!" << std::endl;


    }*/
}
/////////////////////////////////////////////////////////////////////////////////////////////////
void ScreenAlignedNormalsEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject )
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


