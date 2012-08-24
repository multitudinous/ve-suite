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
#include <ves/xplorer/event/data/AxesEventHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/device/cfdCursor.h>
#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
#include <ves/xplorer/data/DatasetPropertySet.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/Debug.h>

#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
AxesEventHandler::AxesEventHandler()
    : ves::xplorer::event::EventHandler(),
      _activeModel( 0 )
{
    CONNECTSIGNALS_2( "%ShowDatasetAxes",
                      void ( const std::string&, const bool ),
                      &AxesEventHandler::ShowAxes,
                      m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
AxesEventHandler::AxesEventHandler( const AxesEventHandler& rhs )
    : ves::xplorer::event::EventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
AxesEventHandler::~AxesEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
AxesEventHandler& AxesEventHandler::operator=( const AxesEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void AxesEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    try
    {
        if( model )
        {
            _activeModel = dynamic_cast< ves::xplorer::Model* >( model );
        }
        else
        {
            _activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        }
    }
    catch( ... )
    {
        _activeModel = 0;
        std::cout << "Invalid object passed to BBoxEventHandler::SetGlobalBaseObject!" << std::endl;
    }
}
//////////////////////////////////////////////////////////////////////////
void AxesEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
    DataValuePairPtr activeModelDVP =
        command->GetDataValuePair( "Axes State" );
    std::string datasetName =
        command->GetDataValuePair( "Active Dataset" )->GetDataString();

    unsigned int state = 0;
    activeModelDVP->GetData( state );
    if( _activeModel )
    {
        DataSet* dataSet = _activeModel->GetCfdDataSet(
                               _activeModel->GetIndexOfDataSet( datasetName ) );
        _activeModel->SetActiveDataSet( dataSet );
        dataSet->SetAxesState( state );
    }

}

void AxesEventHandler::ShowAxes( const std::string& uuid, const bool& show )
{
    SetGlobalBaseObject();
    ves::xplorer::data::DatasetPropertySet set;
    set.SetUUID( uuid );
    set.Load();
    std::string datasetName = boost::any_cast<std::string>( set.GetPropertyValue( "Filename" ) );

    DataSet* dataSet = _activeModel->GetCfdDataSet(
                           _activeModel->GetIndexOfDataSet( datasetName ) );
    if( dataSet )
    {
        dataSet->SetAxesState( show );
    }
}
