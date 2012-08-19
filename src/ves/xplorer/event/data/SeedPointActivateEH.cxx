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

#include <ves/xplorer/event/data/SeedPointActivateEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/event/data/SeedPoints.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
SeedPointActivateEventHandler::SeedPointActivateEventHandler()
    :
    ves::xplorer::event::EventHandler()
{
    _activeModel = 0;

    ///Connect up to the signal emitted from StreamlinePropertySet
    CONNECTSIGNALS_2( "%ActivateSeedPoints",
                      void ( const std::string & dataSetName, const bool seedPointDisplay ),
                      &SeedPointActivateEventHandler::ActivateSeedPoints,
                      m_connections, any_SignalType, normal_Priority );
}
///////////////////////////////////////////////////////////////////
SeedPointActivateEventHandler
::SeedPointActivateEventHandler( const SeedPointActivateEventHandler& ceh )
    :
    ves::xplorer::event::EventHandler( ceh )
{
    _activeModel = ceh._activeModel;
}
/////////////////////////////////////////////////////////////////////
SeedPointActivateEventHandler::~SeedPointActivateEventHandler()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
SeedPointActivateEventHandler&
SeedPointActivateEventHandler::operator=( const SeedPointActivateEventHandler& rhs )
{
    if( &rhs != this )
    {
        _activeModel = rhs._activeModel;
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void SeedPointActivateEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject )
{
    ///is this overkill????
    try
    {
        if( baseObject )
        {
            _activeModel = dynamic_cast<ves::xplorer::Model*>( baseObject );
        }
        else
        {
            _activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        }
    }
    catch( ... )
    {
        _activeModel = 0;
        std::cout << "Invalid object passed to TextureBasedEventHandler!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void SeedPointActivateEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    try
    {
        if( _activeModel )
        {
            ///what happens if texture is somehow added first? Is that possible?
            CommandPtr command =
                boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject );
            DataValuePairPtr seedPointsFlag =
                command->GetDataValuePair( "OnOff" );
            DataValuePairPtr activeDataset =
                command->GetDataValuePair( "Active Dataset" );
            std::string datasetname;
            activeDataset->GetData( datasetname );

            ActivateSeedPoints( datasetname, ( ( seedPointsFlag->GetUIntData() == 1 ) ? true : false ) );
        }
    }
    catch( ... )
    {
        std::cout << "Invalid Model!!" << std::endl;
        std::cout << "SeedPointActivateEventHandler::_operateOnNode()" << std::endl;
    }

}
////////////////////////////////////////////////////////////////////////////////
void SeedPointActivateEventHandler::ActivateSeedPoints( const std::string& dataSetName, const bool seedPointDisplay )
{
    if( !_activeModel )
    {
        _activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        if( !_activeModel )
        {
            return;
        }
    }

    //make the CAD transparent
    _activeModel->GetModelCADHandler()->MakeCADRootTransparent();

    //check to see if the seed points exist
    if( !ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot()->containsNode( ves::xplorer::EnvironmentHandler::instance()->GetSeedPointsDCS() ) )
    {
        ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot()->
        addChild( ves::xplorer::EnvironmentHandler::instance()->GetSeedPointsDCS() );
    }

    //this seems to be a bad sequence of calls but we need to set the
    //active dataset otherwise this set of calls goes in every seed pointEH
    //as well as all the commands have to lug this extra info.
    _activeModel->SetActiveDataSet( _activeModel->GetCfdDataSet( _activeModel->GetIndexOfDataSet( dataSetName ) ) );
    ves::xplorer::scenegraph::DCS* tempDCS = _activeModel->GetActiveDataSet()->GetDCS();
    ves::xplorer::scenegraph::DCS* seedPointDCS = ves::xplorer::EnvironmentHandler::instance()->GetSeedPointsDCS();
    seedPointDCS->SetMat( tempDCS->GetMat() );

    ves::xplorer::EnvironmentHandler::instance()->GetSeedPoints()->Toggle( seedPointDisplay );
}
////////////////////////////////////////////////////////////////////////////////
