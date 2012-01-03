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

#include <ves/xplorer/event/data/SPBoundEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/event/data/SeedPoints.h>
#include <ves/xplorer/DataSet.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>

//#include <vtkDataSet.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
SeedPointBoundsEventHandler::SeedPointBoundsEventHandler()
    :
    ves::xplorer::event::EventHandler()
{
    _activeModel = 0;

    CONNECTSIGNALS_1( "%UpdateSeedPointBounds",
                     void ( const std::vector< double >& bounds ),
                     &SeedPointBoundsEventHandler::UpdateAllBounds,
                     m_connections, any_SignalType, normal_Priority );
}
///////////////////////////////////////////////////////////////////
SeedPointBoundsEventHandler
::SeedPointBoundsEventHandler( const SeedPointBoundsEventHandler& ceh )
    :
    ves::xplorer::event::EventHandler( ceh )
{
    _activeModel = ceh._activeModel;
}
/////////////////////////////////////////////////////////////////////
SeedPointBoundsEventHandler::~SeedPointBoundsEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////
SeedPointBoundsEventHandler&
SeedPointBoundsEventHandler::operator=( const SeedPointBoundsEventHandler& rhs )
{
    if( &rhs != this )
    {
        _activeModel = rhs._activeModel;
    }
    return *this;
}
///////////////////////////////////////////////////////////////////////////
void SeedPointBoundsEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    try
    {
        if( model )
        {
            _activeModel = dynamic_cast<ves::xplorer::Model*>( model );
        }
        else
        {
            _activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        }
    }
    catch ( ... )
    {
        _activeModel = 0;
        std::cout << "Invalid object passed to SeedPointBoundsEventHandler!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void SeedPointBoundsEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    if( !_activeModel )
        throw;
    try
    {
        CommandPtr command = boost::dynamic_pointer_cast<ves::open::xml::Command>( veXMLObject );

        DataValuePairPtr coordinate = command->GetDataValuePair( "Coordinate" );
        std::string boundCoordinate;
        coordinate->GetData( boundCoordinate );

        if( boundCoordinate == "All Bounds" )
        {
            std::vector<double> allBoundaryData;
            DataValuePairPtr bounds = command->GetDataValuePair( "Bounds" );
            bounds->GetData( allBoundaryData );
            UpdateAllBounds( allBoundaryData );
        }
        else
        {
            DataValuePairPtr minMaxDVP = command->GetDataValuePair( "MinMax" );
            std::string minMaxUpdate;
            minMaxDVP->GetData( minMaxUpdate );

            if( minMaxUpdate != "Both" )
            {
                DataValuePairPtr value = command->GetDataValuePair( "Value" );
                ///Get the percentage
                double alpha;
                value->GetData( alpha );
                ///Get the dataset bounds
                double databounds[6] = {0, 0, 0, 0, 0, 0};
                //_activeModel->GetActiveDataSet()->GetDataSet()->GetWholeBoundingBox(databounds);
                _activeModel->GetActiveDataSet()->GetBounds( databounds );

                //udpate the correct bound
                unsigned int index = ( boundCoordinate == "X" ) ? 0 : ( boundCoordinate == "Y" ) ? 2 : 4;
                double newValue = 0;
                newValue = databounds[index] + alpha * ( databounds[index+1] - databounds[index] );
                ves::xplorer::EnvironmentHandler::instance()->GetSeedPoints()->UpdateBounds( newValue,
                        boundCoordinate,
                        minMaxUpdate );
            }
            else if( minMaxUpdate == "Both" )
            {
                DataValuePairPtr minValue = command->GetDataValuePair( "Min Value" );
                double minAlpha;
                minValue->GetData( minAlpha );

                ///Get the dataset bounds
                double databounds[6] = {0, 0, 0, 0, 0, 0};
                //_activeModel->GetActiveDataSet()->GetDataSet()->GetWholeBoundingBox(databounds);
                _activeModel->GetActiveDataSet()->GetBounds( databounds );

                //udpate the correct bound
                unsigned int index = ( boundCoordinate == "X" ) ? 0 : ( boundCoordinate == "Y" ) ? 2 : 4;

                double newValue = 0;
                newValue = databounds[index] + minAlpha * ( databounds[index+1] - databounds[index] );
                ves::xplorer::EnvironmentHandler::instance()->GetSeedPoints()->UpdateBounds( newValue,
                        boundCoordinate,
                        "Min" );
                DataValuePairPtr maxValue = command->GetDataValuePair( "Max Value" );
                double maxAlpha;
                maxValue->GetData( maxAlpha );

                newValue = databounds[index] + maxAlpha * ( databounds[index+1] - databounds[index] );
                ves::xplorer::EnvironmentHandler::instance()->GetSeedPoints()->UpdateBounds( newValue,
                        boundCoordinate,
                        "Max" );

            }
        }
    }
    catch ( ... )
    {
        std::cout << "Invalid Bounds!!" << std::endl;
        std::cout << "SeedPointBoundsEventHandler::Execute()" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void SeedPointBoundsEventHandler::UpdateAllBounds( const std::vector< double >& bounds )
{
    if( !_activeModel )
    {
        _activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        if( !_activeModel )
        {
            return;
        }
    }

    double databounds[6] = {0, 0, 0, 0, 0, 0};
    //_activeModel->GetActiveDataSet()->GetDataSet()->GetWholeBoundingBox(databounds);
    _activeModel->GetActiveDataSet()->GetBounds( databounds );
    double newValue[6] = {0, 0, 0, 0, 0, 0};
    newValue[0] = databounds[0] + bounds.at( 0 ) * ( databounds[1] - databounds[0] );
    newValue[1] = databounds[0] + bounds.at( 1 ) * ( databounds[1] - databounds[0] );
    newValue[2] = databounds[2] + bounds.at( 2 ) * ( databounds[3] - databounds[2] );
    newValue[3] = databounds[2] + bounds.at( 3 ) * ( databounds[3] - databounds[2] );
    newValue[4] = databounds[4] + bounds.at( 4 ) * ( databounds[5] - databounds[4] );
    newValue[5] = databounds[4] + bounds.at( 5 ) * ( databounds[5] - databounds[4] );
    
    ves::xplorer::EnvironmentHandler::instance()->GetSeedPoints()->
        SetBounds( newValue[0],
                    newValue[1],
                    newValue[2],
                    newValue[3],
                    newValue[4],
                    newValue[5] );
}
////////////////////////////////////////////////////////////////////////////////
