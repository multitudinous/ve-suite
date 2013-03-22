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
#include <ves/xplorer/event/data/DataTransformEH.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/event/data/SeedPoints.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/TransformPtr.h>

#include <ves/open/xml/Transform.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/cad/CADNode.h>

#include <iostream>

#include <latticefx/core/vtk/DataSet.h>

#include <osgwTools/Quat.h>

using namespace ves::xplorer::event;
using namespace ves::open::xml;

///////////////////////////////////////////////////////////////////////////////////////
DataTransformEventHandler::DataTransformEventHandler()
    : ves::xplorer::event::EventHandler()
{
    _activeModel = 0;
}
///////////////////////////////////////////////////////////////////////////////////////
DataTransformEventHandler::DataTransformEventHandler( const DataTransformEventHandler& rhs )
    : ves::xplorer::event::EventHandler( rhs )
{}
///////////////////////////////////////////////////////////////////////////////////////
DataTransformEventHandler::~DataTransformEventHandler()
{}
///Equal operator
///////////////////////////////////////////////////////////////////////////////////////
DataTransformEventHandler& DataTransformEventHandler::operator=( const DataTransformEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }
    return *this;
}
///////////////////////////////////////////////////////////////////////////////////////
void DataTransformEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    try
    {
        if( !_activeModel )
        {
            return;
        }

        CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
        DataValuePairPtr datasetName = command->GetDataValuePair( "Parameter Block ID" );
        std::string filename;
        datasetName->GetData( filename );
        if( filename != "NULL" )
        {
            _activeModel->SetActiveDataSet(
                _activeModel->GetCfdDataSet(
                    _activeModel->GetIndexOfDataSet( filename ) ) );
        }

        if( !_activeModel->GetActiveDataSet() )
        {
            std::cout << "|\tNo active dataset assigned." << std::endl;
            return;
        }

        DataValuePairPtr newTransform = command->GetDataValuePair( "Transform" );
        osg::PositionAttitudeTransform* transform = _activeModel->GetActiveDataSet()->GetDCS();

        if( transform )
        {
            TransformPtr dataTransform =
                boost::dynamic_pointer_cast<Transform>( newTransform->GetDataXMLObject() );

            std::vector< double > translation = dataTransform->GetTranslationArray()->GetArray();
            std::vector< double > rotation = dataTransform->GetRotationArray()->GetArray();
            std::vector< double > scale = dataTransform->GetScaleArray()->GetArray();
            
            transform->setScale( osg::Vec3d( scale[ 0 ], scale[ 1 ], scale[ 2 ] ) );
            transform->setPosition( osg::Vec3d( translation[ 0 ], translation[ 1 ], translation[ 2 ] ) );
            transform->setAttitude( osgwTools::makeHPRQuat( rotation[ 0 ], rotation[ 1 ], rotation[ 2 ] ) );
    
            osg::PositionAttitudeTransform* seedPoint =
                ves::xplorer::EnvironmentHandler::instance()->GetSeedPointsDCS();
            seedPoint->setScale( transform->getScale() );
            seedPoint->setPosition( transform->getPosition() );
            seedPoint->setAttitude( transform->getAttitude() );
        }
        //Reset back to null after working with the dataset we are after.
        _activeModel->SetActiveDataSet( lfx::core::vtk::DataSetPtr() );
    }
    catch( ... )
    {
        std::cout << "Error!!!Invalid command passed to DataTransformEH!!" << std::endl;
    }
}
///////////////////////////////////////////////////////////////////////////////////////
void DataTransformEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
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
        std::cout << "Invalid object passed to DataTransformEventHandler::SetGlobalBaseObject!" << std::endl;
    }
}
///////////////////////////////////////////////////////////////////////////////////////
