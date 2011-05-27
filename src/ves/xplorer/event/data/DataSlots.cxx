/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/data/DataSlots.h>
#include <string>
#include <vector>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/data/DatasetPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace data
{
////////////////////////////////////////////////////////////////////////////////
void SetContourPlaneGreyscale( std::string const& uuid, std::vector< bool > const& greyscaleflag )
{
    if( ModelHandler::instance()->GetActiveModel() == NULL )
    {
        return;
    }
    
    if( ModelHandler::instance()->GetActiveModel()->GetActiveDataSet() == NULL )
    {
        return;
    }

    ModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->SetGreyscaleFlag( greyscaleflag[0] );
}
////////////////////////////////////////////////////////////////////////////////
void TransformDatasetNode( const std::string& uuid, const std::vector< double >& transform )
{
    ves::xplorer::Model* activeModel = ModelHandler::instance()->GetActiveModel();

    if( activeModel == NULL )
    {
        return;
    }

    ves::xplorer::data::DatasetPropertySet set;
    set.SetUUID( uuid );
    set.LoadFromDatabase();
    std::string datasetName =
            boost::any_cast<std::string>(set.GetPropertyValue( "Filename" ));

    DataSet* dataSet = activeModel->GetCfdDataSet(
            activeModel->GetIndexOfDataSet( datasetName ) );

    scenegraph::DCS* dcs = dataSet->GetDCS();

    if( dcs )
    {
        // Entire transform is packed into a single vector. Unpack into
        // separate translation, rotation, and scale pieces.
        std::vector<double>::const_iterator start = transform.begin();
        std::vector<double>::const_iterator stop = transform.begin() + 3;
        std::vector<double> translation( start, stop  );
        std::vector<double> rotation( start + 3, stop + 3 );
        std::vector<double> scale( start + 6, stop + 6 );

        dcs->SetTranslationArray( translation );
        dcs->SetRotationArray( rotation );
        dcs->SetScaleArray( scale );

//        EnvironmentHandler::instance()->GetSeedPointsDCS()->SetTranslationArray( translation );
//        EnvironmentHandler::instance()->GetSeedPointsDCS()->SetRotationArray( rotation );
//        EnvironmentHandler::instance()->GetSeedPointsDCS()->SetScaleArray( scale );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SetDatasetSurfaceWrap( std::string const& uuid, bool const& surfaceWrap )
{
    std::cout << "SetDatasetSurfaceWrap" << std::endl << std::flush;
    ves::xplorer::Model* activeModel = ModelHandler::instance()->GetActiveModel();
    ves::xplorer::data::DatasetPropertySet set;
    set.SetUUID( uuid );
    set.LoadFromDatabase();
    std::string datasetName = boost::any_cast<std::string>(set.GetPropertyValue( "Filename" ));

    DataSet* dataSet = activeModel->GetCfdDataSet(
        activeModel->GetIndexOfDataSet( datasetName ) );

    if( !dataSet )
    {
        return;
    }

    if( surfaceWrap )
    {
        dataSet->CreateSurfaceWrap();
    }
    else
    {
        // TODO: How do we undo a surface wrap?
    }
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
