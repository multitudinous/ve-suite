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
#include <ves/xplorer/event/volume/VolumeVisSlots.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/xplorer/event/data/DataSetScalarBar.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/TextureBasedVizHandler.h>

#include <ves/xplorer/volume/cfdTextureDataSet.h>

#include <ves/xplorer/Debug.h>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace volume
{
////////////////////////////////////////////////////////////////////////////////
void SetTransientMode( std::string const& playMode, std::string const& direction )
{
    if( playMode == "Step" )
    {
        ves::xplorer::TextureBasedVizHandler::instance()->StepTransientVisualization( direction );
    }
    else if( playMode == "Play" )
    {
        ves::xplorer::TextureBasedVizHandler::instance()->PlayTransientVisualization();
    }
    else if( playMode == "Stop" )
    {
        ves::xplorer::TextureBasedVizHandler::instance()->StopTransientVisualization();
    }
}
////////////////////////////////////////////////////////////////////////////////
void SetTransientDuration( double const& duration )
{    
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateTransientDuration( duration );        
}
////////////////////////////////////////////////////////////////////////////////
void EnablePhoneShader( bool const& enable )
{
    ves::xplorer::TextureBasedVizHandler::instance()->EnsurePhongShading( enable );
}
////////////////////////////////////////////////////////////////////////////////
void SetActiveShaderManager( std::string const& activeShaderManager )
{
    ves::xplorer::TextureBasedVizHandler::instance()->SetActiveShaderManager( activeShaderManager );
}
////////////////////////////////////////////////////////////////////////////////
void UpdateNumberOfSlicePlanes( unsigned int const& numberOfSlices )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateNumberOfSlicePlanes( numberOfSlices );
}
////////////////////////////////////////////////////////////////////////////////
void EnablePreIntegration( bool const& enable )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdatePreIntegrationTable( enable );
}
////////////////////////////////////////////////////////////////////////////////
void UpdateIsoSurfaceValue( double const& value )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateIsosurface( value );
}
////////////////////////////////////////////////////////////////////////////////
void EnableIsoSurfaces( bool const& enable )
{
    ves::xplorer::TextureBasedVizHandler::instance()->EnsureIsosurface( enable );    
}
////////////////////////////////////////////////////////////////////////////////
void UpdateClipPlaneSettings( std::string const& planeDirection, 
    std::string const& planeCoordinate, double const& roiValue, 
    double const& minRoiValue, double const& maxRoiValue )
{
    if( planeDirection != "Both" )
    {
        ves::xplorer::TextureBasedVizHandler::instance()->UpdateClipPlane( planeCoordinate,
                                                                          planeDirection,
                                                                          roiValue );
    }
    else if( planeDirection == "Both" )
    {
        ves::xplorer::TextureBasedVizHandler::instance()->UpdateClipPlane( planeCoordinate,
                                                                          "Positive",
                                                                          minRoiValue );

        ves::xplorer::TextureBasedVizHandler::instance()->UpdateClipPlane( planeCoordinate,
                                                                          "Negative",
                                                                          maxRoiValue );
    }
}
////////////////////////////////////////////////////////////////////////////////
void TurnOnBBox( bool const& enable )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateBoundingBox( enable );
}
////////////////////////////////////////////////////////////////////////////////
void ActivateTBDataset( std::string const& activeDataset )
{
    ves::xplorer::Model* activeModel = 
        ves::xplorer::ModelHandler::instance()->GetActiveModel();
    DataSet* dataSet = activeModel->GetCfdDataSet(
            activeModel->GetIndexOfDataSet( activeDataset ) );
    
    activeModel->SetActiveDataSet( dataSet );
    
    //make the CAD transparent
    activeModel->GetModelCADHandler()->MakeCADRootTransparent();
    if( !activeModel->GetDCS()->SearchChild( activeModel->GetActiveDataSet()->GetDCS() ) )
    {
        vprDEBUG( vesDBG, 2 ) << "|\t\tadding active switch node to worldDCS"
        << std::endl << vprDEBUG_FLUSH;
        activeModel->GetDCS()->AddChild( activeModel->GetActiveDataSet()->GetDCS() );
    }
    ves::xplorer::scenegraph::Switch* temp = activeModel->GetActiveDataSet()->GetSwitchNode();
    if( !activeModel->GetActiveDataSet()->GetDCS()->SearchChild( temp ) )
    {
        vprDEBUG( vesDBG, 2 ) << "|\t\tadding active dcs node to worldDCS for classic ss "
        << std::endl << vprDEBUG_FLUSH;
        activeModel->GetActiveDataSet()->GetDCS()->AddChild( temp );
    }
    ///what happens if texture is somehow added first? Is that possible?
    activeModel->GetActiveDataSet()->GetSwitchNode()->SetVal( 1 );

    SetActiveTextureDataset();
}
////////////////////////////////////////////////////////////////////////////////
void UpdateScalarRange( double const& minRange, double const& maxRange )
{
    SetActiveTextureDataset();
    
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateActiveTextureManager();
    
    //this is overkill
    float floatRange[2];
    floatRange[0] = minRange;
    floatRange[1] = maxRange;
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateScalarRange( floatRange );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::volume::cfdTextureDataSet* SetActiveTextureDataset()
{
    ves::xplorer::Model* activeModel = 
        ves::xplorer::ModelHandler::instance()->GetActiveModel();
    if( activeModel )
    {
        //This assumes there is only one texture dataset per model which isn't quite right---biv
        ves::xplorer::volume::cfdTextureDataSet* activeTDSet = 
            activeModel->GetTextureDataSet( 0 );
        activeModel->SetActiveTextureDataSet( activeTDSet );
        ves::xplorer::TextureBasedVizHandler::instance()->
            SetActiveTextureDataSet( activeTDSet );
        return activeTDSet;
    }
    
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void UpdateTBSolution( std::string const& dataName, std::string const& dataType, double const& minRange, double const& maxRange )
{    
    ves::xplorer::volume::cfdTextureDataSet* activeTDSet = 
        SetActiveTextureDataset();
    if( !activeTDSet )
    {
        return;
    }
    
    if( dataType == "Scalar" )
    {
        activeTDSet->SetActiveScalar( dataName );
        
        ves::xplorer::TextureBasedVizHandler::instance()->UpdateActiveTextureManager();
        
        //this is overkill
        float floatRange[2];
        floatRange[0] = minRange;
        floatRange[1] = maxRange;
        ves::xplorer::TextureBasedVizHandler::instance()->UpdateScalarRange( floatRange );

        //need to pass the scalar range command to update it
        DataSet* dataSet = 
            ModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
        DataSetScalarBar* scalarBar = dataSet->GetDataSetScalarBar();
        if( scalarBar )
        {
            dataSet->SetActiveScalar( dataName );
            scalarBar->AddScalarBarToGroup();
        }
    }
    else if( dataType == "Vector" )
    {
        activeTDSet->SetActiveVector( dataName );
    }
}
////////////////////////////////////////////////////////////////////////////////
} // namespace volume
} // namespace event
} // namespace xplorer
} // namespace ves
