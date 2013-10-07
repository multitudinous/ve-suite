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
#include <ves/xplorer/event/volume/VolumeVisSlots.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/xplorer/event/data/DataSetScalarBar.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/TextureBasedVizHandler.h>

#include <ves/xplorer/volume/cfdTextureDataSet.h>

#include <ves/xplorer/Debug.h>

#include <latticefx/core/vtk/DataSet.h>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace volume
{
////////////////////////////////////////////////////////////////////////////////
void HideVizFeature( std::string const&, std::vector< bool > const& hide )
{
    ves::xplorer::TextureBasedVizHandler::instance()->ViewTextureBasedVis( !hide.at( 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void SetTransientMode( std::string const&, std::vector< std::string > const& playMode )
{
    if( playMode.at( 0 ) == "Step Forward" )
    {
        ves::xplorer::TextureBasedVizHandler::instance()->StepTransientVisualization( "Forward" );
    }
    else if( playMode.at( 0 ) == "Step Back" )
    {
        ves::xplorer::TextureBasedVizHandler::instance()->StepTransientVisualization( "Backward" );
    }
    else if( playMode.at( 0 ) == "Play" )
    {
        ves::xplorer::TextureBasedVizHandler::instance()->PlayTransientVisualization();
    }
    else if( playMode.at( 0 ) == "Stop" )
    {
        ves::xplorer::TextureBasedVizHandler::instance()->StopTransientVisualization();
    }
}
////////////////////////////////////////////////////////////////////////////////
void SetTransientDuration( std::string const&, std::vector< double > const& duration )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateTransientDuration( duration.at( 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void EnablePhoneShader( std::string const&, std::vector< bool > const& enable )
{
    ves::xplorer::TextureBasedVizHandler::instance()->EnsurePhongShading( enable.at( 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void SetActiveShaderManager( std::string const& activeShaderManager )
{
    ves::xplorer::TextureBasedVizHandler::instance()->SetActiveShaderManager( activeShaderManager );
}
////////////////////////////////////////////////////////////////////////////////
void UpdateNumberOfSlicePlanes( std::string const&, std::vector< int > const& numberOfSlices )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateNumberOfSlicePlanes( numberOfSlices.at( 0 ) );

    ves::xplorer::TextureBasedVizHandler::instance()->UpdateGraph();
}
////////////////////////////////////////////////////////////////////////////////
void EnablePreIntegration( bool const& enable )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdatePreIntegrationTable( enable );
}
////////////////////////////////////////////////////////////////////////////////
void UpdateIsoSurfaceValue( std::string const&, std::vector< double > const& value )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateIsosurface( value.at( 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void EnableIsoSurfaces( std::string const&, std::vector< bool > const& enable )
{
    ves::xplorer::TextureBasedVizHandler::instance()->EnsureIsosurface( enable.at( 0 ) );
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
void UpdateROIBounds( std::string const&, std::vector< double > const& roi )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateClipPlane( roi );
}
////////////////////////////////////////////////////////////////////////////////
void TurnOnBBox( bool const& enable )
{
    ves::xplorer::TextureBasedVizHandler::instance()->UpdateBoundingBox( enable );
}

////////////////////////////////////////////////////////////////////////////////
lfx::core::DataSetPtr activateLfxDataSet( std::string const& activeDataset, bool activate )
{
	ves::xplorer::Model* activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
	if( activeModel == NULL ) return lfx::core::DataSetPtr();

	int idx =  activeModel->GetIndexOfLfxDataSet( activeDataset );
	lfx::core::DataSetPtr dataSet = activeModel->GetLfxDataSet( idx );
	if( !dataSet ) return dataSet;

	if( activate )
	{
		activeModel->SetActiveLfxDataSet( dataSet );
	}

	return dataSet;
}

////////////////////////////////////////////////////////////////////////////////
void ActivateTBDataset( std::string const& activeDataset )
{
    ves::xplorer::Model* activeModel =
        ves::xplorer::ModelHandler::instance()->GetActiveModel();
    lfx::core::vtk::DataSetPtr dataSet = activeModel->GetCfdDataSet(
                           activeModel->GetIndexOfDataSet( activeDataset ) );

    activeModel->SetActiveDataSet( dataSet );

    //make the CAD transparent
    activeModel->GetModelCADHandler()->MakeCADRootTransparent();
    if( !activeModel->GetDCS()->containsNode( activeModel->GetActiveDataSet()->GetDCS() ) )
    {
        vprDEBUG( vesDBG, 1 ) << "|\t\tadding active switch node to worldDCS"
                              << std::endl << vprDEBUG_FLUSH;
        activeModel->GetDCS()->addChild( activeModel->GetActiveDataSet()->GetDCS() );
    }
    osg::Switch* temp = activeModel->GetActiveDataSet()->GetSwitchNode();
    if( !activeModel->GetActiveDataSet()->GetDCS()->containsNode( temp ) )
    {
        vprDEBUG( vesDBG, 1 ) << "|\t\tadding active dcs node to worldDCS for classic ss "
                              << std::endl << vprDEBUG_FLUSH;
        activeModel->GetActiveDataSet()->GetDCS()->addChild( temp );
    }
    ///what happens if texture is somehow added first? Is that possible?
    activeModel->GetActiveDataSet()->GetSwitchNode()->setSingleChildOn( 1 );

    SetActiveTextureDataset();
}
////////////////////////////////////////////////////////////////////////////////
void UpdateScalarRange( double const& minRange, double const& maxRange )
{
    SetActiveTextureDataset();

    ves::xplorer::TextureBasedVizHandler::instance()->UpdateActiveTextureManager();

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
    //TB_ACTIVE_SOLUTION

    ves::xplorer::volume::cfdTextureDataSet* activeTDSet =
        SetActiveTextureDataset();
    if( !activeTDSet )
    {
        return;
    }
    //std::cout << dataType << std::endl;
    if( dataType == "Scalar" )
    {
        //std::cout << dataName << std::endl;

        activeTDSet->SetActiveScalar( dataName );

        ves::xplorer::TextureBasedVizHandler::instance()->UpdateActiveTextureManager();

        float floatRange[2];
        floatRange[0] = minRange;
        floatRange[1] = maxRange;
        ves::xplorer::TextureBasedVizHandler::instance()->UpdateScalarRange( floatRange );

        //need to pass the scalar range command to update it
        lfx::core::vtk::DataSetPtr dataSet =
            ModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
        if( !dataSet )
        {
            return;
        }

        /*DataSetScalarBar* scalarBar = dataSet->GetDataSetScalarBar();
        if( scalarBar )
        {
            dataSet->SetActiveScalar( dataName );
            scalarBar->AddScalarBarToGroup();
        }*/
    }
    else if( dataType == "Vector" )
    {
        activeTDSet->SetActiveVector( dataName );
    }

    EnablePreIntegration( true );

    ves::xplorer::TextureBasedVizHandler::instance()->UpdateGraph();
}
////////////////////////////////////////////////////////////////////////////////
void GetFloat(const boost::any &value, float *pf)
{
	*pf = static_cast<float>( boost::any_cast<double>( value ) );
}

////////////////////////////////////////////////////////////////////////////////
void UpdateLfxProperty(const std::string &dataSetName, const std::string &propName, const boost::any &value, int vloc)
{
	lfx::core::DataSetPtr pds = activateLfxDataSet( dataSetName, false );
	if( !pds.get() )
	{
		vprDEBUG( vesDBG, 0 ) << "|\tUpdateLfxProperty - failed to find the dataset: " << dataSetName << std::endl << vprDEBUG_FLUSH;
		return;
	}

	lfx::core::RendererPtr prender = pds->getRenderer();
	if( !prender.get() )
	{
		// vprDEBUG( vesDBG, 0 ) << "|\tUpdateLfxUniform - no renderer is in the dataset: " << dataSetName << std::endl << vprDEBUG_FLUSH;
		return;
	}


	lfx::core::Renderer::UniformInfo &ui  = prender->getUniform( propName );
	if( ui._prototype->getName() != propName )
	{
		vprDEBUG( vesDBG, 0 ) << "|\tUpdateLfxUniform - uniform: " << propName << " not found!" << std::endl << vprDEBUG_FLUSH;
		return;
	}

	// TODO: DEAL WITH NON UNIFORM NAMED PROPERTIES, IE: Hide, DataSet

	
	switch( ui._prototype->getType() )
	{
	case osg::Uniform::FLOAT_MAT4:
		{
			osg::Matrixf mat;
			ui._prototype->get( mat );

			if (vloc > -1 && vloc < 16)
			{
				GetFloat( value, &(mat.ptr()[vloc]) );
				ui._prototype->set( mat );
			}
	
			break;
		}
	case osg::Uniform::FLOAT_VEC2:
		{
			osg::Vec2f vec2;
			ui._prototype->get( vec2 );

			if( vloc > -1 && vloc < 2 )
			{
				GetFloat( value, &vec2[vloc] );
				ui._prototype->set( vec2 );
			}
			break;
		}
	case osg::Uniform::FLOAT_VEC3:
		{
			osg::Vec3f vec3;
			ui._prototype->get( vec3 );

			if( vloc > -1 && vloc < 3 )
			{
				GetFloat( value, &vec3[vloc] );
				ui._prototype->set( vec3 );
			}
			break;
		}
	case osg::Uniform::FLOAT_VEC4:
		{
			osg::Vec4f vec4;
			ui._prototype->get( vec4 );

			if( vloc > -1 && vloc < 4 )
			{
				GetFloat( value, &vec4[vloc] );
				ui._prototype->set( vec4 );
			}
			break;
		}
	case osg::Uniform::FLOAT:
		{
			float f = 0;
			GetFloat( value, &f );
			ui._prototype->set( f );
			break;
		}
	case osg::Uniform::SAMPLER_1D:
	case osg::Uniform::SAMPLER_2D:
	case osg::Uniform::SAMPLER_3D:
	case osg::Uniform::INT:
		{
			// TODO: this probably doesn't make sense for samplers!
			int i = boost::any_cast<int>( value );
			ui._prototype->set( i );
			break;
		}
	case osg::Uniform::BOOL:
		{
			bool b = boost::any_cast<bool>( value );
			ui._prototype->set( b );
			break;
		}
	default:
		{
			vprDEBUG( vesDBG, 0 ) << "|\tunsupported uniform type: " << propName << std::endl << vprDEBUG_FLUSH;
			break;
		}
	}
}

////////////////////////////////////////////////////////////////////////////////
} // namespace volume
} // namespace event
} // namespace xplorer
} // namespace ves
