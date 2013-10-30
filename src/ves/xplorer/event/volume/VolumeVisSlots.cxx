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
#include <latticefx/core/VolumeRenderer.h>

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
lfx::core::RendererPtr GetLfxRenderer( const std::string &dataSetName, lfx::core::DataSetPtr &pds )
{
	pds = activateLfxDataSet( dataSetName, false );
	if( !pds.get() )
	{
		vprDEBUG( vesDBG, 0 ) << "|\tGetLfxRenderer - failed to find the dataset: " << dataSetName << std::endl << vprDEBUG_FLUSH;
		return lfx::core::RendererPtr();
	}

	lfx::core::RendererPtr prender = pds->getRenderer();
	if( !prender.get() )
	{
		// vprDEBUG( vesDBG, 0 ) << "|\tUpdateLfxUniform - no renderer is in the dataset: " << dataSetName << std::endl << vprDEBUG_FLUSH;
	}

	return prender;
}

////////////////////////////////////////////////////////////////////////////////
lfx::core::RendererPtr GetLfxRenderer( const std::string &dataSetName )
{
	lfx::core::DataSetPtr pds;
	return GetLfxRenderer( dataSetName, pds );
}

////////////////////////////////////////////////////////////////////////////////
void UpdateLfxChannel( const std::string &dataSetName, const std::string &chanName )
{
	lfx::core::DataSetPtr ds;
	lfx::core::RendererPtr r = GetLfxRenderer( dataSetName, ds );
	if( !r.get() ) return;

	// TODO: FIGURE OUT HOW TO SET THE INPUT NAME FOR THE RENDERER TYPE

	std::string curName = r->getInputNameAlias( lfx::core::VolumeRenderer::VOLUME_DATA );
	if( curName != chanName )
	{
		r->setInputNameAlias( lfx::core::VolumeRenderer::VOLUME_DATA, chanName );
		ds->setRenderer( r ); // force dirty
		ds->updateAll(); // update now with new channel data.
	}
}

////////////////////////////////////////////////////////////////////////////////
void UpdateLfxRenderProp(const std::string &dataSetName, int propType, boost::any value1, boost::any value2)
{
	lfx::core::DataSetPtr ds;
	lfx::core::RendererPtr r = GetLfxRenderer( dataSetName, ds );
	if( !r.get() ) return;

	std::string s;
	float f;
	osg::Vec2f v2f;

	if (propType > lfx::core::Renderer::PT_ENUM_BEGIN && propType < lfx::core::Renderer::PT_ENUM_END)
	{
		s = boost::any_cast<std::string>( value1 );
	}
	else if (propType > lfx::core::Renderer::PT_FLOAT_BEGIN && propType < lfx::core::Renderer::PT_FLOAT_END)
	{
		GetFloat( value1, &f );
	}
	else if (propType > lfx::core::Renderer::PT_FLOATRNG_BEGIN && propType < lfx::core::Renderer::PT_FLOATRNG_END)
	{
		GetFloat( value1, &v2f[0] );
		GetFloat( value2, &v2f[1] );
	}

	switch( propType )
	{
		// enums
	case lfx::core::Renderer::PT_TF_DST:
		r->setTransferFunctionDestination( r->getEnumFromNameTrans( s ) );
		break;
	case lfx::core::Renderer::PT_HM_SRC:
		r->setHardwareMaskInputSource( r->getEnumFromNameMaskInput( s ) );
		break;
	case lfx::core::Renderer::PT_HM_OPE:
		r->setHardwareMaskOperator( r->getEnumFromNameHardwareMaskOperator( s ) );
		break;
		// floats
	case lfx::core::Renderer::PT_HM_REF:
		r->setHardwareMaskReference( f );
		break;
	case lfx::core::Renderer::PT_HM_EPS:
		r->setHardwareMaskEpsilon( f );
		break;
	// float range
	case lfx::core::Renderer::PT_TF_INR:
		r->setTransferFunctionInputRange( v2f );
		break;
	default:
		vprDEBUG( vesDBG, 0 ) << "|\tunrecognized lfx::core::Renderer::PropType: " << propType << std::endl << vprDEBUG_FLUSH;
	}

	ds->updateRendererUniforms(); // refresh uniforms down in osg
}

////////////////////////////////////////////////////////////////////////////////
} // namespace volume
} // namespace event
} // namespace xplorer
} // namespace ves
