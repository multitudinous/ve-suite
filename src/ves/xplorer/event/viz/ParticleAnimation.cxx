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
#include <ves/xplorer/event/viz/ParticleAnimation.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <propertystore/PropertySet.h>

#include <ves/xplorer/Debug.h>

#include <latticefx/core/vtk/DataSet.h>

#include <latticefx/utils/vtk/FindVertexCellsCallback.h>
#include <latticefx/utils/vtk/GetScalarDataArraysCallback.h>
#include <latticefx/utils/vtk/CountNumberOfParametersCallback.h>
#include <latticefx/utils/vtk/ProcessScalarRangeCallback.h>

#include <latticefx/core/DataSet.h>
#include <latticefx/core/ChannelData.h>
#include <latticefx/core/ChannelDataOSGArray.h>
#include <latticefx/core/RTPOperation.h>
#include <latticefx/core/Renderer.h>
#include <latticefx/core/TextureUtils.h>
#include <latticefx/core/BoundUtils.h>
#include <latticefx/core/VectorRenderer.h>
#include <latticefx/core/TransferFunctionUtils.h>
#include <latticefx/core/PlayControl.h>
#include <latticefx/core/DBDisk.h>
#include <latticefx/core/Log.h>
#include <latticefx/core/LogMacros.h>

#include <latticefx/core/vtk/VTKSurfaceWrapRTP.h>
#include <latticefx/core/vtk/VTKActorRenderer.h>
#include <latticefx/core/vtk/ChannelDatavtkDataObject.h>

#include <vtkMath.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer::event::viz;

////////////////////////////////////////////////////////////////////////////////
ParticleAnimation::ParticleAnimation()
{
    warpSurface = false;
    warpedContourScale = 1.0f;

    _particleOption = 0;
    _particleScale = 1;
}
////////////////////////////////////////////////////////////////////////////////
ParticleAnimation::ParticleAnimation( ParticleAnimation const& src )
    :
    cfdObjects( src ),
    colorByScalar( src.colorByScalar ),
    warpSurface( src.warpSurface ),
    warpedContourScale( src.warpedContourScale ),
    _particleOption( src._particleOption ),
    _particleScale( src._particleScale )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ParticleAnimation::~ParticleAnimation()
{
}
////////////////////////////////////////////////////////////////////////////////
cfdObjects* ParticleAnimation::CreateCopy()
{
    return new ParticleAnimation( *this );
}
////////////////////////////////////////////////////////////////////////////////
void ParticleAnimation::Update()
{
    if( GetActiveDataSet() == NULL )
    {
        vprDEBUG( vesDBG, 0 )
                << "|\tParticleAnimation has no data so setting updateFlag to false"
                << std::endl << vprDEBUG_FLUSH;
        this->updateFlag = false;
        return;
    }
    else
    {
        vprDEBUG( vesDBG, 1 )
                << "|\tParticleAnimation: this->GetActiveDataSet() = "
                << GetActiveDataSet()->GetFileName() << std::endl
                << vprDEBUG_FLUSH;
    }

    CreateLFXPlane();
    this->updateFlag = true;
    return;
}

//////////////////////////////////////////////////////////
void ParticleAnimation::SetParticleOption( unsigned int option )
{
    _particleOption = option;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int ParticleAnimation::GetParticleOption()
{
    return _particleOption;
}
////////////////////////////////////////////////////////////////////////////////
void ParticleAnimation::SetParticleScale( float x )
{
    _particleScale = x;
}
////////////////////////////////////////////////////////////////////////////////
float ParticleAnimation::GetParticleScale()
{
    return _particleScale;
}
////////////////////////////////////////////////////////////////////////////////
void ParticleAnimation::UpdateCommand()
{
    UpdatePropertySet();
    return;
	 
    //Call base method - currently does nothing
    cfdObjects::UpdateCommand();

    //Extract the specific commands from the overall command
    ves::open::xml::DataValuePairPtr activeModelDVP =
        veCommand->GetDataValuePair( "Sub-Dialog Settings" );
    ves::open::xml::CommandPtr objectCommand =
        boost::dynamic_pointer_cast<ves::open::xml::Command>(
            activeModelDVP->GetDataXMLObject() );

    //Extract the isosurface value
    activeModelDVP = objectCommand->GetDataValuePair( "Polydata Value" );
    //double planePosition;
    activeModelDVP->GetData( warpedContourScale );
    //SetRequestedValue( static_cast< int >( planePosition ) );

    activeModelDVP = objectCommand->GetDataValuePair( "Color By Scalar" );
    activeModelDVP->GetData( colorByScalar );

    activeModelDVP = objectCommand->GetDataValuePair( "Warped Surface" );
    unsigned int surface;
    activeModelDVP->GetData( surface );
    if( surface == 1 )
    {
        warpSurface = true;
    }
    else if( surface == 0 )
    {
        warpSurface = false;
    }

    activeModelDVP = objectCommand->GetDataValuePair( "GPU Tools" );
    unsigned int gpuTools;
    activeModelDVP->GetData( gpuTools );
    m_gpuTools = gpuTools;
}
////////////////////////////////////////////////////////////////////////////////
float ParticleAnimation::GetSphereScaleFactor()
{
    // this->GetParticleScale() is obtained from gui, -100 < sphereScale < 100
    // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
    // convert range to -4 < x < 4, and compute the exponent...
    vprDEBUG( vesDBG, 1 )
            << "|\t\tParticleAnimation::GetSphereScaleFactor sphereScale = "
            << GetParticleScale()
            << std::endl << vprDEBUG_FLUSH;

    float scaleFactor = 0.0;

    if( -100 <= this->GetParticleScale() && this->GetParticleScale() <= 100 )
    {
        scaleFactor = exp( this->GetParticleScale() / 25.0 );
    }

    vprDEBUG( vesDBG, 1 )
            << "\t\tParticleAnimation::GetSphereScaleFactor scaleFactor = "
            << scaleFactor
            << std::endl << vprDEBUG_FLUSH;

    return scaleFactor;
}
////////////////////////////////////////////////////////////////////////////////
void ParticleAnimation::UpdatePropertySet()
{
    //Extract the isosurface value
    warpedContourScale =
        boost::any_cast<double>( m_propertySet->GetPropertyValue( "WarpedScaleFactor" ) );

    colorByScalar =
        boost::any_cast< std::string >( m_propertySet->GetPropertyValue( "ColorByScalar" ) );

    warpSurface =
        boost::any_cast<bool>( m_propertySet->GetPropertyValue( "UseWarpedSurface" ) );

    //m_gpuTools =
      //  boost::any_cast<bool>( m_propertySet->GetPropertyValue( "UseGPUTools" ) );
}
////////////////////////////////////////////////////////////////////////////////
void ParticleAnimation::CreateLFXPlane()
{
    if( !GetActiveDataSet()->IsPartOfTransientSeries() )
    {
    }
    std::cout << "create particles." << std::endl;
    lfx::core::DBBasePtr dbBase;
    {
        lfx::core::DBDiskPtr disk( lfx::core::DBDiskPtr( new lfx::core::DBDisk() ) );
        std::string filePath( "." );
        disk->setRootPath( filePath );
        dbBase = disk;
    }

    {
        transientSeries = GetActiveDataSet()->GetTransientDataSets();
        m_dsp = createInstanced( transientSeries, "test", "test", dbBase );
        //Now force an update of the lfx pipeline
        bool success = m_dsp->updateAll();
        
        if( !success )
        {
            std::cout << "Some sort of problem with lfx " << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
lfx::core::DataSetPtr ParticleAnimation::createInstanced( const std::vector< lfx::core::vtk::DataSetPtr >& transData,
                                      const std::string& activeScalar,
                                      const std::string& activeVector,
                                      lfx::core::DBBasePtr dbBase )
{
    std::vector< std::vector< std::pair< vtkIdType, double* > > >  m_pointCollection;
    ///The raw data for the respective points
    std::vector< std::vector< std::pair< std::string, std::vector< double > > > >  m_dataCollection;
    
    m_pointCollection.clear();
    
    std::vector< lfx::core::vtk::DataSetPtr > m_transientDataSet;
    m_transientDataSet = transData;
    std::string m_activeVector = activeVector;
    std::string m_activeScalar = activeScalar;
    
std::string diameterNameString = "Diameter";
std::string vmagNameString = "MotionVector_magnitude";
double conversionFactor = 0.00328084;

    lfx::vtk_utils::FindVertexCellsCallback* findVertexCellsCbk =
        new lfx::vtk_utils::FindVertexCellsCallback();
    lfx::vtk_utils::DataObjectHandler* dataObjectHandler =
        new lfx::vtk_utils::DataObjectHandler();
    dataObjectHandler->SetDatasetOperatorCallback( findVertexCellsCbk );
    
    size_t maxNumPoints = 0;
    for( size_t i = 0; i < m_transientDataSet.size(); ++i )
    {
        vtkDataObject* tempDataSet = m_transientDataSet.at( i )->GetDataSet();
        
        dataObjectHandler->OperateOnAllDatasetsInObject( tempDataSet );
        std::vector< std::pair< vtkIdType, double* > > tempCellGroups =
        findVertexCellsCbk->GetVertexCells();
        m_pointCollection.push_back( tempCellGroups );
        findVertexCellsCbk->ResetPointGroup();
        if( maxNumPoints < tempCellGroups.size() )
        {
            maxNumPoints = tempCellGroups.size();
        }
    }
    delete findVertexCellsCbk;
    std::cout << maxNumPoints << std::endl;
    lfx::vtk_utils::CountNumberOfParametersCallback* getNumParamsCbk =
    new lfx::vtk_utils::CountNumberOfParametersCallback();
    dataObjectHandler->SetDatasetOperatorCallback( getNumParamsCbk );
    for( size_t i = 0; i < m_transientDataSet.size(); ++i )
    {
        vtkDataObject* tempDataSet = m_transientDataSet.at( i )->GetDataSet();
        
        dataObjectHandler->OperateOnAllDatasetsInObject( tempDataSet );
    }
    std::vector< std::string > scalarNames = getNumParamsCbk->GetParameterNames( false );
    for( size_t i = 0; i < scalarNames.size(); ++i )
    {
        std::cout << " scalar names " << scalarNames.at( i ) << std::endl;
    }
    
    lfx::vtk_utils::ProcessScalarRangeCallback* processScalarRangeCbk =
    new lfx::vtk_utils::ProcessScalarRangeCallback();
    double* diamRange = new double[ 2 ];
    double* vmagRange = new double[ 2 ];
    double diamRangeF[ 2 ] = {10000000., -10000000.};
    double vmagRangeF[ 2 ] = {10000000., -10000000.};
    for( size_t j = 0; j < m_transientDataSet.size(); ++j )
    {
        dataObjectHandler->SetDatasetOperatorCallback( processScalarRangeCbk );
        vtkDataObject* tempDataSet = m_transientDataSet.at( j )->GetDataSet();
        dataObjectHandler->OperateOnAllDatasetsInObject( tempDataSet );
        processScalarRangeCbk->GetScalarRange( diameterNameString, diamRange );
        processScalarRangeCbk->GetScalarRange( vmagNameString, vmagRange );
        if( diamRange[ 0 ] < diamRangeF[ 0 ] )
        {
            diamRangeF[ 0 ] = diamRange[ 0 ];
        }
        
        if( diamRange[ 1 ] > diamRangeF[ 1 ] )
        {
            diamRangeF[ 1 ] = diamRange[ 1 ];
        }
        
        if( vmagRange[ 0 ] < vmagRangeF[ 0 ] )
        {
            vmagRangeF[ 0 ] = vmagRange[ 0 ];
        }
        
        if( vmagRange[ 1 ] > vmagRangeF[ 1 ] )
        {
            vmagRangeF[ 1 ] = vmagRange[ 1 ];
        }
    }
    delete [] diamRange;
    diamRange = 0;
    delete [] vmagRange;
    vmagRange = 0;
    
    std::cout << " range " << diamRangeF[ 0 ] << " " << diamRangeF[ 1 ] << std::endl;
    std::cout << " range " << vmagRangeF[ 0 ] << " " << vmagRangeF[ 1 ] << std::endl;
    
    lfx::vtk_utils::GetScalarDataArraysCallback* getScalarDataArrayCbk =
    new lfx::vtk_utils::GetScalarDataArraysCallback();
    dataObjectHandler->SetDatasetOperatorCallback( getScalarDataArrayCbk );
    getScalarDataArrayCbk->SetScalarNames( scalarNames );
    for( size_t i = 0; i < m_transientDataSet.size(); ++i )
    {
        vtkDataObject* tempDataSet = m_transientDataSet.at( i )->GetDataSet();
        
        dataObjectHandler->OperateOnAllDatasetsInObject( tempDataSet );
        std::vector< std::pair< std::string, std::vector< double > > > tempCellGroups =
        getScalarDataArrayCbk->GetCellData();
        m_dataCollection.push_back( tempCellGroups );
        getScalarDataArrayCbk->ResetPointGroup();
    }
    
    delete getScalarDataArrayCbk;
    delete dataObjectHandler;
    delete getNumParamsCbk;
    delete processScalarRangeCbk;
    
    lfx::core::DataSetPtr dsp( new lfx::core::DataSet() );
    for( size_t i = 0; i < transData.size(); ++i )
    {
        std::vector< std::pair< std::string, std::vector< double > > >* tempScalarData = &m_dataCollection.at( i );
        osg::ref_ptr< osg::FloatArray > radArray( new osg::FloatArray );
        //radArray->resize( samplesPerTime );  
        osg::ref_ptr< osg::FloatArray > depthArray( new osg::FloatArray );
        //depthArray->resize( samplesPerTime );
        std::vector< double >* diamArray = 0;
        std::vector< double >* vmagArray = 0;
        
        for( size_t j = 0; j < tempScalarData->size(); ++j )
        {
            if( tempScalarData->at( j ).first == diameterNameString && tempScalarData->at( j ).second.size() > 0 )
            {
                //std::cout << tempScalarData->at( j ).second.size() << std::endl;
                diamArray = &tempScalarData->at( j ).second;
            }
            else if( tempScalarData->at( j ).first == vmagNameString && tempScalarData->at( j ).second.size() > 0 )
            {
                //std::cout << tempScalarData->at( j ).second.size() << std::endl;
                vmagArray = &tempScalarData->at( j ).second;
            }
            //std::cout << " scalar name " << tempScalarData->at( j ).first << " " << tempScalarData->at( j ).second.size() << std::endl;
        }
        
        
        std::vector< std::pair< vtkIdType, double* > >* tempCellGroups =
        &m_pointCollection.at( i );
        osg::ref_ptr< osg::Vec3Array > posArray( new osg::Vec3Array );
        double val = 0;
        for( size_t j = 0; j < tempCellGroups->size(); ++j )
        {
            //std::cout << tempCellGroups->size() << std::endl;
            double* tempData = tempCellGroups->at( j ).second;
            osg::Vec3d position( tempData[ 0 ], tempData[ 1 ], tempData[ 2 ] );
            position *= conversionFactor;
            posArray->push_back( position );
            //std::cout << tempCellGroups->at( j ).first << " " << tempData[ 0 ] << " " << tempData[ 1 ] << " " << tempData[ 2 ] << std::endl;
            
            //radArray->push_back( 0.02 );
            if( diamArray )
            {
                val = diamArray->at( j ) * 0.5 * conversionFactor;
                //val = vtkMath::ClampAndNormalizeValue( val, diamRange );
                
                radArray->push_back( val );
            }
            else
            {
                std::cout << " no diameter " << j << std::endl;
            }
            
            //depthArray->push_back( float(j%6)/5. );
            if( vmagArray )
            {
                val = vmagArray->at( j );
                val = vtkMath::ClampAndNormalizeValue( val, vmagRangeF );
                
                depthArray->push_back( val );
            }
            else
            {
                std::cout << " no vmag " << j << std::endl;
            }
        }
        
        lfx::core::ChannelDataOSGArrayPtr posData( new lfx::core::ChannelDataOSGArray( "positions", posArray.get() ) );
        dsp->addChannel( posData, i * 0.25 );
        lfx::core::ChannelDataOSGArrayPtr radData( new lfx::core::ChannelDataOSGArray( "radii", radArray.get() ) );
        dsp->addChannel( radData, i * 0.25 );
        lfx::core::ChannelDataOSGArrayPtr depthData( new lfx::core::ChannelDataOSGArray( "depth", depthArray.get() ) );
        dsp->addChannel( depthData, i * 0.25 );
    }
    //341 - 343
    lfx::core::VectorRendererPtr renderOp( new lfx::core::VectorRenderer() );
    renderOp->setPointStyle( lfx::core::VectorRenderer::SPHERES );
    renderOp->addInput( "positions" );
    renderOp->addInput( "radii" );
    renderOp->addInput( "depth" ); // From DepthComputation channel creator
    
    // Configure transfer function.
    renderOp->setTransferFunctionInput( "depth" );
    renderOp->setTransferFunction( lfx::core::loadImageFromDat( "01.dat" ) );
    renderOp->setTransferFunctionDestination( lfx::core::Renderer::TF_RGBA );
    
    // Configure hardware mask.
    renderOp->setHardwareMaskInputSource( lfx::core::Renderer::HM_SOURCE_RED );
    renderOp->setHardwareMaskOperator( lfx::core::Renderer::HM_OP_OFF );
    renderOp->setHardwareMaskReference( 0.f );
    
    renderOp->setDB( dbBase );
    
    dsp->setRenderer( renderOp );
    dsp->setDB( dbBase );
    
    ///Clean up memory now that we have transferred it to the streamline list
    m_pointCollection.clear();
    m_dataCollection.clear();;
    
    return dsp;
}
