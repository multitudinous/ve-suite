#include <vtkActor.h>
#include <vtkCellDataToPointData.h>
#include <vtkContourFilter.h>
#include <vtkDebugLeaks.h>

#include <vtkMultiBlockDataSet.h>
#include <vtkCompositeDataGeometryFilter.h>
#include <vtkLookupTable.h>
#include <vtkThresholdPoints.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkGeometryFilter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkConeSource.h>
#include <vtkGlyph3D.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkPlane.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkCompositeDataPipeline.h>
#include <vtkFLUENTReader.h>
#include <vtkCutter.h>
#include <vtksys/ios/sstream>

#include <vtkTriangleFilter.h>
#include <vtkStripper.h>
#include <vtkPolyDataNormals.h>

#include <iostream>

int main(int argc, char* argv[])
{
    vtkCompositeDataPipeline* prototype = vtkCompositeDataPipeline::New();
    vtkAlgorithm::SetDefaultExecutivePrototype(prototype);
    prototype->Delete();

    vtkDataSet* dataset = 0;

    vtkCellDataToPointData* c2p = vtkCellDataToPointData::New();
    c2p->SetInput( dataset );
    c2p->Update();

    // get every nth point from the dataSet data
    vtkCompositeDataGeometryFilter* m_multiGroupGeomFilter = vtkCompositeDataGeometryFilter::New();
    vtkGeometryFilter* m_geometryFilter = vtkGeometryFilter::New();
    if( dataset->IsA( "vtkCompositeDataSet" ) )
    {
        m_multiGroupGeomFilter->SetInputConnection( c2p->GetOutputPort() );
        //return m_multiGroupGeomFilter->GetOutputPort(0);
    }
    else
    {
        m_geometryFilter->SetInputConnection( c2p->GetOutputPort() );
        //return m_geometryFilter->GetOutputPort();
    }
    //ptmask = vtkMaskPoints::New();
    //ptmask->RandomModeOn();
    
    // Using glyph3D to insert arrow to the data sets
    vtkGlyph3D* glyph = vtkGlyph3D::New();
    
    vtkThresholdPoints* tfilter = vtkThresholdPoints::New();

    
    vtkTriangleFilter* tris = vtkTriangleFilter::New();
    vtkStripper* strip = vtkStripper::New();    
    //void cfdVectorBase::SetGlyphWithThreshold()
    {
        double currentScalarRange[ 2 ] = {0, 1};
        double _vectorThreshHoldValues[ 2 ] = { 0, 1 };
        //dataset->GetRange( currentScalarRange );
        
        if( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] &&
           _vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
        {
            tfilter->SetInputConnection( m_geometryFilter->GetOutputPort() );
            tfilter->ThresholdBetween( _vectorThreshHoldValues[ 0 ],
                                      _vectorThreshHoldValues[ 1 ] );
            //tfilter->SetInputArrayToProcess( 0, 0, 0,
            //                                vtkDataObject::FIELD_ASSOCIATION_POINTS, 
            //                                GetActiveDataSet()->GetActiveScalarName().c_str() );
            
            tris->SetInputConnection( tfilter->GetOutputPort() );
            strip->SetInputConnection( tris->GetOutputPort() );
            glyph->SetInputConnection( strip->GetOutputPort() );
        }
        else if( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] )
        {
            tfilter->SetInputConnection( m_geometryFilter->GetOutputPort() );
            tfilter->ThresholdByUpper( _vectorThreshHoldValues[ 0 ] );
            //tfilter->SetInputArrayToProcess( 0, 0, 0,
            //                                vtkDataObject::FIELD_ASSOCIATION_POINTS, 
            //                                GetActiveDataSet()->GetActiveScalarName().c_str() );
            tris->SetInputConnection( tfilter->GetOutputPort() );
            strip->SetInputConnection( tris->GetOutputPort() );
            glyph->SetInputConnection( strip->GetOutputPort() );
        }
        else if( _vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
        {
            tfilter->SetInputConnection( m_geometryFilter->GetOutputPort() );
            tfilter->ThresholdByLower( _vectorThreshHoldValues[ 1 ] );
            //tfilter->SetInputArrayToProcess( 0, 0, 0,
            //                                vtkDataObject::FIELD_ASSOCIATION_POINTS, 
            //                                GetActiveDataSet()->GetActiveScalarName().c_str() );
            tris->SetInputConnection( tfilter->GetOutputPort() );
            strip->SetInputConnection( tris->GetOutputPort() );
            glyph->SetInputConnection( strip->GetOutputPort() );
        }
        else
        {
            glyph->SetInputConnection( m_geometryFilter->GetOutputPort() );
        }
    }

    {
        vtkConeSource* cone = vtkConeSource::New();
        cone->SetResolution( 5 );

        glyph->SetSource( cone->GetOutput() );
        glyph->SetVectorModeToUseVector();
        glyph->SetScaleFactor( 1.0f );
        
        //if( _scaleByVector == 0 )
        {
            glyph->SetScaleModeToDataScalingOff();
        }
        /*else
        {
            glyph->SetScaleModeToScaleByVector();
            glyph->SetColorModeToColorByScalar();
            glyph->ClampingOn();
            glyph->SetRange( GetActiveDataSet()->GetVectorMagRange() );
        }*/
    }
    
    
    {
    vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
    writer->SetInput( glyph->GetOutput() );
    writer->SetDataModeToAscii();
    writer->SetFileName( "teststreamervecglyphs.vtk" );
    writer->Write();
    writer->Delete();
    }
    
    
  return 0;
}

