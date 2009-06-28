#include <vtkAbaqusInputDeckReader.h>
#include <vtkXMLDataSetWriter.h>
#include <vtkDataSet.h>
#include <vtkUnstructuredGrid.h>
#include <vtkDataObject.h>
#include <vtkCompositeDataPipeline.h>

int main( int argc, char* argv[] )
{
    //vtkCompositeDataPipeline* prototype = vtkCompositeDataPipeline::New();
    //vtkAlgorithm::SetDefaultExecutivePrototype( prototype );
    //prototype->Delete();

    vtkAbaqusInputDeckReader* reader = vtkAbaqusInputDeckReader::New();
    reader->DebugOn();
    reader->SetFileName( "bc_pl_dataset1_v2__25mm.inp" );
    reader->Update();

    vtkUnstructuredGrid* grid = vtkUnstructuredGrid::New();
    //grid  = vtkUnstructuredGrid::New();
    grid->ShallowCopy( reader->GetOutput() );
    //ugReader->Delete();
    
    vtkXMLDataSetWriter* writer = vtkXMLDataSetWriter::New();
    writer->SetFileName( "test.vtu" );
    writer->SetInput(grid );
    writer->SetDataModeToAscii();
    writer->Write();
    writer->Delete();
    reader->Delete();
    
    return 0;
}
