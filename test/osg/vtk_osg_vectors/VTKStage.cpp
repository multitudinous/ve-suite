#include "VTKStage.h"

#include <vtkXMLUnstructuredGridReader.h>
#include <vtkCellData.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLStructuredGridReader.h>
#include <vtkPolyDataReader.h>
#include <vtkPolyDataWriter.h>
#include <vtkStructuredGrid.h>

VTKStage::VTKStage(void)
{
    vtkCompositeDataPipeline* prototype = vtkCompositeDataPipeline::New();
    vtkAlgorithm::SetDefaultExecutivePrototype(prototype);
    prototype->Delete();

	polydataReader = NULL;
	c2p = NULL;
	m_multiGroupGeomFilter = NULL;
	m_geometryFilter = NULL;
	glyph = NULL;
	tfilter = NULL;
	tris = NULL;
	strip = NULL;
	cone = NULL;
	mask = NULL;
}

VTKStage::VTKStage(string name)
{ 
	xmlPolydateFname =name; 
	polydataReader = NULL;
	c2p = NULL;
	m_multiGroupGeomFilter = NULL;
	m_geometryFilter = NULL;
	glyph = NULL;
	tfilter = NULL;
	tris = NULL;
	strip = NULL;
	cone = NULL;
	mask = NULL;
}

VTKStage::~VTKStage(void)
{
	if (polydataReader)
		polydataReader->Delete();
	if (c2p)
		c2p->Delete();
	if (m_multiGroupGeomFilter)
		m_multiGroupGeomFilter->Delete();
	if (m_geometryFilter)
		m_geometryFilter->Delete();
	if (glyph)
		glyph->Delete();
	if (tfilter)
		tfilter->Delete();
	if (tris)
		tris->Delete();
    if (strip)
		strip->Delete();
	if (cone)
		cone->Delete();
	if (mask)
		mask->Delete();
}

vtkPolyData* VTKStage::GetOutput()
{

	return mask->GetOutput();
}

void VTKStage::Update(int n)
{
	if (polydataReader)
		polydataReader->Delete();

	polydataReader = vtkXMLUnstructuredGridReader::New();
	polydataReader->SetFileName(xmlPolydateFname.c_str());//("C:\\Dougm\\testvecglyphs_large.vtp");
    polydataReader->Update();
	vtkDataSet* dataset = static_cast< vtkXMLUnstructuredGridReader* >( polydataReader )->GetOutput();

	

    //dataset->GetPointData()->SetActiveVectors( "steve's_vector" );
    //dataset->GetPointData()->SetActiveScalars( "first-scalar" );
    //dataset->GetCellData()->SetActiveVectors( "steve's_vector" );
    //dataset->GetCellData()->SetActiveScalars( "first-scalar" );


	if (c2p)
		c2p->Delete();
    c2p = vtkCellDataToPointData::New();
    c2p->SetInput( dataset );
    c2p->Update();

    // get every nth point from the dataSet data 
	//?? the above comment doesn't make sense to me CYANG 07/11/09
	if (m_multiGroupGeomFilter)
		m_multiGroupGeomFilter->Delete();
    m_multiGroupGeomFilter = vtkCompositeDataGeometryFilter::New();

	if (m_geometryFilter)
		m_geometryFilter->Delete();
	m_geometryFilter = vtkGeometryFilter::New();
    
	if( dataset->IsA( "vtkCompositeDataSet" ) )
    {
        m_multiGroupGeomFilter->SetInputConnection( c2p->GetOutputPort() );
    }
    else
    {
        m_geometryFilter->SetInputConnection( c2p->GetOutputPort() );
    }
    
    // Using glyph3D to insert arrow to the data sets
	if (glyph)
		glyph->Delete();
    glyph = vtkGlyph3D::New();
    
	if (tfilter)
		tfilter->Delete();
    tfilter = vtkThresholdPoints::New();

    if (tris)
		tris->Delete();
    tris = vtkTriangleFilter::New();
    
	if (strip)
		strip->Delete();
	strip = vtkStripper::New();    
    
	//using vtkMaskPoints filter to select every nth point
	if (mask)
		mask->Delete();
	mask = vtkMaskPoints::New();
	mask->RandomModeOn();

	if (n>0)
		mask->SetOnRatio(n);

	//void cfdVectorBase::SetGlyphWithThreshold()
    {
        double currentScalarRange[ 2 ] = {0, 1};
        double _vectorThreshHoldValues[ 2 ] = { 0, 1 };
        //dataset->GetRange( currentScalarRange );
		//dataset->GetScalarRange(currentScalarRange );







        
        /*if( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] &&
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
			mask->SetInputConnection( strip->GetOutputPort() );
            glyph->SetInputConnection( mask->GetOutputPort() );
        }
        else if( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] )*/
        /*{
            mask->SetInputConnection( m_geometryFilter->GetOutputPort() );
            mask->Update();
            tfilter->SetInputConnection( mask->GetOutputPort() );
            tfilter->ThresholdByUpper( 1.0f );
            //tfilter->SetInputArrayToProcess( 0, 0, 0,
            //                                vtkDataObject::FIELD_ASSOCIATION_POINTS, 
            //                               "first-scalar" );
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
            mask->SetInputConnection( strip->GetOutputPort() );
            glyph->SetInputConnection( mask->GetOutputPort() );
        }
        else*/





        {
			mask->SetInputConnection( m_geometryFilter->GetOutputPort() );
            mask->Update();

               /*{
    vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
    writer->SetInput( mask->GetOutput() );
    writer->SetDataModeToAscii();
    writer->SetFileName( "teststreamervecglyphs.vtp" );
    writer->Write();
    writer->Delete();
    }*/

            glyph->SetInputConnection( mask->GetOutputPort() );
        }
    }

    {
		if (cone)
			cone->Delete();
        cone = vtkConeSource::New();
        cone->SetResolution( 5 );

        glyph->SetSource( cone->GetOutput() );
        glyph->SetVectorModeToUseVector();
        glyph->SetScaleFactor( 1.0f );
        
        //if( _scaleByVector == 0 )
        {
            glyph->SetScaleModeToDataScalingOff();
            glyph->Update();
        }
        /*else
        {
            glyph->SetScaleModeToScaleByVector();
            glyph->SetColorModeToColorByScalar();
            glyph->ClampingOn();
            glyph->SetRange( GetActiveDataSet()->GetVectorMagRange() );
        }*/
    }
    
    /*    
        vtkPolyData* polydata = vtkPolyData::New();
	//       polydata = (vtkPolyData*)mask->GetOutput();
       polydata = mask->GetOutput();

 	vtkPolyDataWriter *writer=vtkPolyDataWriter::New();

 	writer->SetInput(polydata);
 	writer->SetFileName("theone.vtk");
 	writer->Write();
    */
   
   //file dumping is turned off 
   /*{
    vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
    writer->SetInput( glyph->GetOutput() );
    writer->SetDataModeToAscii();
    writer->SetFileName( "teststreamervecglyphs.vtp" );
    writer->Write();
    writer->Delete();
    }*/

  return;

}

void VTKStage::Dump(string fname)
{
	if (glyph!=NULL)
	{
		vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
		writer->SetInput( glyph->GetOutput() );
		writer->SetDataModeToAscii();
		writer->SetFileName(fname.c_str()); //( "C:\\Dougm\\teststreamervecglyphs.vtk" );
		writer->Write();
		writer->Delete();
	}
}
