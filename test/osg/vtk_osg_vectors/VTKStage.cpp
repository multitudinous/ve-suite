#include "VTKStage.h"

VTKStage::VTKStage(void)
{
	polydataReader = NULL;
	c2p = NULL;
	m_multiGroupGeomFilter = NULL;
	m_geometryFilter = NULL;
	glyph = NULL;
	tfilter = NULL;
	tris = NULL;
	strip = NULL;
	cone = NULL;
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
}

vtkGlyph3D* VTKStage::GetOutput()
{
	return glyph;
}

void VTKStage::Update()
{
	//Not sure why we need this, but I am leaving this alone. CYANG 07/10/09
    vtkCompositeDataPipeline* prototype = vtkCompositeDataPipeline::New();
    vtkAlgorithm::SetDefaultExecutivePrototype(prototype);
    prototype->Delete();

	if (polydataReader)
		polydataReader->Delete();
	polydataReader = vtkXMLPolyDataReader::New();
	polydataReader->SetFileName(xmlPolydateFname.c_str());//("C:\\Dougm\\testvecglyphs_large.vtp");
	vtkDataSet* dataset = polydataReader->GetOutput();

	if (c2p)
		c2p->Delete();
    c2p = vtkCellDataToPointData::New();
    c2p->SetInput( dataset );
    c2p->Update();

    // get every nth point from the dataSet data
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
   file dumping is turned off 
   {
    vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
    writer->SetInput( glyph->GetOutput() );
    writer->SetDataModeToAscii();
    writer->SetFileName( "C:\\Dougm\\teststreamervecglyphs.vtk" );
    writer->Write();
    writer->Delete();
    }
    */
    
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