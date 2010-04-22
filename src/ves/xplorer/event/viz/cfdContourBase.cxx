
/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/xplorer/event/viz/cfdContourBase.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/event/viz/cfdCuttingPlane.h>
#include <ves/xplorer/event/viz/cfdPlanes.h>
#include <ves/xplorer/Debug.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <vtkPolyData.h>
#include <vtkContourFilter.h>                // contour lines
#include <vtkBandedPolyDataContourFilter.h>  // banded contours
#include <vtkGeometryFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkLookupTable.h>
#include <vtkDecimatePro.h>
#include <vtkTriangleFilter.h>
#include <vtkStripper.h>
#include <vtkPolyDataNormals.h>
#include <vtkCutter.h>
#include <vtkPlane.h>
#include <vtkAlgorithmOutput.h>
#include <vtkProbeFilter.h>
#include <vtkCompositeDataProbeFilter.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkDoubleArray.h>

#include <vtkCellDataToPointData.h>
#include <vtkPCellDataToPointData.h>

#include <vtkFLUENTReader.h>
#include <vtkMultiBlockDataSet.h>

#include <vtkPassThroughFilter.h>
#include <vtkXMLPolyDataWriter.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

// this class requires that the dataset has a scalar field.
cfdContourBase::cfdContourBase()
        : 
        cfdObjects(),
        mC2p( vtkCellDataToPointData::New() ),
        deci( vtkDecimatePro::New() )
{
    cfilter = vtkContourFilter::New();              // for contourlines
    bfilter = vtkBandedPolyDataContourFilter::New();// for banded contours
    // turn clipping on to avoid unnecessary value generations with
    // vtkBandedPolyDataContourFilter::GenerateValues().
    bfilter->ClippingOn();
    tris = vtkTriangleFilter::New();
    strip = vtkStripper::New();

    mapper = vtkPolyDataMapper::New();
    //mapper->SetColorModeToMapScalars();
    //mapper->ImmediateModeRenderingOn();
    normals = vtkPolyDataNormals::New();

    warpedContourScale = 0.0f;
    contourOpacity = 1.0f;
    contourLOD = 1;
    cuttingPlane = 0;
}
////////////////////////////////////////////////////////////////////////////////
cfdContourBase::~cfdContourBase()
{
    //vprDEBUG(vesDBG,2) << "cfdContourBase destructor"
    //                      << std::endl  << vprDEBUG_FLUSH;

    //this->filter->Delete();
    //this->filter = NULL;

    mC2p->Delete();
    mC2p = 0;

    deci->Delete();
    deci = 0;
    
    if( cfilter )
    {
        this->cfilter->Delete();
        this->cfilter = 0;
    }
    if( bfilter )
    {
        this->bfilter->Delete();
        this->bfilter = 0;
    }

    if( tris )
    {
        this->tris->Delete();
        this->tris = 0;
    }

    if( strip )
    {
        this->strip->Delete();
        this->strip = 0;
    }

    if( mapper )
    {
        this->mapper->Delete();
        this->mapper = 0;
    }

    if( normals )
    {
        normals->Delete();
        normals = 0;
    }

    if( cuttingPlane )
    {
        delete cuttingPlane;
        cuttingPlane = NULL;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::SetMapperInput( vtkAlgorithmOutput* polydata )
{
    mC2p->SetInputConnection( polydata );
    mC2p->Update();
    
    tris->SetInputConnection( mC2p->GetOutputPort() );
    tris->Update();
    //tris->GetOutput()->ReleaseDataFlagOn();

    // decimate points is used for lod control of contours
    /*this->deci->SetInputConnection( tris->GetOutputPort() );
    this->deci->PreserveTopologyOn();
    this->deci->BoundaryVertexDeletionOff();
    deci->Update();*/
    //deci->GetOutput()->ReleaseDataFlagOn();

    this->strip->SetInputConnection( tris->GetOutputPort() );
    strip->Update();
    //strip->GetOutput()->ReleaseDataFlagOn();

    if( this->fillType == 0 )
    {
        normals->SetInputConnection( strip->GetOutputPort() );
        normals->SetFeatureAngle( 130.0f );
        //normals->GetOutput()->ReleaseDataFlagOn();
        normals->ComputePointNormalsOn();
        //normals->ComputeCellNormalsOn();
        normals->FlipNormalsOn();
        normals->Update();
    }
    else if( this->fillType == 1 ) // banded contours
    {
        // putting the decimation routines as inputs to the bfilter
        // cause the bfilter to crash while being updated
        this->bfilter->SetInputConnection( strip->GetOutputPort() );
        double range[2];
        this->GetActiveDataSet()->GetUserRange( range );
        this->bfilter->GenerateValues( 10, range[0], range[1] );
        this->bfilter->SetScalarModeToValue();
        this->bfilter->GenerateContourEdgesOn();
        bfilter->SetInputArrayToProcess( 0, 0, 0,
              vtkDataObject::FIELD_ASSOCIATION_POINTS, 
              GetActiveDataSet()->GetActiveScalarName().c_str() );
        
        //bfilter->GetOutput()->ReleaseDataFlagOn();
        normals->SetInputConnection( bfilter->GetOutputPort() );
        normals->SetFeatureAngle( 130.0f );
        //normals->GetOutput()->ReleaseDataFlagOn();
        normals->ComputePointNormalsOn();
        //normals->ComputeCellNormalsOn();
        normals->FlipNormalsOn();
    }
    else if( this->fillType == 2 ) // contourlines
    {
        this->cfilter->SetInputConnection( mC2p->GetOutputPort() );
        double range[2];
        this->GetActiveDataSet()->GetUserRange( range );
        this->cfilter->GenerateValues( 10, range[0], range[1] );
        //this->cfilter->UseScalarTreeOn();
        cfilter->SetInputArrayToProcess( 0, 0, 0,
            vtkDataObject::FIELD_ASSOCIATION_POINTS, 
            GetActiveDataSet()->GetActiveScalarName().c_str() );
        //cfilter->GetOutput()->ReleaseDataFlagOn();
        normals->SetInputConnection( cfilter->GetOutputPort() );
        normals->SetFeatureAngle( 130.0f );
        //normals->GetOutput()->ReleaseDataFlagOn();
        normals->ComputePointNormalsOn();
        //normals->ComputeCellNormalsOn();
        normals->FlipNormalsOn();
    }
    vtkAlgorithmOutput* tempPolydata = 0;
    tempPolydata = ApplyGeometryFilterNew( normals->GetOutputPort() );

    mapper->SetInputConnection( tempPolydata );
    //mapper->SetScalarModeToDefault();
    //mapper->SetColorModeToDefault();
    //mapper->SetColorModeToMapScalars();
    //mapper->InterpolateScalarsBeforeMappingOff();
    mapper->SetScalarModeToUsePointFieldData();
    mapper->UseLookupTableScalarRangeOn();
    mapper->SelectColorArray( GetActiveDataSet()->
        GetActiveScalarName().c_str() );
    mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
    mapper->Update();

}	
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::UpdateCommand()
{
    //Call base method - currently does nothing
    cfdObjects::UpdateCommand();

    //Extract the specific commands from the overall command
    ves::open::xml::DataValuePairPtr activeModelDVP = veCommand->GetDataValuePair( "Sub-Dialog Settings" );
    ves::open::xml::CommandPtr objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>(  activeModelDVP->GetDataXMLObject() );

    //Extract the integration direction
    activeModelDVP = objectCommand->GetDataValuePair( "select viz quantity" );
    std::string intvecorscalr;
    activeModelDVP->GetData( intvecorscalr );
    
    vprDEBUG( vesDBG, 0 ) << "|\tselect scalar or volume flux for contour display"
        << std::endl << vprDEBUG_FLUSH;

    if( !intvecorscalr.compare( "scalarviz" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tVISUALIZE SCALARS"
            << std::endl << vprDEBUG_FLUSH;

        Selectscalarorvolflux( 0 );
    }
    else if( !intvecorscalr.compare( "volumefluxviz" ) )
    {
        vprDEBUG( vesDBG, 0 ) << "|\t\tVISUALIZE VOLUME FLUX"
            << std::endl << vprDEBUG_FLUSH;

        Selectscalarorvolflux( 1 );
    }

    //Extract the plane position
    activeModelDVP = objectCommand->GetDataValuePair( "Position" );
    double planePosition;
    activeModelDVP->GetData( planePosition );
    SetRequestedValue( planePosition );

    activeModelDVP = objectCommand->GetDataValuePair( "Plane Option" );
    if( activeModelDVP )
    {
        std::string preCalculatedFlag;
        activeModelDVP->GetData( preCalculatedFlag );

        if( preCalculatedFlag == "Use Nearest Precomputed Plane" )
        {
            SetPreCalcFlag( true );
        }
    }
    else
    {
        SetPreCalcFlag( false );
    }

    //Extract the advanced settings from the commands
    activeModelDVP = objectCommand->GetDataValuePair( "Advanced Scalar Settings" );
    objectCommand = boost::dynamic_pointer_cast<ves::open::xml::Command>( activeModelDVP->GetDataXMLObject() );

    // set the opacity
    activeModelDVP = objectCommand->GetDataValuePair( "Contour Opacity" );
    double opacity;
    activeModelDVP->GetData( opacity );
    contourOpacity = opacity * 0.01f;

    // set the warped contour scale
    activeModelDVP = objectCommand->GetDataValuePair( "Warped Contour Scale" );
    double contourScale;
    activeModelDVP->GetData( contourScale );
    double v[2];
    this->GetActiveDataSet()->GetUserRange( v );
    //double scale = contourScale;
    this->warpedContourScale = ( contourScale / 5.0 ) * 2.0f / ( float )( v[1] - v[0] );
    vprDEBUG( vesDBG, 0 ) << "Warped Contour Scale "
        << warpedContourScale << " : " << v[1] << " - " << v[0]
        << std::endl << vprDEBUG_FLUSH;
    
    // Set the lod values
    activeModelDVP = objectCommand->GetDataValuePair( "Contour LOD" );
    double contourLOD;
    activeModelDVP->GetData( contourLOD );
    double lod = contourLOD;
    double realLOD = lod * 0.01f;
    vprDEBUG( vesDBG, 0 ) << "CHANGE_CONTOUR_SETTINGS LOD Settings"
    << contourLOD << " : " << lod << " : " << realLOD
    << std::endl << vprDEBUG_FLUSH;
    this->deci->SetTargetReduction( realLOD );

    activeModelDVP = objectCommand->GetDataValuePair( "Type" );
    std::string contourType;
    activeModelDVP->GetData( contourType );

    if( contourType == "Graduated" )
    {
        SetFillType( 0 );
    }
    else if( contourType == "Banded" )
    {
        SetFillType( 1 );
    }
    else if( contourType == "Lined" )
    {
        SetFillType( 2 );
    }

	//Extract the surface flag
    activeModelDVP = objectCommand->GetDataValuePair( "SURF Tools" );
    if( activeModelDVP )
	{
    	unsigned int surfFlag;
    	activeModelDVP->GetData( surfFlag );
		m_surfTools = surfFlag;
	}

}
//////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::Selectscalarorvolflux( int value )
{
    m_selectvolfluxorscalr = value;
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::SetFillType( const int type )
{
    if( -1 < type && type < 3 )
        fillType = type;
    else
    {
        vprDEBUG( vesDBG, 0 )
        << "cfdContourBase: requested fillType (" << type
        << ") is not available, using 0 instead"
        << std::endl << vprDEBUG_FLUSH;
        fillType = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::CreatePlane( void )
{
    if( !cuttingPlane )
    {
        cuttingPlane = new cfdCuttingPlane(
                           GetActiveDataSet()->GetBounds(),
                           xyz, numSteps );
    }

    // insure that we are using correct bounds for the given data set...
    cuttingPlane->SetBounds(
        GetActiveDataSet()->GetBounds() );
    cuttingPlane->Advance( requestedValue );
    	
    vtkCutter* tempCutter = vtkCutter::New();
    tempCutter->SetCutFunction( cuttingPlane->GetPlane() );
    tempCutter->SetInput( GetActiveDataSet()->GetDataSet() );
    tempCutter->Update();
    
    SetMapperInput( tempCutter->GetOutputPort(0) );	
	
	// code commented here prints out the vtp file for GUI
	// selected contour plane.
/*	
    vtkPolyData* originalContour = NULL; 
	if ( !tempCutter->GetOutput() ) vprDEBUG( vesDBG, 0 )
        << " tempCutter->GetOutput() is NULL " 
        << std::endl << vprDEBUG_FLUSH;
    originalContour = dynamic_cast< vtkPolyData* >( mapper->GetInput() );
	vtkXMLPolyDataWriter* writer = vtkXMLPolyDataWriter::New();
    writer->SetInput( originalContour );
    writer->SetDataModeToAscii();
    writer->SetFileName( "testsurf1.vtp" );
    writer->Write();
    writer->Delete();
*/	

	// The code below computes volume flux on the specified contour plane
	// and allows for display of vector direction (in or out) by using only
	// two colors for mapping

	if( m_selectvolfluxorscalr == 1 )
    {

		vtkPolyData * pd = dynamic_cast< vtkPolyData* >( mapper->GetInput() );

	    vtkPolyDataNormals* normalGen = vtkPolyDataNormals::New();
    	normalGen->SetInput( pd );
    	normalGen->Update();

		pd = normalGen->GetOutput();

    	vtkDataArray *normalsArray = pd->GetPointData()->GetNormals();
    	unsigned int n_points = pd->GetNumberOfPoints();

    	vtkDoubleArray* vol_flux_array = vtkDoubleArray::New();
    	vol_flux_array->SetNumberOfTuples(n_points);
    	vol_flux_array->SetName("VolumeFlux");
	
    	pd->Update();
	
    	vtkPointData *pointData = pd->GetPointData();
    	if (pointData==NULL)
    	{
    	    std::cout << " pd point data is null " << std::endl;
    	}
	
    	vtkPoints *points = pd->GetPoints();    
    	if (points==NULL)
    	{
    	    std::cout << " points are null " << std::endl;
    	}
	
    	vtkDataArray *vectorArray = pointData->GetVectors( GetActiveDataSet()->GetActiveVectorName().c_str() );
	
    	if (vectorArray==NULL)
    	{
    	    std::cout << " vectors are null " << std::endl;
    	}
	
    	if (normalsArray==NULL)
    	{
    	    std::cout << " normals are null " << std::endl;
    	}
	
		double normalVec[3], vectorVec[3], volume_flux;
	
		for(unsigned int i = 0; i < n_points; i++)
    	{
    	    vectorArray->GetTuple(i, vectorVec);
    	    normalsArray->GetTuple(i, normalVec);
    	    volume_flux = vectorVec[0]*normalVec[0]+vectorVec[1]*normalVec[1]+vectorVec[2]*normalVec[2];
    	    vol_flux_array->SetTuple1(i, volume_flux);
    	}
    	
    	pd->GetPointData()->SetScalars(vol_flux_array);

    	normalGen->Update();
	
		pd = normalGen->GetOutput();

	    vtkPointData * poind = pd->GetPointData();
		unsigned int numberofarrays = poind->GetNumberOfArrays();	

		// commented out the 'print out' of array info in the vtp file
		//    	std::cout
		//    	    << "|\tNum Arrays: " << numberofarrays
		//    	    << std::endl;
		//	
		//		std::vector<std::string> arrayNames1;
		//		for(unsigned int ii = 0; ii < numberofarrays; ii++)
		//		{
		//			arrayNames1.push_back(poind->GetArray(ii)->GetName());
		//			int dataTypeID1 = poind->GetArray(ii)->GetDataType();
		//			std::cout << "|\tArray" << ii << ": " << arrayNames1[ii] 
		//					  << " (type: " << dataTypeID1 << ")" << std::endl;
		//		}
			
    	mapper->SetInput( pd );
	
    	double range[ 2 ];
    	pd->GetPointData()->GetScalars( "VolumeFlux" )->GetRange( range );
	
    	vtkLookupTable* lut2;
    	lut2 = vtkLookupTable::New();
    	lut2->SetNumberOfColors( 2 );            //default is 256
    	lut2->SetHueRange( 2.0f / 3.0f, 0.0f );    //a blue-to-red scale
    	lut2->SetTableRange( range );
    	lut2->Build();
	
    	mapper->SetColorModeToMapScalars();
    	mapper->SetScalarRange( range );
    	mapper->SetLookupTable( lut2 );
    	mapper->SetScalarModeToUsePointFieldData();
    	mapper->UseLookupTableScalarRangeOn();
    	mapper->SelectColorArray( "VolumeFlux" );
		mapper->Update();
    	normalGen->Delete();
    	lut2->Delete();
	
    }

	delete cuttingPlane;
	cuttingPlane = NULL;
	
	tempCutter->Delete();	

}
////////////////////////////////////////////////////////////////////////////////
void cfdContourBase::CreateArbSurface()
{   
    //Need to set the active datasetname and get the position of the dataset
    Model* activeModel = ModelHandler::instance()->GetActiveModel();
    unsigned int i = activeModel->GetNumberOfCfdDataSets();
    vprDEBUG( vesDBG, 1 )
        << "|\tContour source GET_SURFACE_DATASET " << i
        << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 0 ) << "|\tContour source::SetActiveDataSet dataset = "
        << activeModel->GetCfdDataSet( i-1 )->GetFileName()
        << ", dcs = " << activeModel->GetCfdDataSet( i-1 )->GetDCS()
        << std::endl << vprDEBUG_FLUSH;

    int cfdType = activeModel->GetCfdDataSet( i-1 )->GetType();
    vprDEBUG( vesDBG, 1 ) << "|\tContour source::SetActiveDataSet cfdType: " << cfdType
        << std::endl << vprDEBUG_FLUSH;

    // set the dataset as the appropriate dastaset type
    // (and the active dataset as well)
    DataSet* surfDataset = activeModel->GetCfdDataSet( i-1 );
    vtkPolyData * pd = surfDataset->GetPolyData();

    vtkPointData * poind = pd->GetPointData();
	unsigned int numberofarrays = poind->GetNumberOfArrays();

	// commented out the 'print out' of array info in the vtp file
	//    std::cout
	//        << "|\tNum Arrays: " << numberofarrays
	//        << std::endl << vprDEBUG_FLUSH;

	//	std::cout << "|\tkey: \n";
	//	std::cout << VTK_UNSIGNED_CHAR << " unsigned char\n";
	//	std::cout << VTK_UNSIGNED_INT << " unsigned int\n";
	//	std::cout << VTK_FLOAT << " float\n";
	//	std::cout << VTK_DOUBLE << " double\n";

	//	std::vector<std::string> arrayNames;
	//	for(unsigned int i = 0; i < numberofarrays; i++)
	//	{
	//		arrayNames.push_back(poind->GetArray(i)->GetName());
	//		int dataTypeID = poind->GetArray(i)->GetDataType();
	//		std::cout << "|\tArray" << i << ": " << arrayNames[i] 
	//				  << " (type: " << dataTypeID << ")" << std::endl;
	//	}

	vtkProbeFilter *surfProbe= vtkCompositeDataProbeFilter::New();
    surfProbe->SetInput( pd );
    surfProbe->SetSource(GetActiveDataSet()->GetDataSet());
  
    surfProbe->Update(); 
  
    assert(surfProbe->GetPolyDataOutput()!=0);

 	pd = surfProbe->GetPolyDataOutput();

    if( m_selectvolfluxorscalr != 1 )
    {
    	normals->SetInput( pd );
    	normals->NonManifoldTraversalOn();
    	normals->AutoOrientNormalsOn();
    	normals->ConsistencyOn();
    	normals->SplittingOn();
    	normals->Update();
	
    	mapper->SetColorModeToMapScalars();
    	mapper->SetInput( normals->GetOutput() );
	
   		mapper->SetScalarModeToUsePointFieldData();
    	mapper->UseLookupTableScalarRangeOn();
    	mapper->SelectColorArray( GetActiveDataSet()->
    	    	GetActiveScalarName().c_str() );
    	mapper->SetLookupTable( GetActiveDataSet()->GetLookupTable() );
		mapper->Update();
	}
    else
    { 

	// The code below computes volume flux on the specified contour plane

	    normals->SetInput( pd );
	    normals->Update();
	
		pd = normals->GetOutput();

    	vtkDataArray *normalsArray = pd->GetPointData()->GetNormals();
    	unsigned int n_points = pd->GetNumberOfPoints();
	
    	vtkDoubleArray* vol_flux_array = vtkDoubleArray::New();
    	vol_flux_array->SetNumberOfTuples(n_points);
    	vol_flux_array->SetName("VolumeFlux");
	
    	pd->Update();
	
    	vtkPointData *pointData = pd->GetPointData();
    	if (pointData==NULL)
    	{
    	    std::cout << " pd point data is null " << std::endl;
    	}
	
    	vtkPoints *points = pd->GetPoints();    
    	if (points==NULL)
    	{
    	    std::cout << " points are null " << std::endl;
    	}
	
    	vtkDataArray *vectorArray = pointData->GetVectors( GetActiveDataSet()->GetActiveVectorName().c_str() );
	
    	if (vectorArray==NULL)
    	{
    	    std::cout << " vectors are null " << std::endl;
    	}
	
    	if (normalsArray==NULL)
    	{
    	    std::cout << " normals are null " << std::endl;
    	}
	
		double normalVec[3], vectorVec[3], volume_flux;
	
		for(i = 0; i < n_points; i++)
    	{
    	    vectorArray->GetTuple(i, vectorVec);
    	    normalsArray->GetTuple(i, normalVec);
    	    volume_flux = vectorVec[0]*normalVec[0]+vectorVec[1]*normalVec[1]+vectorVec[2]*normalVec[2];
    	    vol_flux_array->SetTuple1(i, volume_flux);
    	}
    	
    	pd->GetPointData()->SetScalars(vol_flux_array);

    	normals->Update();
		pd = normals->GetOutput();
	
    	poind = pd->GetPointData();
		numberofarrays = poind->GetNumberOfArrays();

		// commented out the 'print out' of array info in the vtp file
		//    	std::cout
		//    	    << "|\tNum Arrays: " << numberofarrays
		//    	    << std::endl;
		//	
		//		std::vector<std::string> arrayNames1;
		//		for(unsigned int ii = 0; ii < numberofarrays; ii++)
		//		{
		//			arrayNames1.push_back(poind->GetArray(ii)->GetName());
		//			int dataTypeID1 = poind->GetArray(ii)->GetDataType();
		//			std::cout << "|\tArray" << ii << ": " << arrayNames1[ii] 
		//					  << " (type: " << dataTypeID1 << ")" << std::endl;
		//		}
			
	
    	mapper->SetInput( pd );
	
    	double range[ 2 ];
    	pd->GetPointData()->GetScalars( "VolumeFlux" )->GetRange( range );
	
    	vtkLookupTable* lut1;
    	lut1 = vtkLookupTable::New();
    	lut1->SetNumberOfColors( 256 );            //default is 256
    	lut1->SetHueRange( 2.0f / 3.0f, 0.0f );    //a blue-to-red scale
    	lut1->SetTableRange( range );
    	lut1->Build();
	
    	mapper->SetColorModeToMapScalars();
    	mapper->SetScalarRange( range );
    	mapper->SetLookupTable( lut1 );
    	mapper->SetScalarModeToUsePointFieldData();
    	mapper->UseLookupTableScalarRangeOn();
    	mapper->SelectColorArray( "VolumeFlux" );
		mapper->Update();
    	lut1->Delete();
	
	}

}
//////////////////////////////////////////////////////////////////////////////////
