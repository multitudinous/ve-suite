// Timothy Gundere July 9, 2009

// --- Header Includes --- //
#include "wssMagSurfFilter.h"

// --- VTK Includes --- //
#include <vtkUnstructuredGrid.h>
#include <vtkPointData.h>
#include <vtkPolyDataMapper.h>
#include <vtkPolyDataNormals.h>
#include <vtkActor.h>
#include <vtkGeometryFilter.h>
#include <vtkArrayCalculator.h>
#include <vtkLookupTable.h>

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/Geode.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/vtkActorToOSG.h>

// --- This Plugin Includes --- //
#include "vtkUGDataSet.h"

// --- C++ Includes --- //
#include <iostream> //I include this for debugging

using namespace vascularsimviewer;

// Constructor ////////////////////////////////////
wssMagSurfFilter::wssMagSurfFilter()
{
    ;
}

//Another Constructor
wssMagSurfFilter::wssMagSurfFilter( vtkUGDataSet* inputDataSet, ves::xplorer::scenegraph::DCS* parentDCS )
    :
    mDataSet( inputDataSet )
{
    mPluginDCS = parentDCS;

    //std::string _activeVector = "wss";
    
    // Check
    std::cout << "No. Arrays before calc: " << mDataSet->GetUGData()->GetPointData()->GetNumberOfArrays() << std::endl;
    
    //Start of vtk filter pipeline
    vtkArrayCalculator* _calc = vtkArrayCalculator::New();
    _calc->AddVectorArrayName( "wss" );
    _calc->SetFunction("mag(wss)*10");
    _calc->SetResultArrayName("wss_mag");
    _calc->SetInput( mDataSet->GetUGData() );
    _calc->Update();
    
    // Check
    std::cout << "No. Arrays after calc: " << _calc->GetUnstructuredGridOutput()->GetPointData()->GetNumberOfArrays() << std::endl;
    int i;
    std::cout << "Arrays Names:" << std::endl;
    for( i=0; i<_calc->GetUnstructuredGridOutput()->GetPointData()->GetNumberOfArrays(); i++)
    {
        std::cout << "    " << _calc->GetUnstructuredGridOutput()->GetPointData()->GetArrayName(i) << std::endl;
    }
    
    double _wssRange[2];
    _calc->GetUnstructuredGridOutput()->GetPointData()->GetArray( "wss_mag" )->GetRange( _wssRange );
    
    std::cout << "WSS min: " << _wssRange[0] << std::endl;
    std::cout << "WSS max: " << _wssRange[1] << std::endl;
    
    vtkGeometryFilter* _filter = vtkGeometryFilter::New();
    _filter->SetInputConnection( _calc->GetOutputPort(0) );

    _filter->Update();   
    vtkPolyDataNormals* _normals = vtkPolyDataNormals::New();
    _normals->SetInputConnection( 0, _filter->GetOutputPort(0));
    
    vtkLookupTable* _lut = vtkLookupTable::New();
    _lut->SetNumberOfColors( 256 );
    _lut->SetHueRange( 2.0f / 3.0f, 0.0f );    //a blue-to-red scale
    _lut->SetTableRange( 0, 30 );
    _lut->Build();
    
    vtkPolyDataMapper* _polyMapper = vtkPolyDataMapper::New();
    _polyMapper->SetInputConnection( 0, _normals->GetOutputPort(0));
    _polyMapper->SetLookupTable( _lut );
    _polyMapper->UseLookupTableScalarRangeOn(); //Could also set a scalar range for the mapper here
    _polyMapper->SelectColorArray( "wss_mag" );
    _polyMapper->Update();
        
    vtkActor* _actor = vtkActor::New();
    _actor->SetMapper( _polyMapper );
    
    mGeode = new ves::xplorer::scenegraph::Geode();
    mGeode->TranslateToGeode( _actor );
    
    mPluginDCS->AddChild( mGeode );
    
    
    
    //Delete vtk Objects
    _calc->Delete();
 //   _calcOut->Delete();
    _filter->Delete();
    _normals->Delete();
    _lut->Delete();
    _polyMapper->Delete();
    _actor->Delete();

    std::cout << "DONE!" <<std::endl;
}

// Destructor ////////////////////////////////////
wssMagSurfFilter::~wssMagSurfFilter()
{
    //delete mGeode;
}
