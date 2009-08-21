// Timothy Gundere July 9, 2009

// --- Header Includes --- //
#include "ShowSurfaceFilter.h"

// --- VTK Includes --- //
#include <vtkUnstructuredGrid.h>
#include <vtkPointData.h>
#include <vtkPolyDataMapper.h>
#include <vtkPolyDataNormals.h>
#include <vtkActor.h>
#include <vtkGeometryFilter.h>
#include <vtkProperty.h>

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
ShowSurfaceFilter::ShowSurfaceFilter()
{
    ;
}

//Another Constructor
ShowSurfaceFilter::ShowSurfaceFilter( vtkUGDataSet* inputDataSet, ves::xplorer::scenegraph::DCS* parentDCS )
    :
    mDataSet( inputDataSet )
{
    mPluginDCS = parentDCS;

    vtkGeometryFilter* _filter = vtkGeometryFilter::New();
    _filter->SetInput( mDataSet->GetUGData() );
    
    vtkPolyDataNormals* _normals = vtkPolyDataNormals::New();
    _normals->SetInputConnection( 0, _filter->GetOutputPort(0));
    
    vtkPolyDataMapper* _polyMapper = vtkPolyDataMapper::New();
    _polyMapper->SetInputConnection( 0, _normals->GetOutputPort(0));
    _polyMapper->ScalarVisibilityOff();
    _polyMapper->Update();
    
    vtkActor* _actor = vtkActor::New();
    _actor->SetMapper( _polyMapper );
    _actor->GetProperty()->SetRepresentationToWireframe(); //Doesnt work?
    _actor->GetProperty()->SetColor( 1.0, 0, 0 );
    
    double* actorColor = _actor->GetProperty()->GetColor();
    std::cout << actorColor[0] << actorColor[1] << actorColor[2] << std::endl;
   
    mGeode = new ves::xplorer::scenegraph::Geode();
    mGeode->TranslateToGeode( _actor );
    
    mPluginDCS->AddChild( mGeode ); //Adds the vizualization to the Xplorer window
    
    //Delete vtk Objects
    _filter->Delete();
    _normals->Delete();
    _polyMapper->Delete();
    _actor->Delete();

    std::cout << "DONE!" <<std::endl;
}

// Destructor ////////////////////////////////////
ShowSurfaceFilter::~ShowSurfaceFilter()
{
    //delete mGeode;
}
