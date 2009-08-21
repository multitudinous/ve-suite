// Timothy Gundere July 29, 2009

// --- Header Includes --- //
#include "vtkUGDataSet.h"

// --- VTK Includes --- //
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkDataArraySelection.h>
#include <vtkPointData.h>
#include <vtkUnstructuredGrid.h>

// --- This Plugin Includes --- //
#include "PolyVisualizationBase.h"

// --- C++ Includes --- ///
#include <iostream>

using namespace vascularsimviewer;

//Constructor
vtkUGDataSet::vtkUGDataSet( const std::string& infilename, std::vector< std::string > arrays )
    :
    _activeArrays( arrays ),
    _reader( vtkXMLUnstructuredGridReader::New() ),
    _ArraySelector( _reader->GetPointDataArraySelection() ),
    mFilename( infilename ),
    _UGData( vtkUnstructuredGrid::New() )
{
	_reader->SetFileName( mFilename.c_str() );
    _reader->Update();  //If not Updated here, the enabling/disabling of arrays doesn't work
   
    _ArraySelector->DisableAllArrays();
    
    int i;
    //Loop enables arrays indicated by the conductor plugin
    for( i=0; i<_activeArrays.size(); i++ )
    {
        std::cout << "Passed arrays are: " << arrays[i] << std::endl;
        _ArraySelector->EnableArray( _activeArrays[i].c_str() );
    }
    _reader->Update(); //Need to update again before the output of the reader is read
    
    _UGData = _reader->GetOutput();
    
    //A debug check
    std::cout << "No of Arrays in UG is: " << _UGData->GetPointData()->GetNumberOfArrays() << std::endl;
}

//Destructor
vtkUGDataSet::~vtkUGDataSet()
{
    _reader->Delete();

    //Write loop to delete all visualizatios;
}

////////////////////////////////////////////
void vtkUGDataSet::UpdateArrays( std::vector< std::string > array )
{
    _activeArrays.clear();
    _reader->GetPointDataArraySelection()->DisableAllArrays();

    int i;
    for( i=0; i<array.size(); i++ )
    {
        _activeArrays.push_back( std::string ( array[i] ) );
        
        _reader->GetPointDataArraySelection()->EnableArray( _activeArrays.back().c_str() );
    }

    _reader->Update();
    _UGData = _reader->GetOutput();
}

////////////////////////////////////////////
const std::string& vtkUGDataSet::GetFilename()
{
    return mFilename;
}

///////////////////////////////////////
void vtkUGDataSet::AddVisualization( PolyVisualizationBase* inPolyViz )
{
    _derivedVisualizations.push_back( inPolyViz ); 
}

////////////////////////////////////////
vtkUnstructuredGrid* vtkUGDataSet::GetUGData()
{
    return _UGData;
}
