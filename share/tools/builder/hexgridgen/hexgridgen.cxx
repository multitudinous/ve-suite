#include <vector>
#include <utility>
#include <iostream>
#include <cstdlib>

#include <vtkXMLDataSetWriter.h>
#include <vtkPolyData.h>
#include <vtkPoints.h>
#include <vtkPolygon.h>

void MakeCenters();
void MakeCells();
void MakePoints( std::pair< double, double > centerPoint );
// center points
std::vector< std::pair< double, double > > centers;
//all the points
std::vector< std::pair< double, double > > points;
//polydata of the grid
vtkPolyData* hexGrid;
//points
vtkPoints* polygonPoints;
//Get number of columns
unsigned int columns = 5;
//Get number of rows
unsigned int rows = 3;
//Get radius
double radius = 1.0f;
//pointCounter
unsigned int pointCounter = 0;

int main( int argc, char* argv[] )
{
    columns = std::atoi( argv[ 1 ] );
    rows = std::atoi( argv[ 2 ] );

    //Make list of center points
    MakeCenters();
    //Make points from center points
    for( std::vector< std::pair< double, double > >::iterator 
        iter = centers.begin(); iter != centers.end(); ++iter )
    {
        MakePoints( *iter );
    }
    //Make grid
    hexGrid = vtkPolyData::New();
    //Create cells from points
    MakeCells();
    //Add scalars
    //Write out file
    vtkXMLDataSetWriter* writer = vtkXMLDataSetWriter::New();
    writer->SetDataModeToBinary();
    writer->SetFileName( "hexgrid.vtp" );
    writer->SetInput( hexGrid );
    writer->Write();
    
    writer->Delete();
    hexGrid->Delete();
    polygonPoints->Delete();
    return 1;
}

void MakePoints( std::pair< double, double > centerPoint )
{
    unsigned int counter = 0;
    double halfRadius = radius * 0.5;
    double height = 
        sqrt( ( radius * radius ) - ( halfRadius * halfRadius ) );
    
    double x1 = centerPoint.first - halfRadius;
    double y1 = centerPoint.second + height;
    points.push_back( std::make_pair< double, double >( x1, y1 ) );
    counter += 1;

    double x2 = centerPoint.first - radius;
    double y2 = centerPoint.second;
    points.push_back( std::make_pair< double, double >( x2, y2 ) );
    counter += 1;

    double x3 = x1; 
    double y3 = centerPoint.second - height;
    points.push_back( std::make_pair< double, double >( x3, y3 ) );
    counter += 1;

    double x4 = centerPoint.first + halfRadius;
    double y4 = y3;
    points.push_back( std::make_pair< double, double >( x4, y4 ) );
    counter += 1;

    double x5 = centerPoint.first + radius;
    double y5 = centerPoint.second;
    points.push_back( std::make_pair< double, double >( x5, y5 ) );
    counter += 1;

    double x6 = x4;
    double y6 = y1;
    points.push_back( std::make_pair< double, double >( x6, y6 ) );
    counter += 1;
}

void MakeCenters()
{
    double x0 = 0;
    double y0 = 0;
    double x = 0;
    double y = 0;
    double halfRadius = radius * 0.5;
    double height = 
        sqrt( ( radius * radius ) - ( halfRadius * halfRadius ) );

    for( size_t j = 0; j < rows; ++j )
    {
        for( size_t i = 0; i < columns; ++i )
        {
            x = x0 + ( i * ( radius + halfRadius ) );
            y = y0 + ( j * ( height * 2 ) ) + ( ( i % 2 ) * height );
            centers.push_back( std::make_pair< double, double >( x, y ) );
        }
    }
}

void MakeCells()
{
    polygonPoints = vtkPoints::New();
    polygonPoints->SetNumberOfPoints( points.size() );
    for( size_t i = 0; i < points.size(); ++i )
    {
        polygonPoints->InsertPoint(i, points.at(i).first, points.at(i).second, 0);
    }
    
    hexGrid->Allocate( centers.size(), centers.size() );
    hexGrid->SetPoints( polygonPoints );
        
    for( size_t i = 0; i < points.size(); )
    {
        vtkPolygon* aPolygon = vtkPolygon::New();
        aPolygon->GetPointIds()->SetNumberOfIds( 6 );
        aPolygon->GetPointIds()->SetId( 0, i++ );
        aPolygon->GetPointIds()->SetId( 1, i++ );
        aPolygon->GetPointIds()->SetId( 2, i++ );
        aPolygon->GetPointIds()->SetId( 3, i++ );
        aPolygon->GetPointIds()->SetId( 4, i++ );
        aPolygon->GetPointIds()->SetId( 5, i++ );
        hexGrid->InsertNextCell( aPolygon->GetCellType(), aPolygon->GetPointIds() );
        aPolygon->Delete();
    }
}
