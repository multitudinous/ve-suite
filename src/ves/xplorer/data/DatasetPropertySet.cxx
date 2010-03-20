#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/data/Property.h>

using namespace ves::xplorer::data;

DatasetPropertySet::DatasetPropertySet( )
{
    _createSkeleton( );
}

DatasetPropertySet::DatasetPropertySet( const DatasetPropertySet& orig )
{
}

DatasetPropertySet::~DatasetPropertySet( )
{
}

void DatasetPropertySet::_createSkeleton( )
{
    AddProperty( "SurfaceWrap", false, "Surface Wrap" );

    AddProperty( "BoundingBox", false, "Bounding Box" );

    AddProperty( "ScalarBar", false, "Scalar Bar" );

    AddProperty( "Axes", false, "Axes" );
    SetPropertyAttribute( "Axes", "setExpanded", false );

    AddProperty( "Axes.XLabel", std::string( "X Axis" ), "X axis label" );

    AddProperty( "Axes.YLabel", std::string( "Y Axis" ), "Y axis label" );

    AddProperty( "Axes.ZLabel", std::string( "Z Axis" ), "Z axis label" );
}

