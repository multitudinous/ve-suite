#include <ves/xplorer/data/ContourPlanePropertySet.h>
#include <ves/xplorer/data/Property.h>

#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/DataSet.h>

#include <boost/bind.hpp>

using namespace ves::xplorer::data;

ContourPlanePropertySet::ContourPlanePropertySet( )
{
    mTableName = "Contour_Plane";
    _createSkeleton( );
}

ContourPlanePropertySet::ContourPlanePropertySet( const ContourPlanePropertySet& orig )
{
}

ContourPlanePropertySet::~ContourPlanePropertySet( )
{
}

void ContourPlanePropertySet::_createSkeleton( )
{
    AddProperty( "DataSet", 0, "Data Set" );
    PSVectorOfStrings enumValues;

    AddProperty( "DataSet_ScalarData", 0, "Scalar Data" );
    // Dummy value to ensure this gets set up as an enum
    enumValues.push_back( "Select Scalar Data" );
    SetPropertyAttribute( "DataSet_ScalarData", "enumValues", enumValues );
    mPropertyMap["DataSet"]->SignalValueChanged.connect( boost::bind( &ContourPlanePropertySet::UpdateScalarDataOptions, this, _1 ) );

    AddProperty( "DataSet_ScalarRange", boost::any( ), "Scalar Range" );
    SetPropertyAttribute( "DataSet_ScalarRange", "isUIGroupOnly", true );
    SetPropertyAttribute( "DataSet_ScalarRange", "setExpanded", true );

    AddProperty( "DataSet_ScalarRange_Min", 0.0, "Min" );
    mPropertyMap["DataSet_ScalarRange_Min"]->SetDisabled( );

    AddProperty( "DataSet_ScalarRange_Max", 1.0, "Max" );
    mPropertyMap["DataSet_ScalarRange_Max"]->SetDisabled( );

    mPropertyMap["DataSet_ScalarData"]->SignalValueChanged.connect( boost::bind( &ContourPlanePropertySet::UpdateScalarDataRange, this, _1 ) );
    mPropertyMap["DataSet_ScalarRange_Min"]->SignalRequestValidation.connect( boost::bind( &ContourPlanePropertySet::ValidateScalarMinMax, this, _1, _2 ) );
    mPropertyMap["DataSet_ScalarRange_Max"]->SignalRequestValidation.connect( boost::bind( &ContourPlanePropertySet::ValidateScalarMinMax, this, _1, _2 ) );

    // Now that DataSet subproperties exist, we can initialize the values in
    // the dataset enum. If we had tried to do this beforehand, none of the
    // connections between DataSet and its subproperties would have been in
    // place yet.
    enumValues.clear( );
    Model* model = NULL;
    model = ModelHandler::instance( )->GetActiveModel( );

    if( model )
    {
        int numSets = model->GetNumberOfCfdDataSets( );
        for ( int index = 0; index < numSets; index++ )
        {
            enumValues.push_back( model->GetCfdDataSet( index )->GetFileName( ) );
        }
    }
    else
    {
        std::cout << "No active model." << std::endl;
        enumValues.push_back( "No datasets loaded" );
    }
    SetPropertyAttribute( "DataSet", "enumValues", enumValues );
    UpdateScalarDataOptions( 0 );

    AddProperty( "Direction", 0, "Direction" );
    enumValues.clear( );
    enumValues.push_back( "x" );
    enumValues.push_back( "y" );
    enumValues.push_back( "z" );
    enumValues.push_back( "By Wand" );
    SetPropertyAttribute( "Direction", "enumValues", enumValues );

    AddProperty( "Mode", 0, "Mode" );
    enumValues.clear( );
    enumValues.push_back( "Specify a Single Plane" );
    enumValues.push_back( "Use All Precomputed Surfaces" );
    SetPropertyAttribute( "Mode", "enumValues", enumValues );
    // Connect SignalValueChanged of "Mode" to a function that enables and disables
    // its sub-properties as appropriate
    Property* mode = mPropertyMap["Mode"];
    if( mode )
    {
        mode->SignalValueChanged.connect( boost::bind( &ContourPlanePropertySet::UpdateModeOptions, this, _1 ) );
    }

    AddProperty( "Mode_UseNearestPrecomputedPlane", false, "Use Nearest Precomputed Plane" );

    AddProperty( "Mode_CyclePrecomputedSurfaces", false, "Cycle Precomputed Surfaces" );
    // We disable this one by default since the selected Mode,
    // "Specify a Single Plane", does not support this option.
    mPropertyMap["Mode_CyclePrecomputedSurfaces"]->SetDisabled( );


    AddProperty( "PlaneLocation", 0.00, "Plane Location" );
    SetPropertyAttribute( "PlaneLocation", "minimumValue", 0.00 );
    SetPropertyAttribute( "PlaneLocation", "maximumValue", 100.00 );

    AddProperty( "Advanced", boost::any( ), "Advanced" );
    SetPropertyAttribute( "Advanced", "isUIGroupOnly", true );

    AddProperty( "Advanced_Opacity", 100, "Opacity" );
    SetPropertyAttribute( "Advanced_Opacity", "minimumValue", 0 );
    SetPropertyAttribute( "Advanced_Opacity", "maximumValue", 100 );

    AddProperty( "Advanced_WarpedContourScale", 100, "Warped Contour Scale" );
    SetPropertyAttribute( "Advanced_WarpedContourScale", "minimumValue", 0 );
    SetPropertyAttribute( "Advanced_WarpedContourScale", "maximumValue", 100 );

    AddProperty( "Advanced_ContourLOD", 100, "Contour LOD" );
    SetPropertyAttribute( "Advanced_ContourLOD", "minimumValue", 0 );
    SetPropertyAttribute( "Advanced_ContourLOD", "maximumValue", 100 );

    // This demonstrates doing extra processing when a value is
    // changed. Similar operations could also occur inside a validator. In this
    // case, the value is initially allowed to change but is then forcibly reset
    //Property* temp = mPropertyMap["Advanced_ContourLOD"];
    //if( temp )
    //{
    //  temp->SignalValueChanged.connect( boost::bind( &ContourPlanePropertySet::LockIntToZero, this, _1 ) );
    //}

    AddProperty( "Advanced_ContourType", 0, "Contour Type" );
    enumValues.clear( );
    enumValues.push_back( "Graduated" );
    enumValues.push_back( "Banded" );
    enumValues.push_back( "Lined" );
    SetPropertyAttribute( "Advanced_ContourType", "enumValues", enumValues );

    AddProperty( "Advanced_WarpOption", false, "Warp Option" );
}

void ContourPlanePropertySet::UpdateScalarDataOptions( Property* property )
{
    PSVectorOfStrings enumValues;
    Model* model = NULL;
    model = ModelHandler::instance( )->GetActiveModel( );
    if( model )
    {
        int setNum = boost::any_cast<int>( GetPropertyValue( "DataSet" ) );
        DataSet* dSet = model->GetCfdDataSet( setNum );
        for ( int index = 0; index < dSet->GetNumberOfScalars( ); index++ )
        {
            enumValues.push_back( dSet->GetScalarName( index ) );
        }
    }
    else
    {
        enumValues.push_back( "No dataset loaded" );
    }
    SetPropertyAttribute( "DataSet_ScalarData", "enumValues", enumValues );
    UpdateScalarDataRange( 0 );
}

void ContourPlanePropertySet::UpdateScalarDataRange( Property* property )
{
    Model* model = NULL;
    model = ModelHandler::instance( )->GetActiveModel( );
    if( model )
    {
        mPropertyMap["DataSet_ScalarRange_Min"]->SetEnabled( );
        mPropertyMap["DataSet_ScalarRange_Max"]->SetEnabled( );

        int index = boost::any_cast<int>( GetPropertyValue( "DataSet" ) );
        DataSet* dSet = model->GetCfdDataSet( index );
        index = boost::any_cast<int>( GetPropertyValue( "DataSet_ScalarData" ) );
        double * range = dSet->GetActualScalarRange( index );
        
        // Update the upper and lower bounds of Min and Max first so that 
        // boundary values will be allowed!
        SetPropertyAttribute( "DataSet_ScalarRange_Min", "minimumValue", range[0] );
        SetPropertyAttribute( "DataSet_ScalarRange_Min", "maximumValue", range[1] );
        SetPropertyAttribute( "DataSet_ScalarRange_Max", "minimumValue", range[0] );
        SetPropertyAttribute( "DataSet_ScalarRange_Max", "maximumValue", range[1] );

        // Set min and max to the lower and upper boundary values, respectively
        SetPropertyValue( "DataSet_ScalarRange_Min", range[0] );
        SetPropertyValue( "DataSet_ScalarRange_Max", range[1] );
    }
}

void ContourPlanePropertySet::LockIntToZero( Property* property )
{
    property->SetValue( 0 );
}

void ContourPlanePropertySet::UpdateModeOptions( Property* property )
{
    // Make sure the main value is an int as it should be
    if( property->IsInt( ) )
    {
        int value = boost::any_cast<int>( property->GetValue( ) );
        if( value == 0 ) // "Specify a Single Plane"
        {
            mPropertyMap["Mode_UseNearestPrecomputedPlane"]->SetEnabled( );
            mPropertyMap["Mode_CyclePrecomputedSurfaces"]->SetDisabled( );
        }
        else
        {
            mPropertyMap["Mode_UseNearestPrecomputedPlane"]->SetDisabled( );
            mPropertyMap["Mode_CyclePrecomputedSurfaces"]->SetEnabled( );
        }
    }
}

bool ContourPlanePropertySet::ValidateScalarMinMax( Property* property, boost::any value )
{
    Property* min = mPropertyMap["DataSet_ScalarRange_Min"];
    Property* max = mPropertyMap["DataSet_ScalarRange_Max"];

    double castMin, castMax;

    if( property == min )
    {
        castMin = boost::any_cast<double>( value );
        castMax = boost::any_cast<double>( max->GetValue( ) );
    }
    else
    {
        castMin = boost::any_cast<double>( min->GetValue( ) );
        castMax = boost::any_cast<double>( value );
    }

    if( castMin < castMax )
    {
        return true;
    }
    else
    {
        return false;
    }
}


