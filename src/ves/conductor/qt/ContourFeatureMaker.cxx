#include <ves/conductor/qt/ContourFeatureMaker.h>

#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/ContourPlanePropertySet.h>
#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <QtGui/QMessageBox>

#include <boost/any.hpp>

using namespace ves::conductor;

ContourFeatureMaker::ContourFeatureMaker( )
{
}
////////////////////////////////////////////////////////////////////////////////

ContourFeatureMaker::ContourFeatureMaker( const ContourFeatureMaker& orig )
{
}
////////////////////////////////////////////////////////////////////////////////

ContourFeatureMaker::~ContourFeatureMaker( )
{
}
////////////////////////////////////////////////////////////////////////////////

void ContourFeatureMaker::update( const std::string& dbFile, unsigned int recordID )
{
    // For now we won't worry about how to discover an existing plane that needs
    // to be deleted, moved, etc. We will just create a new one
    xplorer::data::ContourPlanePropertySet contourSet;
    contourSet.SetRecordID( recordID );
    contourSet.LoadFromDatabase( dbFile );
    _addPlane( static_cast < xplorer::data::PropertySet& > ( contourSet ) );
}
////////////////////////////////////////////////////////////////////////////////

void ContourFeatureMaker::_updateContourInformation( xplorer::data::PropertySet& set )
{
    _contourInformation.clear( );

    // Plane direction
    ves::open::xml::DataValuePairPtr contourDirection( new ves::open::xml::DataValuePair( ) );
    contourDirection->SetDataType( "STRING" );
    contourDirection->SetDataName( std::string( "Direction" ) );
    std::string value = boost::any_cast<std::string > ( set.GetPropertyAttribute( "Direction", "enumCurrentString" ) );
    contourDirection->SetDataString( value );
    _contourInformation.push_back( contourDirection );

    // Mode: Single or Multiple
    ves::open::xml::DataValuePairPtr numberOfPlanes( new ves::open::xml::DataValuePair( ) );
    numberOfPlanes->SetDataType( "STRING" );
    numberOfPlanes->SetDataName( std::string( "Number of Planes" ) );
    std::string _numberOfPlanesOption( "Single" );
    int mode = boost::any_cast<int>( set.GetPropertyValue( "Mode" ) );
    if( mode == 0 )
    {
        _numberOfPlanesOption = "Single";
    }
    else if( mode == 1 )
    {
        _numberOfPlanesOption = "Multiple";
    }
    numberOfPlanes->SetDataString( _numberOfPlanesOption );
    _contourInformation.push_back( numberOfPlanes );

    // Plane location
    ves::open::xml::DataValuePairPtr planePosition( new ves::open::xml::DataValuePair( ) );
    planePosition->SetData( "Position", boost::any_cast<double>( set.GetPropertyValue( "PlaneLocation" ) ) );
    _contourInformation.push_back( planePosition );

    // Use Nearest or Cycle Precomputed
    std::string _planeOption( "" );
    if( boost::any_cast<bool>( set.GetPropertyValue( "Mode_UseNearestPrecomputedPlane" ) ) )
    {
        _planeOption = "Use Nearest Precomputed Plane";
    }
    else if( boost::any_cast<bool>( set.GetPropertyValue( "Mode_CyclePrecomputedSurfaces" ) ) )
    {
        _planeOption = "Cycle Precomputed Surfaces";
    }

    if( !_planeOption.empty( ) )
    {
        ves::open::xml::DataValuePairPtr planeOption( new ves::open::xml::DataValuePair( ) );
        planeOption->SetDataType( "STRING" );
        planeOption->SetDataName( std::string( "Plane Option" ) );
        planeOption->SetDataString( _planeOption );

        _contourInformation.push_back( planeOption );
    }
}
////////////////////////////////////////////////////////////////////////////////

// This should go in a base class that queries for Advanced_ type properties
// and makes all the appropriate settings
void ContourFeatureMaker::_updateAdvancedSettings( xplorer::data::PropertySet& set )
{
    _advancedSettings.clear( );

    ves::open::xml::DataValuePairPtr contourOpacity( new ves::open::xml::DataValuePair( ) );
    contourOpacity->SetData( "Contour Opacity",
                             static_cast<unsigned int>(boost::any_cast<int>
                             ( set.GetPropertyValue( "Advanced_Opacity" ) ) ) );
    _advancedSettings.push_back( contourOpacity );

    ves::open::xml::DataValuePairPtr warpedScale( new ves::open::xml::DataValuePair( ) );
    warpedScale->SetData( "Warped Contour Scale",
                          static_cast<unsigned int>(boost::any_cast<int>
                          ( set.GetPropertyValue( "Advanced_WarpedContourScale" ) ) ) );
    _advancedSettings.push_back( warpedScale );

    ves::open::xml::DataValuePairPtr LODSetting( new ves::open::xml::DataValuePair( ) );
    LODSetting->SetData( "Contour LOD",
                         static_cast<unsigned int>(boost::any_cast<int>
                         ( set.GetPropertyValue( "Advanced_ContourLOD" ) ) ) );
    _advancedSettings.push_back( LODSetting );

    ves::open::xml::DataValuePairPtr contourType( new ves::open::xml::DataValuePair( ) );
    contourType->SetDataType( "STRING" );
    contourType->SetDataName( std::string( "Type" ) );
    std::string _planeType = boost::any_cast<std::string >
            ( set.GetPropertyAttribute
            ( "Advanced_ContourType", "enumCurrentString" ) );
    contourType->SetDataString( _planeType );
    _advancedSettings.push_back( contourType );

    ves::open::xml::DataValuePairPtr warpOptionFlag( new ves::open::xml::DataValuePair( ) );
    warpOptionFlag->SetDataName( "Warp Option" );
    warpOptionFlag->SetDataType( "UNSIGNED INT" );
    if( boost::any_cast<bool>( set.GetPropertyValue( "Advanced_WarpOption" ) ) )
    {
        warpOptionFlag->SetDataValue( static_cast < unsigned int > ( 1 ) );
    }
    else
    {
        warpOptionFlag->SetDataValue( static_cast < unsigned int > ( 0 ) );
    }
    _advancedSettings.push_back( warpOptionFlag );

}
////////////////////////////////////////////////////////////////////////////////

void ContourFeatureMaker::_addPlane( xplorer::data::PropertySet& set )
{
    _updateContourInformation( set );
    _updateAdvancedSettings( set );

    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command( ) );

    newCommand->SetCommandName( "UPDATE_SCALAR_SETTINGS" );

    for ( size_t i = 0; i < _contourInformation.size( ); i++ )
    {
        newCommand->AddDataValuePair( _contourInformation.at( i ) );
    }

    //The advanced settings command
    ves::open::xml::CommandPtr advancedSettings( new ves::open::xml::Command( ) );
    advancedSettings->SetCommandName( "ADVANCED_CONTOUR_SETTINGS" );
    for ( size_t i = 0; i < _advancedSettings.size( ); i++ )
    {
        advancedSettings->AddDataValuePair( _advancedSettings.at( i ) );
    }
    std::string typeName = "Advanced Scalar Settings";

    //dvp representing the advanced settings within the contours information
    ves::open::xml::DataValuePairPtr advancedContourSettings( new ves::open::xml::DataValuePair( ) );
    advancedContourSettings->SetData( typeName, advancedSettings );
    newCommand->AddDataValuePair( advancedContourSettings );

    try
    {
        SendUpdatedSettingsToXplorer( newCommand, set );
    }
    catch ( ... )
    {
        QMessageBox msg;
        msg.setText( "Invalid Parent" );
        //msg.setTitle( "Communication Failure" );
        msg.setIcon( QMessageBox::Information );
        msg.exec( );
    }
}
////////////////////////////////////////////////////////////////////////////////

// This should go in a VisFeatureMaker base class
void ContourFeatureMaker::SendUpdatedSettingsToXplorer( ves::open::xml::CommandPtr subDialogCommand,
                                                        xplorer::data::PropertySet& set )
{
    _updateBaseInformation( set );
    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command( ) );

    for ( size_t i = 0; i < _vistabBaseInformation.size( ); i++ )
    {
        newCommand->AddDataValuePair( _vistabBaseInformation.at( i ) );
    }
    if( subDialogCommand )
    {
        ves::open::xml::DataValuePairPtr subDialogSettings( new ves::open::xml::DataValuePair( ) );
        subDialogSettings->SetData( "Sub-Dialog Settings", subDialogCommand );
        newCommand->AddDataValuePair( subDialogSettings );
    }
    newCommand->SetCommandName( _commandName );

    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( newCommand );
}
////////////////////////////////////////////////////////////////////////////////

// This should go in a VisFeatureMaker base class that can ask the underlying dataset
// about axes and bounding boxes and such.
void ContourFeatureMaker::_updateBaseInformation( xplorer::data::PropertySet& set )
{
    _vistabBaseInformation.clear( );
    _commandName.clear( );

    ///This is the default. Other dialogs actions will set the command name to the specific value if they are launched
    _commandName = "VISUALIZATION_SETTINGS";

    ves::open::xml::DataValuePairPtr activeScalar( new ves::open::xml::DataValuePair( ) );
    activeScalar->SetDataType( "STRING" );
    activeScalar->SetDataName( std::string( "Active Scalar" ) );
    activeScalar->SetDataString( boost::any_cast<std::string >
                                 ( set.GetPropertyAttribute
                                 ( "DataSet_ScalarData", "enumCurrentString" )
                                 ) );
    _vistabBaseInformation.push_back( activeScalar );


//    ves::open::xml::DataValuePairPtr activeVector( new ves::open::xml::DataValuePair() );
//    activeVector->SetDataType( "STRING" );
//    activeVector->SetDataName( std::string( "Active Vector" ) );
//    activeVector->SetDataString( _activeVectorName );
//    _vistabBaseInformation.push_back( activeVector );

    ves::open::xml::DataValuePairPtr activeDataset( new ves::open::xml::DataValuePair( ) );
    activeDataset->SetDataType( "STRING" );
    activeDataset->SetDataName( std::string( "Active Dataset" ) );
    activeDataset->SetDataString( boost::any_cast<std::string >
                                  ( set.GetPropertyAttribute
                                  ( "DataSet", "enumCurrentString" )
                                  ) );
    _vistabBaseInformation.push_back( activeDataset );

    ves::open::xml::DataValuePairPtr scalarMin( new ves::open::xml::DataValuePair( ) );
    double minimumValue = boost::any_cast<double>( set.GetPropertyValue( "DataSet_ScalarRange_Min" ) );
    scalarMin->SetData( "Scalar Min", minimumValue );
    _vistabBaseInformation.push_back( scalarMin );

    ves::open::xml::DataValuePairPtr scalarMax( new ves::open::xml::DataValuePair( ) );
    double maximumValue = boost::any_cast<double>( set.GetPropertyValue( "DataSet_ScalarRange_Max" ) );
    scalarMax->SetData( "Scalar Max", maximumValue );
    _vistabBaseInformation.push_back( scalarMax );

    //Store the axes display value
    /*VE_XML::DataValuePair* axes= new VE_XML::DataValuePair();
    axes->SetData( std::string("Show Axes"), static_cast< unsigned int >( axesCB->GetValue() ) );
    _vistabBaseInformation.push_back( axes );*/

    // Temporarily code-set:
        //Store the axes display value
        ves::open::xml::DataValuePairPtr bbox( new ves::open::xml::DataValuePair( ) );
        //bbox->SetData( std::string( "Show Bounding Box" ), static_cast < unsigned int > ( bboxCB->GetValue( ) ) );
        bbox->SetData( std::string( "Show Bounding Box" ), static_cast < unsigned int > ( 0 ) );
        _vistabBaseInformation.push_back( bbox );
    
        //Store the axes display value
        ves::open::xml::DataValuePairPtr wireMesh( new ves::open::xml::DataValuePair( ) );
        //wireMesh->SetData( std::string( "Show Wire Mesh" ), static_cast < unsigned int > ( wireFrameCB->GetValue( ) ) );
        wireMesh->SetData( std::string( "Show Wire Mesh" ), static_cast < unsigned int > ( 0 ) );
        _vistabBaseInformation.push_back( wireMesh );
    
        //set scalar bar state
        ves::open::xml::DataValuePairPtr scalarBarDVP( new ves::open::xml::DataValuePair( ) );
        //scalarBarDVP->SetData( "Scalar Bar State", static_cast < unsigned int > ( scalarBarCB->GetValue( ) ) );
        scalarBarDVP->SetData( "Scalar Bar State", static_cast < unsigned int > ( 0 ) );
        _vistabBaseInformation.push_back( scalarBarDVP );
}