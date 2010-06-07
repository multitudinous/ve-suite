#include <ves/conductor/qt/VisFeatureMakerBase.h>
#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <iostream>

namespace ves
{
namespace conductor
{

VisFeatureMakerBase::VisFeatureMakerBase( )
{
}

VisFeatureMakerBase::VisFeatureMakerBase( const VisFeatureMakerBase& orig )
{
}

VisFeatureMakerBase::~VisFeatureMakerBase( )
{
}

void VisFeatureMakerBase::_updateBaseInformation( xplorer::data::PropertySet& set )
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

    ves::open::xml::DataValuePairPtr activeVector( new ves::open::xml::DataValuePair( ) );
    activeVector->SetDataType( "STRING" );
    activeVector->SetDataName( std::string( "Active Vector" ) );
    activeVector->SetDataString( boost::any_cast<std::string >
                                 ( set.GetPropertyAttribute
                                 ( "DataSet_VectorData", "enumCurrentString" )
                                 ) );
    _vistabBaseInformation.push_back( activeVector );

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

void VisFeatureMakerBase::SendUpdatedSettingsToXplorer( ves::open::xml::CommandPtr subDialogCommand,
                                                        xplorer::data::PropertySet& set )
{
    _updateBaseInformation( set );
    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command( ) );

    for( size_t i = 0; i < _vistabBaseInformation.size( ); i++ )
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

    ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( newCommand );
}
////////////////////////////////////////////////////////////////////////////////

void VisFeatureMakerBase::_updateAdvancedSettings( xplorer::data::PropertySet& set )
{
    _advancedSettings.clear( );

    // With a bit of re-thinking here and some normalization of names, we may be
    // able to convert much of the following code to something like:
    //
    // std::vector<std::string> propList = set.GetPropertyList();
    // std::vector<std::sting>::iterator iter;
    // for( iter = propList.begin(); iter != propList.end(); iter++ )
    // {
    //      ...1. Search for substring "Advanced_" in (*iter)
    //      ...2. Extract remainder of name in (*iter)
    //      ...3. Create a DVP, set its name to string from step 2, set
    //              its value to cast value from set.GetPropertyValue( *iter ).
    //              Casting operation could look at property type to determine
    //              appropriate cast -- esp for enums, to use associated enumCurrentString
    //      ...4. Add DVP to _advancedSettings
    // }

    if( set.PropertyExists( "Advanced_Opacity" ) )
    {
        ves::open::xml::DataValuePairPtr contourOpacity( new ves::open::xml::DataValuePair( ) );
        contourOpacity->SetData( "Contour Opacity",
                                  boost::any_cast<double>
                                 ( set.GetPropertyValue( "Advanced_Opacity" ) ) );
        _advancedSettings.push_back( contourOpacity );
    }

    if( set.PropertyExists( "Advanced_WarpedContourScale" ) )
    {
        ves::open::xml::DataValuePairPtr warpedScale( new ves::open::xml::DataValuePair( ) );
        warpedScale->SetData( "Warped Contour Scale",
                              boost::any_cast<double>
                              ( set.GetPropertyValue( "Advanced_WarpedContourScale" ) ) );
        _advancedSettings.push_back( warpedScale );
    }

    if( set.PropertyExists( "Advanced_ContourLOD" ) )
    {
        ves::open::xml::DataValuePairPtr LODSetting( new ves::open::xml::DataValuePair( ) );
        LODSetting->SetData( "Contour LOD",
                             boost::any_cast<double>
                             ( set.GetPropertyValue( "Advanced_ContourLOD" ) ) );
        _advancedSettings.push_back( LODSetting );
    }

    if( set.PropertyExists( "Advanced_ContourType" ) )
    {
        ves::open::xml::DataValuePairPtr contourType( new ves::open::xml::DataValuePair( ) );
        contourType->SetDataType( "STRING" );
        contourType->SetDataName( std::string( "Type" ) );
        std::string _planeType = boost::any_cast<std::string >
                ( set.GetPropertyAttribute
                ( "Advanced_ContourType", "enumCurrentString" ) );
        contourType->SetDataString( _planeType );
        _advancedSettings.push_back( contourType );
    }

    if( set.PropertyExists( "Advanced_WarpOption" ) )
    {
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

//    if( m_datasetSelection->IsEnabled() )
//    {
//        ves::open::xml::DataValuePairPtr surfToolsDVP( new ves::open::xml::DataValuePair() );
//        surfToolsDVP->SetData( "SURF Tools", ConvertUnicode( m_datasetSelection->GetValue().c_str() ) );
//        _advancedSettings.push_back( surfToolsDVP );
//    }

}

} // namespace conductor
} // namespace ves
