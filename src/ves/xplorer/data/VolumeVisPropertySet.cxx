/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <ves/xplorer/data/VolumeVisPropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/data/Property.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/data/MakeLive.h>

#include <ves/xplorer/eventmanager/EventManager.h>

#include <boost/bind.hpp>
#include <boost/concept_check.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>

using namespace ves::xplorer::data;
////////////////////////////////////////////////////////////////////////////////
VolumeVisPropertySet::VolumeVisPropertySet()
{
    using eventmanager::SignalWrapper;
/*
    ///Signal for turning on seed points
    {
        std::string name("VolumeVisPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".ActivateSeedPoints";

        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< ActivateSeedPointsSignal_type >( &m_activateSeedPoints ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal to change the bounds of seed points
    {
        std::string name("VolumeVisPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".UpdateSeedPointBounds";
        
        eventmanager::EventManager::instance()->RegisterSignal(
           new SignalWrapper< UpdateSeedPointBoundsSignal_type >( &m_updateSeedPointBounds ),
           name, eventmanager::EventManager::unspecified_SignalType );
    }*/
    ///Signal to change the active dataset
    {
        std::string name("VolumeVisPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateScalarRange";
        
        eventmanager::EventManager::instance()->RegisterSignal(
           new SignalWrapper< ves::util::TwoDoubleSignal_type >( &m_updateTBETScalarRange ),
           name, eventmanager::EventManager::unspecified_SignalType );
    }
    ///Signal to change the active dataset
    {
        std::string name("VolumeVisPropertySet");
        name += boost::lexical_cast<std::string>( this );
        name += ".TBETUpdateScalar";
        
        eventmanager::EventManager::instance()->RegisterSignal(
            new SignalWrapper< UpdateScalar_type >( &m_updateTBETScalar ),
            name, eventmanager::EventManager::unspecified_SignalType );
    }
    
    mTableName = "VolumeVis";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
VolumeVisPropertySet::VolumeVisPropertySet( const VolumeVisPropertySet& orig )
    :
    VizBasePropertySet( orig )
{
}
////////////////////////////////////////////////////////////////////////////////
VolumeVisPropertySet::~VolumeVisPropertySet()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr VolumeVisPropertySet::CreateNew()
{
    return PropertySetPtr( new VolumeVisPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void VolumeVisPropertySet::UpdateScalarRange( PropertyPtr property )
{
    PropertyPtr min = mPropertyMap["DataSet_ScalarRange_Min"];
    PropertyPtr max = mPropertyMap["DataSet_ScalarRange_Max"];
    
    double castMin, castMax;
    castMax = boost::any_cast<double>( max->GetValue() );
    castMin = boost::any_cast<double>( min->GetValue() );

    m_updateTBETScalarRange( castMin, castMax );
}
////////////////////////////////////////////////////////////////////////////////
void VolumeVisPropertySet::UpdateScalar( PropertyPtr property )
{
    VizBasePropertySet::UpdateScalarDataRange( property );
    
    std::string const currentScalar = boost::any_cast<std::string >
        ( GetPropertyAttribute( "DataSet_ScalarData", "enumCurrentString" ) );

    PropertyPtr min = mPropertyMap["DataSet_ScalarRange_Min"];
    PropertyPtr max = mPropertyMap["DataSet_ScalarRange_Max"];
    
    double castMin, castMax;
    castMax = boost::any_cast<double>( max->GetValue() );
    castMin = boost::any_cast<double>( min->GetValue() );
    
    m_updateTBETScalar( currentScalar, "Scalar", castMin, castMax );
}
////////////////////////////////////////////////////////////////////////////////
void VolumeVisPropertySet::CreateSkeleton()
{
/*
    wxStaticBox* scalarNames = new wxStaticBox( this, -1, wxT( "Active Scalar" ) );
    wxStaticBoxSizer* scalarNameSizer = new wxStaticBoxSizer( scalarNames, wxVERTICAL );
    //_availableScalars = new wxComboBox(this,availableScalars);//,wxEmptyString, wxDefaultPosition, wxSize(150,wxDefaultCoord) );
    //scalarNameSizer->Add(_availableScalars,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
    
    mainSizer->Add( scalarNameSizer, 1, wxEXPAND | wxALIGN_CENTER_HORIZONTAL );
    
    wxStaticText* itemStaticText6 = new wxStaticText( this, wxID_STATIC, _T( "Isosurface" ), wxDefaultPosition, wxDefaultSize, 0 );
    mainSizer->Add( itemStaticText6, 0, wxALIGN_LEFT | wxALL | wxADJUST_MINSIZE, 5 );
    
    _isoSurfaceSlider = new wxSlider( this, TBISOSURFACE_PLANE_SLIDER, 0, 0, 100, wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL | wxSL_LABELS );
    mainSizer->Add( _isoSurfaceSlider, 0, wxGROW | wxALL, 5 );

 ves::open::xml::DataValuePairPtr isosurfaceValue( new ves::open::xml::DataValuePair() );
 isosurfaceValue->SetData( "Iso-Surface Value", static_cast<double>(( _isoSurfaceSlider->GetValue() ) ) );
 newCommand->AddDataValuePair( isosurfaceValue );
 
 ves::open::xml::DataValuePairPtr colorByScalar( new ves::open::xml::DataValuePair() );
 colorByScalar->SetData( "Color By Scalar", _colorByScalarName );
 newCommand->AddDataValuePair( colorByScalar );
 
*/    

/*
 _xBounds = new DualSlider( this, -1, 1, 0, 100, 0, 100, wxDefaultPosition, wxDefaultSize,
 wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS, _( "X Bounds" ) );
 ROIMinSliderCallback* minX = new ROIMinSliderCallback( this, "X" );
 ROIMaxSliderCallback* maxX = new ROIMaxSliderCallback( this, "X" );
 ROIBothMoveCallback* bothX = new ROIBothMoveCallback( this, "X" );
 
 _xBounds->SetMinSliderCallback( minX );
 _xBounds->SetMaxSliderCallback( maxX );
 _xBounds->SetBothSliderUpdateCallback( bothX );
 
 _yBounds = new DualSlider( this, -1, 1, 0, 100, 0, 100, wxDefaultPosition, wxDefaultSize,
 wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS, _( "Y Bounds" ) );
 
 ROIMinSliderCallback* minY = new ROIMinSliderCallback( this, "Y" );
 ROIMaxSliderCallback* maxY = new ROIMaxSliderCallback( this, "Y" );
 ROIBothMoveCallback* bothY = new ROIBothMoveCallback( this, "Y" );
 
 _yBounds->SetMinSliderCallback( minY );
 _yBounds->SetMaxSliderCallback( maxY );
 _yBounds->SetBothSliderUpdateCallback( bothY );
 
 _zBounds = new DualSlider( this, -1, 1, 0, 100, 0, 100, wxDefaultPosition, wxDefaultSize,
 wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS, _( "Z Bounds" ) );
 
 ROIMinSliderCallback* minZ = new ROIMinSliderCallback( this, "Z" );
 ROIMaxSliderCallback* maxZ = new ROIMaxSliderCallback( this, "Z" );
 ROIBothMoveCallback* bothZ = new ROIBothMoveCallback( this, "Z" );

 void ROIDialog::ROIMinSliderCallback::SliderOperation()
 {
 _roidlg->SetCommandName( "TB_ROI_UPDATE" );
 
 ves::open::xml::DataValuePairPtr coordinate( new ves::open::xml::DataValuePair() );
 coordinate->SetDataType( "STRING" );
 coordinate->SetDataName( std::string( "Coordinate" ) );
 coordinate->SetDataString( _direction );
 _roidlg->AddInstruction( coordinate );
 
 ves::open::xml::DataValuePairPtr direction( new ves::open::xml::DataValuePair() );
 direction->SetDataType( "STRING" );
 direction->SetDataName( std::string( "Direction" ) );
 direction->SetDataString( "Positive" );
 _roidlg->AddInstruction( direction );
 
 ves::open::xml::DataValuePairPtr value( new ves::open::xml::DataValuePair() );
 value->SetData( "ROI Value", static_cast<double>( _dualSlider->GetMinSliderValue() ) / 100.0 );
 _roidlg->AddInstruction( value );
 
 _roidlg->SendCommands();
 _roidlg->ClearInstructions();
 }
 ///////////////////////////////////////////////////////
 void ROIDialog::ROIMaxSliderCallback::SliderOperation()
 {
 _roidlg->SetCommandName( "TB_ROI_UPDATE" );
 ves::open::xml::DataValuePairPtr coordinate( new ves::open::xml::DataValuePair() );
 coordinate->SetDataType( "STRING" );
 coordinate->SetDataName( std::string( "Coordinate" ) );
 coordinate->SetDataString( _direction );
 _roidlg->AddInstruction( coordinate );
 
 ves::open::xml::DataValuePairPtr direction( new ves::open::xml::DataValuePair() );
 direction->SetDataType( "STRING" );
 direction->SetDataName( std::string( "Direction" ) );
 direction->SetDataString( "Negative" );
 _roidlg->AddInstruction( direction );
 
 ves::open::xml::DataValuePairPtr value( new ves::open::xml::DataValuePair() );
 value->SetData( "ROI Value", static_cast<double>( _dualSlider->GetMaxSliderValue() ) / 100.0 );
 _roidlg->AddInstruction( value );
 
 _roidlg->SendCommands();
 _roidlg->ClearInstructions();
 }
 //////////////////////////////////////////////////////
 void ROIDialog::ROIBothMoveCallback::SliderOperation()
 {
 _roidlg->SetCommandName( "TB_ROI_UPDATE" );
 
 ves::open::xml::DataValuePairPtr coordinate( new ves::open::xml::DataValuePair() );
 coordinate->SetDataType( "STRING" );
 coordinate->SetDataName( std::string( "Coordinate" ) );
 coordinate->SetDataString( _direction );
 _roidlg->AddInstruction( coordinate );
 
 ves::open::xml::DataValuePairPtr direction( new ves::open::xml::DataValuePair() );
 direction->SetDataType( "STRING" );
 direction->SetDataName( std::string( "Direction" ) );
 direction->SetDataString( "Both" );
 _roidlg->AddInstruction( direction );
 
 
 ves::open::xml::DataValuePairPtr minvalue( new ves::open::xml::DataValuePair() );
 minvalue->SetData( "ROI Min Value", static_cast<double>( _dualSlider->GetMinSliderValue() ) / 100.0 );
 _roidlg->AddInstruction( minvalue );
 
 ves::open::xml::DataValuePairPtr maxvalue( new ves::open::xml::DataValuePair() );
 maxvalue->SetData( "ROI Max Value", static_cast<double>( _dualSlider->GetMaxSliderValue() ) / 100.0 );
 _roidlg->AddInstruction( maxvalue );
 
 _roidlg->SendCommands();
 _roidlg->ClearInstructions();
 }
*/
 
    {
        AddProperty( "Hide", false, "Toggle Viz Off" );
        const std::string slotName = 
        boost::lexical_cast<std::string>( this ) +".HideVizFeature";
        std::vector< PropertyPtr > dataLink;
        dataLink.push_back( GetProperty( "Hide" ) );
        MakeLiveBasePtr p( 
                          new MakeLiveLinked< bool >( mUUIDString, dataLink,
                                                                  slotName ) );
        mLiveObjects.push_back( p );
    }
    
    AddProperty( "DataSet", 0, "Data Set" );
    PSVectorOfStrings enumValues;

    AddProperty( "DataSet_ScalarData", 0, "Scalar Data" );
    // Dummy value to ensure this gets set up as an enum
    enumValues.push_back( "Select Scalar Data" );
    SetPropertyAttribute( "DataSet_ScalarData", "enumValues", enumValues );
    mPropertyMap["DataSet"]->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateScalarDataOptions, this, _1 ) );

    AddProperty( "DataSet_ScalarRange", boost::any(), "Scalar Range" );
    SetPropertyAttribute( "DataSet_ScalarRange", "isUIGroupOnly", true );
    SetPropertyAttribute( "DataSet_ScalarRange", "setExpanded", true );

    AddProperty( "DataSet_ScalarRange_Min", 0.0, "Min" );
    mPropertyMap["DataSet_ScalarRange_Min"]->SetDisabled();

    AddProperty( "DataSet_ScalarRange_Max", 1.0, "Max" );
    mPropertyMap["DataSet_ScalarRange_Max"]->SetDisabled();

    mPropertyMap["DataSet_ScalarData"]->SignalValueChanged.connect( boost::bind( &VolumeVisPropertySet::UpdateScalar, this, _1 ) );
    mPropertyMap["DataSet_ScalarRange_Min"]->SignalRequestValidation.connect( boost::bind( &VizBasePropertySet::ValidateScalarMinMax, this, _1, _2 ) );
    mPropertyMap["DataSet_ScalarRange_Max"]->SignalRequestValidation.connect( boost::bind( &VizBasePropertySet::ValidateScalarMinMax, this, _1, _2 ) );
    mPropertyMap["DataSet_ScalarRange_Min"]->SignalValueChanged.connect( boost::bind( &VolumeVisPropertySet::UpdateScalarRange, this, _1 ) );
    mPropertyMap["DataSet_ScalarRange_Max"]->SignalValueChanged.connect( boost::bind( &VolumeVisPropertySet::UpdateScalarRange, this, _1 ) );

    /*AddProperty( "DataSet_VectorData", 0, "Vector Data" );
    enumValues.clear();
    enumValues.push_back( "Select Vector Data" );
    SetPropertyAttribute( "DataSet_VectorData", "enumValues", enumValues );
    mPropertyMap["DataSet"]->SignalValueChanged.connect( boost::bind( &VizBasePropertySet::UpdateVectorDataOptions, this, _1 ) );*/
    
    // Now that DataSet subproperties exist, we can initialize the values in
    // the dataset enum. If we had tried to do this beforehand, none of the
    // connections between DataSet and its subproperties would have been in
    // place yet.
    enumValues.clear();
    enumValues = ves::xplorer::data::DatabaseManager::instance()->GetStringVector( "Dataset", "Filename" );
    if( enumValues.empty() )
    {
        enumValues.push_back( "No datasets loaded" );
    }

    SetPropertyAttribute( "DataSet", "enumValues", enumValues );
    // Now that DataSet has choices loaded, force an update on the available
    // scalar and vector data
    PropertyPtr nullPtr;
    UpdateScalarDataOptions( nullPtr );
    UpdateVectorDataOptions( nullPtr );

    //Setup ROI bbox
    //Setup iso surface activation
    ///Setup iso surface value
    ///setup iso surface color by scalar
    //setup preintegration values
    //Setup number of slice planes
    
    ///How to control vis animations????

    ///Integration controls
    /*AddProperty( "IntegrationDirection", 2, "Integration Direction" );
    enumValues.clear();
    enumValues.push_back( "Backward" );
    enumValues.push_back( "Forward" );
    enumValues.push_back( "Both" );
    SetPropertyAttribute( "IntegrationDirection", "enumValues", enumValues );*/

    ///Seed point controls
    /*AddProperty( "SeedPoints", boost::any(), "Seed Points");
    SetPropertyAttribute( "SeedPoints", "isUIGroupOnly", true );
    SetPropertyAttribute( "SeedPoints", "setExpanded", true );
    AddProperty( "SeedPoints_DisplaySeedPoints", false, "Display Seed Points" );
    mPropertyMap["SeedPoints_DisplaySeedPoints"]->SignalValueChanged.connect( boost::bind( &VolumeVisPropertySet::UpdateSeedPointDisplay, this, _1 ) );
    AddProperty( "SeedPoints_NumberOfPointsInX", 5, "Number of Points in X" );
    SetPropertyAttribute( "SeedPoints_NumberOfPointsInX", "minimumValue",   0 );
    AddProperty( "SeedPoints_NumberOfPointsInY", 5, "Number of Points in Y" );
    SetPropertyAttribute( "SeedPoints_NumberOfPointsInY", "minimumValue",   0 );
    AddProperty( "SeedPoints_NumberOfPointsInZ", 1, "Number of Points in Z" );
    SetPropertyAttribute( "SeedPoints_NumberOfPointsInZ", "minimumValue",   0 );
*/
    // Link the three NumberOfPointsIn... properties together and have them
    // fire a signal with signature void( std::vector<int> ) whose tail is
    // named "UpdateSeedPointDimensions"
    /*std::vector< PropertyPtr > numberOfPointsLink;
    numberOfPointsLink.push_back( GetProperty( "SeedPoints_NumberOfPointsInX" ) );
    numberOfPointsLink.push_back( GetProperty( "SeedPoints_NumberOfPointsInY" ) );
    numberOfPointsLink.push_back( GetProperty( "SeedPoints_NumberOfPointsInZ" ) );
    MakeLiveBasePtr p( new MakeLiveLinked< int >(
            mUUIDString,
            numberOfPointsLink,
            "UpdateSeedPointDimensions" ) );
    mLiveObjects.push_back( p );

    AddProperty( "SeedPoints_Bounds", boost::any(), "Bounds");
    SetPropertyAttribute( "SeedPoints_Bounds", "isUIGroupOnly", true );
    SetPropertyAttribute( "SeedPoints_Bounds", "setExpanded", true );
    AddProperty( "SeedPoints_Bounds_XMin",   0.0, "X Minimum" );
    SetPropertyAttribute( "SeedPoints_Bounds_XMin", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_XMin", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_XMax", 1.0, "X Maximum" );
    SetPropertyAttribute( "SeedPoints_Bounds_XMax", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_XMax", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_YMin",   0.0, "Y Minimum" );
    SetPropertyAttribute( "SeedPoints_Bounds_YMin", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_YMin", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_YMax", 1.0, "Y Maximum" );
    SetPropertyAttribute( "SeedPoints_Bounds_YMax", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_YMax", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_ZMin",   0.0, "Z Minimum" );
    SetPropertyAttribute( "SeedPoints_Bounds_ZMin", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_ZMin", "maximumValue", 1.0 );
    AddProperty( "SeedPoints_Bounds_ZMax", 1.0, "Z Maximum" );
    SetPropertyAttribute( "SeedPoints_Bounds_ZMax", "minimumValue",   0.0 );
    SetPropertyAttribute( "SeedPoints_Bounds_ZMax", "maximumValue", 1.0 );*/

    ///General streamline properties
    /*AddProperty( "UseGPUTools", false, "Use GPU Tools" );
    AddProperty( "UseStreamArrows", false, "Use Stream Arrows" );
    AddProperty( "UseStreamRibbons", false, "Use Stream Ribbons" );
    AddProperty( "UseLastSeedPoints", false, "Use Last Seed Points" );

    ///Old values that are no longer used by the vis code
    AddProperty( "CursorDirection", 0, "Cursor Direction" );
    enumValues.clear();
    enumValues.push_back( "x" );
    SetPropertyAttribute( "CursorDirection", "enumValues", enumValues );
    
    AddProperty( "CursortType", 0, "Cursor Type" );
    enumValues.clear();
    enumValues.push_back( "none" );
    SetPropertyAttribute( "CursortType", "enumValues", enumValues );
    
    //AddProperty( "StreamlineSize", 0.5, "Streamline Size" );
    //AddProperty( "NumberOfPointsPerPlane", 2.0, "Number of Points Per Plane" );

    ///Advanced settings
    AddProperty( "Advanced", boost::any(), "Advanced" );
    SetPropertyAttribute( "Advanced", "isUIGroupOnly", true );
    AddProperty( "Advanced_PropogationTime", 100.0, "Propogation Time" );
    SetPropertyAttribute( "Advanced_PropogationTime", "minimumValue", 1.0 );
    SetPropertyAttribute( "Advanced_PropogationTime", "maximumValue", 100.0 );
    AddProperty( "Advanced_IntegrationStepSize", 1000.0f, "Integration Step Size" );
    SetPropertyAttribute( "Advanced_IntegrationStepSize", "minimumValue", 1.0 );
    SetPropertyAttribute( "Advanced_IntegrationStepSize", "maximumValue", 5000.0 );
    AddProperty( "Advanced_Diameter", -80.0f, "Diameter" );
    SetPropertyAttribute( "Advanced_Diameter", "minimumValue", -100.0 );
    SetPropertyAttribute( "Advanced_Diameter", "maximumValue", 100.0 );
    AddProperty( "Advanced_SphereArrowParticleSize", 5.0f, "Sphere/Arrow/Particle Size" );
    SetPropertyAttribute( "Advanced_VectorThreshold", "minimumValue", 1.0 );
    SetPropertyAttribute( "Advanced_VectorThreshold", "maximumValue", 50.0 );
    */
    /*
    AddProperty( "Advanced_VectorScale", 200.0, "Vector Scale" );
    SetPropertyAttribute( "Advanced_VectorScale", "minimumValue",   1.0 );
    SetPropertyAttribute( "Advanced_VectorScale", "maximumValue", 400.0 );

    AddProperty( "Advanced_VectorRatio", 1.0, "Vector Ratio" );
    SetPropertyAttribute( "Advanced_VectorRatio", "minimumValue",   1.0 );
    SetPropertyAttribute( "Advanced_VectorRatio", "maximumValue", 200.0 );

    AddProperty( "Advanced_ScaleByVectorMagnitude", false, "Scale By Vector Magnitude" );
    
    ///Mode controls
    AddProperty( "Mode", 0, "Mode" );
    enumValues.clear();
    enumValues.push_back( "Specify a Single Plane" );
    enumValues.push_back( "Use All Precomputed Surfaces" );
    SetPropertyAttribute( "Mode", "enumValues", enumValues );
    // Connect SignalValueChanged of "Mode" to a function that enables and disables
    // its sub-properties as appropriate
    PropertyPtr mode = mPropertyMap["Mode"];
    if( mode )
    {
        mode->SignalValueChanged.connect( boost::bind( &VolumeVisPropertySet::UpdateModeOptions, this, _1 ) );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
/*void VolumeVisPropertySet::UpdateSeedPointDisplay( PropertyPtr property )
{
    const std::string dataSetName = 
        boost::any_cast< std::string >( GetPropertyAttribute( "DataSet", "enumCurrentString" ) );
    m_activeDataSet( dataSetName );

    bool showDataSet = boost::any_cast< bool >( property->GetValue() );

    //Update the seed point bounds before turning the box off or on
    if( showDataSet )
    {
        std::vector<double> seedPointBounds;
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_XMin" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_XMax" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_YMin" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_YMax" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_ZMin" ) ) );
        seedPointBounds.push_back( boost::any_cast<double>( GetPropertyValue( "SeedPoints_Bounds_ZMax" ) ) );
        m_updateSeedPointBounds( seedPointBounds );    
    }

    m_activateSeedPoints( dataSetName, showDataSet );
}
////////////////////////////////////////////////////////////////////////////////*/
void VolumeVisPropertySet::ActivateVoilumeVis()
{
    //_tbTools->SetVectors( _availableSolutions["TEXTURE_VECTORS"] );
    //_tbTools->SetScalars( _availableSolutions["TEXTURE_SCALARS"] );
    
    /*_commandName = "TB_ACTIVATE";
    
    ves::open::xml::DataValuePairPtr activeDatasetName( new ves::open::xml::DataValuePair() );
    activeDatasetName->SetData( std::string( "Active Dataset Name" ) ,
                               dynamic_cast< Vistab* >( GetParent() )->GetActiveDatasetName() );
    _instructions.push_back( activeDatasetName );
    
    if( _availableScalars.GetCount() )
    {
        ves::open::xml::DataValuePairPtr activateCommand( new ves::open::xml::DataValuePair() );
        activateCommand->SetDataType( "STRING" );
        activateCommand->SetDataName( std::string( "Active Scalar" ) );
        activateCommand->SetDataString( ConvertUnicode( _availableScalars[0].GetData() ) );
        _instructions.push_back( activateCommand );
        _sendCommandsToXplorer();
    }
    else if( _availableVectors.GetCount() )
    {
        ves::open::xml::DataValuePairPtr activateCommand( new ves::open::xml::DataValuePair() );
        activateCommand->SetDataType( "STRING" );
        activateCommand->SetDataName( std::string( "Active Vector" ) );
        activateCommand->SetDataString( ConvertUnicode( _availableVectors[0].GetData() ) );
        _instructions.push_back( activateCommand );
        _sendCommandsToXplorer();
    }    */
}

/*
 void ScalarToolsDialog::ScalarToolsSliderCallback::SliderOperation()
 {
 _scalarDlg->ClearInstructions();
 _scalarDlg->SetCommandName( "TB_SCALAR_RANGE" );
 
 ves::open::xml::DataValuePairPtr minRangevalue( new ves::open::xml::DataValuePair() );
 minRangevalue ->SetData( "Mininum Scalar Range", static_cast<double>( _dualSlider->GetMinSliderValue() ) / 100.0 );
 _scalarDlg->AddInstruction( minRangevalue );
 
 ves::open::xml::DataValuePairPtr maxRangevalue( new ves::open::xml::DataValuePair() );
 maxRangevalue->SetData( "Maximum Scalar Range", static_cast<double>( _dualSlider->GetMaxSliderValue() ) / 100.0 );
 _scalarDlg->AddInstruction( maxRangevalue );
 
 _scalarDlg->SendCommands();
 _scalarDlg->ClearInstructions();
 }
 /////////////////////////////////////////////////////////////////////////////////////////
 void ScalarToolsDialog::ScalarToolsStopSliderCallback::SliderOperation()
 {
 _scalarDlg->ClearInstructions();
 _scalarDlg->SetCommandName( "TB_FULL_PREINTEGRATE_UPDATE" );
 
 ves::open::xml::DataValuePairPtr fullUpdate( new ves::open::xml::DataValuePair() );
 unsigned int on = 1;
 fullUpdate ->SetData( "Recalculate Pre-Integration", on );
 _scalarDlg->AddInstruction( fullUpdate );
 
 _scalarDlg->SendCommands();
 _scalarDlg->ClearInstructions();
 }
 ////////////////////////////////////////////////////////////////////////////////////
 void ScalarToolsDialog::UpdateScalarList( wxArrayString scalarNames )
 {
 _scalarSelection->Clear();
 for( size_t i = 0; i < scalarNames.GetCount(); i++ )
 {
 _scalarSelection->Append( scalarNames[i] );
 }
 if( scalarNames.GetCount() )
 {
 _scalarSelection->SetValue( _activeScalar );
 }
 }
 /////////////////////////////////////////////////////////////////////////////////////////////
 void ScalarToolsDialog::_updateActiveScalar( wxCommandEvent& command )
 {
 ClearInstructions();
 
 _commandName = "TB_ACTIVE_SOLUTION";
 _activeScalar = _scalarSelection->GetValue();
 ves::open::xml::DataValuePairPtr name( new ves::open::xml::DataValuePair() );
 name->SetData( "Active Dataset", ConvertUnicode( _activeScalar.GetData() ) );
 _instructions.push_back( name );
 
 ves::open::xml::DataValuePairPtr type( new ves::open::xml::DataValuePair() );
 type->SetData( "Data Type", "Scalar" );
 _instructions.push_back( type );
 
 ves::open::xml::DataValuePairPtr minRangevalue( new ves::open::xml::DataValuePair() );
 minRangevalue->SetData( "Mininum Scalar Range", static_cast<double>( _scalarRange->GetMinSliderValue() ) / 100.0 );
 _instructions.push_back( minRangevalue );
 
 ves::open::xml::DataValuePairPtr maxRangevalue( new ves::open::xml::DataValuePair() );
 maxRangevalue->SetData( "Maximum Scalar Range", static_cast<double>( _scalarRange->GetMaxSliderValue() ) / 100.0 );
 _instructions.push_back( maxRangevalue );
 
 _sendCommandsToXplorer();
 ClearInstructions();
 wxScrollEvent event;
 _onPreIntegrate( event );
 }
 /////////////////////////////////////////////////////////////////////////////////
 void ScalarToolsDialog::_updateActiveScalarShaderManager( wxCommandEvent& command )
 {
 ClearInstructions();
 
 _commandName = "TB_SET_ACTIVE_SHADER_MANAGER";
 
 ves::open::xml::DataValuePairPtr name( new ves::open::xml::DataValuePair() );
 name->SetData( "Active Shader Manager", ConvertUnicode( _shaderManagerSelection->GetValue().GetData() ) );
 _instructions.push_back( name );
 
 _sendCommandsToXplorer();
 ClearInstructions();
 wxScrollEvent event;
 _onPreIntegrate( event );
 }
 ////////////////////////////////////////////////////////////////
 void ScalarToolsDialog::_setColorByFace( wxCommandEvent& command )
 {
 int selectionIndex = 0;
 wxArrayString scalarNames;
 for( size_t i = 0; i < _scalarSelection->GetCount(); i++ )
 {
 if( !_scalarSelection->GetString( i ).Cmp( _colorByScalarName.c_str() ) )
 {
 selectionIndex = i;
 }
 scalarNames.Add( _scalarSelection->GetString( i ) );
 }
 wxSingleChoiceDialog scalarSelector( this, _T( "Select Scalar to color isosurface by." ), _T( "Color by Scalar" ),
 scalarNames );
 scalarSelector.SetSize( GetRect() );
 scalarSelector.SetSelection( selectionIndex );
 if( scalarSelector.ShowModal() == wxID_OK )
 {
 _colorByScalarName = scalarSelector.GetStringSelection();
 }
 }
 //////////////////////////////////////////////////////////////////////////////////
 void ScalarToolsDialog::_onPreIntegrate( wxScrollEvent& command )
 {
 ClearInstructions();
 _commandName = "TB_FULL_PREINTEGRATE_UPDATE";
 
 unsigned int on = 1;
 ves::open::xml::DataValuePairPtr fullUpdate( new ves::open::xml::DataValuePair() );
 fullUpdate ->SetData( "Recalculate Pre-Integration", on );
 AddInstruction( fullUpdate );
 
 _sendCommandsToXplorer();
 ClearInstructions();
 }
 ////////////////////////////////////////////////////////////////////////////////////////
 void ScalarToolsDialog::_onUpdateIsosurface( wxScrollEvent& command )
 {
 ClearInstructions();
 _commandName = "TB_UPDATE_ISOSURFACE";
 
 ves::open::xml::DataValuePairPtr isosurfaceValue( new ves::open::xml::DataValuePair() );
 isosurfaceValue->SetData( "Iso-Surface Value", static_cast<double>(( _isoSlider->GetValue() / 100.0 ) ) );
 _instructions.push_back( isosurfaceValue );
 
 ves::open::xml::DataValuePairPtr colorByScalar( new ves::open::xml::DataValuePair() );
 colorByScalar->SetData( "Color By Scalar", ConvertUnicode( _colorByScalarName.GetData() ) );
 _instructions.push_back( colorByScalar );
 
 _sendCommandsToXplorer();
 ClearInstructions();
 }
 ////////////////////////////////////////////////////////////////////////////
 void ScalarToolsDialog::_onUpdateNumberOfSlicePlanes( wxScrollEvent& command )
 {
 ClearInstructions();
 _commandName = "TB_UPDATE_NUMBER_SLICE_PLANES";
 
 ves::open::xml::DataValuePairPtr nPlanesValue( new ves::open::xml::DataValuePair() );
 nPlanesValue->SetData( "Number of Slice Planes", static_cast<unsigned int>(( _numSlicesSlider->GetValue() ) ) );
 _instructions.push_back( nPlanesValue );
 
 _sendCommandsToXplorer();
 ClearInstructions();
 }
 ////////////////////////////////////////////////////////////////////
 void ScalarToolsDialog::_onEnableIsoSurface( wxCommandEvent& command )
 {
 _isoSlider->Enable( _isosurfaceCheck->GetValue() );
 //this isn't ready yet
 //_advancedButton->Enable(_isosurfaceCheck->GetValue());
 
 ClearInstructions();
 _commandName = "TB_ISOSURFACE_ENABLE";
 
 ves::open::xml::DataValuePairPtr isosurfaceValue( new ves::open::xml::DataValuePair() );
 isosurfaceValue->SetData( "Iso-Surface State", ( _isosurfaceCheck->GetValue() ) ? "On" : "Off" );
 _instructions.push_back( isosurfaceValue );
 
 _sendCommandsToXplorer();
 ClearInstructions();
*/