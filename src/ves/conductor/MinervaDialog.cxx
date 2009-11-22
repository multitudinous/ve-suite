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
 * Date modified: $Date: 2009-06-28 23:47:14 -0700 (Sun, 28 Jun 2009) $
 * Version:       $Rev: 12939 $
 * Author:        $Author: akubach $
 * Id:            $Id: AppFrame.cxx 12939 2009-06-29 06:47:14Z akubach $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

///////////////////////////////////////////////////////////////////////////////
//
//  Dialog for Minerva properties.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/conductor/MinervaDialog.h>
#include <ves/conductor/MinervaWmsDialog.h>
#include <ves/conductor/UserPreferencesDataBuffer.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/util/commands/Minerva.h>

#include <vpr/Util/GUID.h>

// Include this to avoid unresolved references to GetClassInfo.
#if defined (WIN32)
# include <wx/msw/winundef.h>
#elif defined (WIN64)
# include <wx/msw/winundef.h>
#endif

#include <wx/button.h>
#include <wx/filedlg.h>
#include <wx/listbox.h>
#include <wx/string.h>
#include <wx/gdicmn.h>
#include <wx/font.h>
#include <wx/colour.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/filename.h>

const wxString WINDOW_TITLE( wxT( "Minerva Properties" ) );

namespace Detail {

template<class T>
inline ves::open::xml::DataValuePairPtr MakeDVP ( const std::string& name, T value )
{
  ves::open::xml::DataValuePairPtr dvp ( new ves::open::xml::DataValuePair );
  dvp->SetData ( name, value );
  return dvp;
}

}


///////////////////////////////////////////////////////////////////////////////
//
//  Event Ids.
//
///////////////////////////////////////////////////////////////////////////////

enum
{
  MINERVA_DIALOG_ADD_ELEVATION_WMS_LAYER,
  MINERVA_DIALOG_ADD_ELEVATION_LAYER_FILE_SYSTEM,
  MINERVA_DIALOG_REMOVE_ELEVATION_LAYER,
  MINERVA_DIALOG_ADD_RASTER_WMS_LAYER,
  MINERVA_DIALOG_ADD_RASTER_LAYER_FILE_SYSTEM,
  MINERVA_DIALOG_REMOVE_RASTER_LAYER,
  MINERVA_DIALOG_ELEVATION_LAYER_LIST,
  MINERVA_DIALOG_RASTER_LAYER_LIST
};


///////////////////////////////////////////////////////////////////////////////
//
//  Event table.
//
///////////////////////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( MinervaDialog, MinervaDialog::BaseClass )
    EVT_BUTTON( MINERVA_DIALOG_ADD_ELEVATION_WMS_LAYER, MinervaDialog::AddElevationLayerWMS )
    EVT_BUTTON( MINERVA_DIALOG_ADD_ELEVATION_LAYER_FILE_SYSTEM, MinervaDialog::AddElevationLayerFileSystem )
    EVT_BUTTON( MINERVA_DIALOG_REMOVE_ELEVATION_LAYER, MinervaDialog::RemoveElevationLayer )
    EVT_BUTTON( MINERVA_DIALOG_ADD_RASTER_WMS_LAYER, MinervaDialog::AddRasterLayerWMS )
    EVT_BUTTON( MINERVA_DIALOG_ADD_RASTER_LAYER_FILE_SYSTEM, MinervaDialog::AddRasterLayerFileSystem )
    EVT_BUTTON( MINERVA_DIALOG_REMOVE_RASTER_LAYER, MinervaDialog::RemoveRasterLayer )
    EVT_LISTBOX_DCLICK( MINERVA_DIALOG_ELEVATION_LAYER_LIST, MinervaDialog::NavigateToElevationLayer )
    EVT_LISTBOX_DCLICK( MINERVA_DIALOG_RASTER_LAYER_LIST, MinervaDialog::NavigateToRasterLayer )
END_EVENT_TABLE()


///////////////////////////////////////////////////////////////////////////////
MinervaDialog::MinervaDialog( 
  wxWindow *parent, 
  wxWindowID id ) : BaseClass( parent, id, WINDOW_TITLE, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER ),
  _elevationLayersList( 0x0 ),
	_addElevationLayerButton( 0x0 ),
	_removeElevationLayerButton( 0x0 ),
	_rasterLayersList( 0x0 ),
	_addRasterLayerButton( 0x0 ),
	_removeRasterLayerButton( 0x0 ),
	_sdbSizer1( 0x0 ),
	_sdbSizer1OK( 0x0 ),
	_sdbSizer1Cancel( 0x0 ),
  _elevationLayers(),
  _rasterLayers(),
  _rasterGroupCommand ( new ves::open::xml::Command ),
  _elevationGroupCommand ( new ves::open::xml::Command )
{
  _rasterGroupCommand->SetCommandName ( ves::util::commands::ADD_RASTER_GROUP );
  ves::conductor::UserPreferencesDataBuffer::instance()->SetCommand ( ves::util::commands::ADD_RASTER_GROUP, _rasterGroupCommand );

  _elevationGroupCommand->SetCommandName ( ves::util::commands::ADD_ELEVATION_GROUP );
  ves::conductor::UserPreferencesDataBuffer::instance()->SetCommand ( ves::util::commands::ADD_ELEVATION_GROUP, _elevationGroupCommand );

  this->SetSizeHints( wxDefaultSize, wxDefaultSize );
	
	wxBoxSizer* outerSizer;
	outerSizer = new wxBoxSizer( wxVERTICAL );
	
  // Create interface for elevation layers.
  {
	  wxStaticBoxSizer* elevationSizer;
	  elevationSizer = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Elevation") ), wxVERTICAL );
  	
	  _elevationLayersList = new wxListBox( this, MINERVA_DIALOG_ELEVATION_LAYER_LIST, wxDefaultPosition, wxDefaultSize, 0, NULL, 0 ); 
	  elevationSizer->Add( _elevationLayersList, 0, wxALL | wxEXPAND, 5 );
  	
	  wxGridSizer* buttonSizer;
    buttonSizer = new wxGridSizer( 3, 2, 0, 0 );

    wxButton* addFileSystem = new wxButton( this, MINERVA_DIALOG_ADD_ELEVATION_LAYER_FILE_SYSTEM, wxT("Add file..."), wxDefaultPosition, wxDefaultSize, 0 );
	  buttonSizer->Add( addFileSystem, 0, wxALL, 5 );
  	
	  _addElevationLayerButton = new wxButton( this, MINERVA_DIALOG_ADD_ELEVATION_WMS_LAYER, wxT("Add From WMS..."), wxDefaultPosition, wxDefaultSize, 0 );
	  buttonSizer->Add( _addElevationLayerButton, 0, wxALL, 5 );
  	
	  _removeElevationLayerButton = new wxButton( this, MINERVA_DIALOG_REMOVE_ELEVATION_LAYER, wxT("Remove"), wxDefaultPosition, wxDefaultSize, 0 );
	  buttonSizer->Add( _removeElevationLayerButton, 0, wxALL, 5 );
  	
	  elevationSizer->Add( buttonSizer, 1, wxEXPAND, 5 );
  	
	  outerSizer->Add( elevationSizer, 1, wxEXPAND, 5 );
  }

  // Create interface for raster layers.
  {
	  wxStaticBoxSizer* rasterSizer;
	  rasterSizer = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Raster") ), wxVERTICAL );
  	
	  _rasterLayersList = new wxListBox( this, MINERVA_DIALOG_RASTER_LAYER_LIST, wxDefaultPosition, wxDefaultSize, 0, NULL, 0 ); 
	  rasterSizer->Add( _rasterLayersList, 0, wxALL | wxEXPAND, 5 );
  	
	  wxGridSizer* buttonSizer;
	  buttonSizer = new wxGridSizer( 3, 2, 0, 0 );

    wxButton* addFileSystem = new wxButton( this, MINERVA_DIALOG_ADD_RASTER_LAYER_FILE_SYSTEM, wxT("Add file..."), wxDefaultPosition, wxDefaultSize, 0 );
	  buttonSizer->Add( addFileSystem, 0, wxALL, 5 );
  	
	  _addRasterLayerButton = new wxButton( this, MINERVA_DIALOG_ADD_RASTER_WMS_LAYER, wxT("Add From WMS..."), wxDefaultPosition, wxDefaultSize, 0 );
	  buttonSizer->Add( _addRasterLayerButton, 0, wxALL, 5 );
  	
	  _removeRasterLayerButton = new wxButton( this, MINERVA_DIALOG_REMOVE_RASTER_LAYER, wxT("Remove"), wxDefaultPosition, wxDefaultSize, 0 );
	  buttonSizer->Add( _removeRasterLayerButton, 0, wxALL, 5 );
  	
	  rasterSizer->Add( buttonSizer, 1, wxEXPAND, 5 );
  	
	  outerSizer->Add( rasterSizer, 1, wxEXPAND, 5 );
  }

	_sdbSizer1 = new wxStdDialogButtonSizer();
	_sdbSizer1OK = new wxButton( this, wxID_OK );
	_sdbSizer1->AddButton( _sdbSizer1OK );
	_sdbSizer1Cancel = new wxButton( this, wxID_CANCEL );
	_sdbSizer1->AddButton( _sdbSizer1Cancel );
	_sdbSizer1->Realize();
	outerSizer->Add( _sdbSizer1, 1, wxEXPAND, 5 );
	
	this->SetSizer( outerSizer );
	this->Layout();
	outerSizer->Fit( this );
}
///////////////////////////////////////////////////////////////////////////////
MinervaDialog::~MinervaDialog()
{
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::AddDefaultLayers()
{
  //const std::string server( "http://onearth.jpl.nasa.gov/wms.cgi" );
  //MinervaDialog::_addLayer( ves::util::commands::ADD_RASTER_LAYER, server, "BMNG,global_mosaic", "Jul,visual", "image/jpeg", _rasterLayersList, _rasterLayers, _rasterGroupCommand );

  const std::string server( "http://hypercube.telascience.org/cgi-bin/landsat7" );
  MinervaDialog::_addLayer( ves::util::commands::ADD_RASTER_LAYER, server, "landsat7", "", "image/jpeg", _rasterLayersList, _rasterLayers, _rasterGroupCommand );
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::AddElevationLayerWMS( wxCommandEvent& event )
{
    MinervaWmsDialog dialog( this, wxID_ANY );
    if( wxID_OK == dialog.ShowModal() )
    {
        const std::string server( dialog.server() );
        const std::string layers( dialog.layers() );
        const std::string styles( dialog.styles() );
        const std::string format( dialog.format() );
        MinervaDialog::_addLayer( ves::util::commands::ADD_ELEVATION_LAYER, server, layers, styles, format, _elevationLayersList, _elevationLayers, _elevationGroupCommand );
    }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::AddElevationLayerFileSystem( wxCommandEvent& event )
{
    wxFileDialog dialog( this, _("Open") );
    if( wxID_OK == dialog.ShowModal() )
    {
        wxFileName vegFileName( dialog.GetPath() );
        vegFileName.MakeRelativeTo( ::wxGetCwd() );
        wxString vegFileNamePath( vegFileName.GetFullPath() );
        vegFileNamePath.Replace( _( "\\" ), _( "/" ), true );

        MinervaDialog::_addLayerFileSystem( 
            ves::util::commands::ADD_ELEVATION_LAYER, 
            ConvertUnicode( vegFileNamePath.c_str() ), 
            _elevationLayersList, _elevationLayers, _elevationGroupCommand );
    }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::RemoveElevationLayer( wxCommandEvent& event )
{
    MinervaDialog::_removeLayer( ves::util::commands::REMOVE_ELEVATION_LAYER, _elevationLayersList, _elevationLayers, _elevationGroupCommand );
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::AddRasterLayerWMS( wxCommandEvent& event )
{
    MinervaWmsDialog dialog( this, wxID_ANY );
    if( wxID_OK == dialog.ShowModal() )
    {
        const std::string server( dialog.server() );
        const std::string layers( dialog.layers() );
        const std::string styles( dialog.styles() );
        const std::string format( dialog.format() );
        MinervaDialog::_addLayer( 
          ves::util::commands::ADD_RASTER_LAYER, server, layers, styles, format, _rasterLayersList, _rasterLayers, _rasterGroupCommand );
    }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::AddRasterLayerFileSystem( wxCommandEvent& event )
{
    wxFileDialog dialog( this, _("Open") );
    if( wxID_OK == dialog.ShowModal() )
    {
        wxFileName vegFileName( dialog.GetPath() );
        vegFileName.MakeRelativeTo( ::wxGetCwd() );
        wxString vegFileNamePath( vegFileName.GetFullPath() );
        vegFileNamePath.Replace( _( "\\" ), _( "/" ), true );
        
        MinervaDialog::_addLayerFileSystem( 
            ves::util::commands::ADD_RASTER_LAYER,  
            ConvertUnicode( vegFileNamePath.c_str() ), 
            _rasterLayersList, _rasterLayers, _rasterGroupCommand );
    }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::RemoveRasterLayer( wxCommandEvent& event )
{
    MinervaDialog::_removeLayer( ves::util::commands::REMOVE_RASTER_LAYER, _rasterLayersList, _rasterLayers, _rasterGroupCommand );
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::_addLayerFileSystem( 
  const std::string& commandName, 
  const std::string& filename, 
  wxListBox *layersList, 
  LayerIds &guids,
  ves::open::xml::CommandPtr groupCommand )
{
    ves::open::xml::CommandPtr command( new ves::open::xml::Command );
    command->SetCommandName( commandName );

    // Start adding the dave value pairs.
    ves::open::xml::DataValuePairPtr typeData( new ves::open::xml::DataValuePair );
    typeData->SetData( ves::util::names::LAYER_DATA_SOURCE, ves::util::values::FILESYSTEM_SOURCE );
    command->AddDataValuePair( typeData );

    ves::open::xml::DataValuePairPtr filenameData( new ves::open::xml::DataValuePair );
    filenameData->SetData( ves::util::names::FILENAME, filename );
    command->AddDataValuePair( filenameData );

    vpr::GUID guid;
    guid.generate();

    ves::open::xml::DataValuePairPtr guidData( new ves::open::xml::DataValuePair );
    guidData->SetData( ves::util::names::UNIQUE_ID, guid.toString() );
    command->AddDataValuePair( guidData );

    ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

    layersList->Append( wxString( filename.c_str(), wxConvUTF8 ) );
    guids.push_back( guid.toString() );

    groupCommand->AddDataValuePair ( Detail::MakeDVP ( guid.toString(), command ) );
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::_addLayer( 
  const std::string& commandName, 
  const std::string& server, 
  const std::string& layers, 
  const std::string& styles, 
  const std::string& format,
  wxListBox *layersList, 
  LayerIds &guids,
  ves::open::xml::CommandPtr groupCommand )
{
    // Make the command for adding a raster layer.
    ves::open::xml::CommandPtr command( new ves::open::xml::Command );
    command->SetCommandName( commandName );

    // Start adding the dave value pairs.
    ves::open::xml::DataValuePairPtr typeData( new ves::open::xml::DataValuePair );
    typeData->SetData( ves::util::names::LAYER_DATA_SOURCE, ves::util::values::WMS_SOURCE );
    command->AddDataValuePair( typeData );

    ves::open::xml::DataValuePairPtr serverData( new ves::open::xml::DataValuePair );
    serverData->SetData( ves::util::names::SERVER_URL, server );
    command->AddDataValuePair( serverData );

    ves::open::xml::DataValuePairPtr formatData( new ves::open::xml::DataValuePair );
    formatData->SetData( ves::util::names::WMS_FORMAT, format );
    command->AddDataValuePair( formatData );

    ves::open::xml::DataValuePairPtr layersData( new ves::open::xml::DataValuePair );
    layersData->SetData( ves::util::names::WMS_LAYERS, layers );
    command->AddDataValuePair( layersData );

    ves::open::xml::DataValuePairPtr stylesData( new ves::open::xml::DataValuePair );
    stylesData->SetData( ves::util::names::WMS_STYLES, styles );
    command->AddDataValuePair( stylesData );

    vpr::GUID guid;
    guid.generate();

    ves::open::xml::DataValuePairPtr guidData( new ves::open::xml::DataValuePair );
    guidData->SetData( ves::util::names::UNIQUE_ID, guid.toString() );
    command->AddDataValuePair( guidData );

    ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

    layersList->Append( wxString( server.c_str(), wxConvUTF8 ) );
    guids.push_back( guid.toString() );

    groupCommand->AddDataValuePair ( Detail::MakeDVP ( guid.toString(), command ) );
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::_removeLayer( 
  const std::string& commandName, 
  wxListBox *layersList, 
  LayerIds &guids,
  ves::open::xml::CommandPtr groupCommand )
{
    wxArrayInt selectedIndices;
    layersList->GetSelections( selectedIndices );

    if( selectedIndices.Count() > 0 )
    {
        int index( selectedIndices[0] );
        std::string guid( guids.at( index ) );

        guids.erase( guids.begin() + index );
        layersList->Delete( index );

        ves::open::xml::CommandPtr command( new ves::open::xml::Command );
        command->SetCommandName( commandName );

        ves::open::xml::DataValuePairPtr guidData( new ves::open::xml::DataValuePair );
        guidData->SetData( ves::util::names::UNIQUE_ID, guid );
        command->AddDataValuePair( guidData );

        ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

        groupCommand->RemoveDataValuePair ( guid );
    }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::InitalizeFromCommands( ves::open::xml::CommandPtr elevationGroupCommand, ves::open::xml::CommandPtr rasterGroupCommand )
{
  if ( elevationGroupCommand )
  {
    _elevationGroupCommand = elevationGroupCommand;
    ves::conductor::UserPreferencesDataBuffer::instance()->SetCommand ( _elevationGroupCommand->GetCommandName(), _elevationGroupCommand );
    MinervaDialog::_initializeFromCommand ( elevationGroupCommand, _elevationLayersList, _elevationLayers );
  }

  if ( rasterGroupCommand )
  {
    _rasterGroupCommand = rasterGroupCommand;
    ves::conductor::UserPreferencesDataBuffer::instance()->SetCommand ( _rasterGroupCommand->GetCommandName(), _rasterGroupCommand );
    MinervaDialog::_initializeFromCommand ( rasterGroupCommand, _rasterLayersList, _rasterLayers );
  }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::_initializeFromCommand (
      ves::open::xml::CommandPtr groupCommand,
      wxListBox *layersList, 
      LayerIds &guids )
{
  if ( groupCommand )
  {
    const std::size_t numLayers ( groupCommand->GetNumberOfDataValuePairs() );
    for ( std::size_t i = 0; i < numLayers; ++i )
    {
      ves::open::xml::DataValuePairPtr dvp ( groupCommand->GetDataValuePair ( i ) );
      if ( dvp )
      {
        ves::open::xml::CommandPtr commandForLayer ( boost::dynamic_pointer_cast<ves::open::xml::Command> ( dvp->GetDataXMLObject() ) );
        if ( commandForLayer )
        {
          ves::open::xml::DataValuePairPtr filenameDVP ( commandForLayer->GetDataValuePair ( ves::util::names::FILENAME ) );
          ves::open::xml::DataValuePairPtr serverDVP ( commandForLayer->GetDataValuePair ( ves::util::names::SERVER_URL ) );

          std::string name ( "unnamed" );
          if ( filenameDVP )
          {
            filenameDVP->GetData ( name );
          }
          else if ( serverDVP )
          {
            serverDVP->GetData ( name );
          }

          ves::open::xml::DataValuePairPtr guidDVP ( commandForLayer->GetDataValuePair ( ves::util::names::UNIQUE_ID ) );
          if ( guidDVP )
          {
            std::string guid;
            guidDVP->GetData ( guid );

            layersList->Append( wxString( name.c_str(), wxConvUTF8 ) );
            guids.push_back ( guid );
          }
        }
      }
    }
  }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::NavigateToElevationLayer( wxCommandEvent& event )
{
    wxArrayInt selectedIndices;
    _elevationLayersList->GetSelections( selectedIndices );

    if( selectedIndices.Count() > 0 )
    {
        int index( selectedIndices[0] );
        std::string guid( _elevationLayers.at( index ) );

        ves::open::xml::CommandPtr command( new ves::open::xml::Command );
        command->SetCommandName( ves::util::commands::NAVIGATE_TO_LAYER );

        ves::open::xml::DataValuePairPtr guidData( new ves::open::xml::DataValuePair );
        guidData->SetData( ves::util::names::UNIQUE_ID, guid );
        command->AddDataValuePair( guidData );

        ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( command );
    }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::NavigateToRasterLayer( wxCommandEvent& event )
{
    wxArrayInt selectedIndices;
    _rasterLayersList->GetSelections( selectedIndices );

    if( selectedIndices.Count() > 0 )
    {
        int index( selectedIndices[0] );
        std::string guid( _rasterLayers.at( index ) );

        ves::open::xml::CommandPtr command( new ves::open::xml::Command );
        command->SetCommandName( ves::util::commands::NAVIGATE_TO_LAYER );

        ves::open::xml::DataValuePairPtr guidData( new ves::open::xml::DataValuePair );
        guidData->SetData( ves::util::names::UNIQUE_ID, guid );
        command->AddDataValuePair( guidData );

        ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( command );
    }
}
