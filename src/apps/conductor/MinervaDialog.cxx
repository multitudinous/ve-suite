
///////////////////////////////////////////////////////////////////////////////
//
//  Dialog for Minerva properties.
//
///////////////////////////////////////////////////////////////////////////////

#include "MinervaDialog.h"
#include "MinervaWmsDialog.h"

#include <ves/conductor/util/CORBAServiceList.h>

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
#include <wx/listbox.h>
#include <wx/string.h>
#include <wx/gdicmn.h>
#include <wx/font.h>
#include <wx/colour.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statbox.h>

const wxString WINDOW_TITLE ( wxT( "Minerva Properties" ) );

///////////////////////////////////////////////////////////////////////////////
//
//  Event Ids.
//
///////////////////////////////////////////////////////////////////////////////

enum
{
  MINERVA_DIALOG_ADD_ELEVATION_LAYER,
  MINERVA_DIALOG_REMOVE_ELEVATION_LAYER,
  MINERVA_DIALOG_ADD_RASTER_LAYER,
  MINERVA_DIALOG_REMOVE_RASTER_LAYER
};


///////////////////////////////////////////////////////////////////////////////
//
//  Event table.
//
///////////////////////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( MinervaDialog, wxDialog )
    EVT_BUTTON( MINERVA_DIALOG_ADD_ELEVATION_LAYER, MinervaDialog::AddElevationLayer )
    EVT_BUTTON( MINERVA_DIALOG_REMOVE_ELEVATION_LAYER, MinervaDialog::RemoveElevationLayer )
    EVT_BUTTON( MINERVA_DIALOG_ADD_RASTER_LAYER, MinervaDialog::AddRasterLayer )
    EVT_BUTTON( MINERVA_DIALOG_REMOVE_RASTER_LAYER, MinervaDialog::RemoveRasterLayer )
END_EVENT_TABLE()


///////////////////////////////////////////////////////////////////////////////
MinervaDialog::MinervaDialog ( 
  wxWindow *parent, 
  wxWindowID id ) : BaseClass ( parent, id, WINDOW_TITLE, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE ),
  _elevationLayersList ( 0x0 ),
	_addElevationLayerButton ( 0x0 ),
	_removeElevationLayerButton ( 0x0 ),
	_rasterLayersList ( 0x0 ),
	_addRasterLayerButton ( 0x0 ),
	_removeRasterLayerButton ( 0x0 ),
	_sdbSizer1 ( 0x0 ),
	_sdbSizer1OK ( 0x0 ),
	_sdbSizer1Cancel ( 0x0 ),
  _elevationLayers(),
  _rasterLayers()
{
  this->SetSizeHints( wxDefaultSize, wxDefaultSize );
	
	wxBoxSizer* outerSizer;
	outerSizer = new wxBoxSizer( wxVERTICAL );
	
  // Create interface for elevation layers.
  {
	  wxStaticBoxSizer* elevationSizer;
	  elevationSizer = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Elevation") ), wxVERTICAL );
  	
	  _elevationLayersList = new wxListBox( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, 0, NULL, 0 ); 
	  elevationSizer->Add( _elevationLayersList, 0, wxALL, 5 );
  	
	  wxGridSizer* buttonSizer;
	  buttonSizer = new wxGridSizer( 2, 2, 0, 0 );
  	
	  _addElevationLayerButton = new wxButton( this, MINERVA_DIALOG_ADD_ELEVATION_LAYER, wxT("Add From WMS..."), wxDefaultPosition, wxDefaultSize, 0 );
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
  	
	  _rasterLayersList = new wxListBox( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, 0, NULL, 0 ); 
	  rasterSizer->Add( _rasterLayersList, 0, wxALL, 5 );
  	
	  wxGridSizer* buttonSizer;
	  buttonSizer = new wxGridSizer( 2, 2, 0, 0 );
  	
	  _addRasterLayerButton = new wxButton( this, MINERVA_DIALOG_ADD_RASTER_LAYER, wxT("Add From WMS..."), wxDefaultPosition, wxDefaultSize, 0 );
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
  const std::string server ( "http://serv.asu.edu/cgi-bin/tilecache-2.03/tilecache.cgi" );
  MinervaDialog::_addLayer ( ves::util::commands::ADD_RASTER_LAYER, server, "OpenAerialMap", "", "image/jpeg", _rasterLayersList, _rasterLayers );
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::AddElevationLayer ( wxCommandEvent& event )
{
  MinervaWmsDialog dialog ( this, wxID_ANY );
  if ( wxID_OK == dialog.ShowModal() )
  {
    const std::string server ( dialog.server() );
    const std::string layers ( dialog.layers() );
    const std::string styles ( dialog.styles() );
    const std::string format ( dialog.format() );
    MinervaDialog::_addLayer ( ves::util::commands::ADD_ELEVATION_LAYER, server, layers, styles, format, _elevationLayersList, _elevationLayers );
  }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::RemoveElevationLayer ( wxCommandEvent& event )
{
  MinervaDialog::_removeLayer ( ves::util::commands::REMOVE_ELEVATION_LAYER, _rasterLayersList, _rasterLayers );
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::AddRasterLayer ( wxCommandEvent& event )
{
  MinervaWmsDialog dialog ( this, wxID_ANY );
  if ( wxID_OK == dialog.ShowModal() )
  {
    const std::string server ( dialog.server() );
    const std::string layers ( dialog.layers() );
    const std::string styles ( dialog.styles() );
    const std::string format ( dialog.format() );
    MinervaDialog::_addLayer ( ves::util::commands::ADD_RASTER_LAYER, server, layers, styles, format, _rasterLayersList, _rasterLayers );
  }
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::RemoveRasterLayer ( wxCommandEvent& event )
{
  MinervaDialog::_removeLayer ( ves::util::commands::REMOVE_RASTER_LAYER, _rasterLayersList, _rasterLayers );
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::_addLayer ( 
  const std::string& commandName, 
  const std::string& server, 
  const std::string& layers, 
  const std::string& styles, 
  const std::string& format,
  wxListBox *layersList, 
  LayerIds &guids )
{
  // Make the command for adding a raster layer.
  ves::open::xml::CommandPtr command ( new ves::open::xml::Command );
  command->SetCommandName ( commandName );

  // Start adding the dave value pairs.
  ves::open::xml::DataValuePairPtr serverData ( new ves::open::xml::DataValuePair );
  serverData->SetData ( ves::util::names::SERVER_URL, server );
  command->AddDataValuePair ( serverData );

  ves::open::xml::DataValuePairPtr formatData ( new ves::open::xml::DataValuePair );
  formatData->SetData ( ves::util::names::WMS_FORMAT, format );
  command->AddDataValuePair ( formatData );

  ves::open::xml::DataValuePairPtr layersData ( new ves::open::xml::DataValuePair );
  layersData->SetData ( ves::util::names::WMS_LAYERS, layers );
  command->AddDataValuePair ( layersData );

  ves::open::xml::DataValuePairPtr stylesData ( new ves::open::xml::DataValuePair );
  stylesData->SetData ( ves::util::names::WMS_STYLES, styles );
  command->AddDataValuePair ( stylesData );

  vpr::GUID guid;
  guid.generate();

  ves::open::xml::DataValuePairPtr guidData ( new ves::open::xml::DataValuePair );
  guidData->SetData ( ves::util::names::UNIQUE_ID, guid.toString() );
  command->AddDataValuePair ( guidData );

  ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

  layersList->Append ( wxString( server.c_str(), wxConvUTF8 ) );
  guids.push_back ( guid.toString() );
}
///////////////////////////////////////////////////////////////////////////////
void MinervaDialog::_removeLayer ( const std::string& commandName, wxListBox *layersList, LayerIds &guids )
{
  wxArrayInt selectedIndices;
  layersList->GetSelections ( selectedIndices );

  if ( selectedIndices.Count() > 0 )
  {
    int index ( selectedIndices[0] );
    std::string guid ( guids.at ( index ) );

    guids.erase ( guids.begin() + index );
    layersList->Delete ( index );

    ves::open::xml::CommandPtr command ( new ves::open::xml::Command );
    command->SetCommandName ( commandName );

    ves::open::xml::DataValuePairPtr guidData ( new ves::open::xml::DataValuePair );
    guidData->SetData ( ves::util::names::UNIQUE_ID, guid );
    command->AddDataValuePair ( guidData );

    ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( command );
  }
}
