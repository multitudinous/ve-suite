//--- Header Include ---//
#include "LoadDataGUI.h"

//--- wxWidgets Includes ---// - For GUI Construction
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/filepicker.h>
#include <wx/statline.h>
#include <wx/button.h>
#include <wx/panel.h>
#include <wx/notebook.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/treebase.h>
#include <wx/treectrl.h>

// --- wxWidgets Includes --- // - For File Handling
//#include <wx/filename.h>

// --- C++ Includes --- //
#include <iostream>
#include <vector>

// --- This Plugin Includes --- //
#include "VascularSimViewerUIDialog.h"
#include "DataSet.h"

using namespace vascularsimviewer;

BEGIN_EVENT_TABLE( LoadDataGUI, wxDialog )
END_EVENT_TABLE()


/// Constructor
LoadDataGUI::LoadDataGUI( wxWindow* parent,	wxWindowID id, const wxString& title, VascularSimViewerUIDialog* UIDialog )
    :
    wxDialog( parent, id, title,
        wxDefaultPosition,
        wxDefaultSize,
        wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxCLOSE_BOX | wxMAXIMIZE_BOX | wxMINIMIZE_BOX ),
    VascSimViewUIDialog( UIDialog )
{
    BuildDialog();
}

// Desctructor
LoadDataGUI::~LoadDataGUI()
{
    Disconnect( wxID_EXIT, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( LoadDataGUI::OnClose ) );
    Disconnect( LOAD_VTK_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( LoadDataGUI::OnLoadVTUFile ) );
}

/// Construct GUI
void LoadDataGUI::BuildDialog()
{
    // Created w/ wxFormBuilder
    this->SetSizeHints( wxDefaultSize, wxDefaultSize );

    wxBoxSizer* mainSizer = new wxBoxSizer( wxVERTICAL );

    //Notebook with Panels - START
    wxNotebook* notebook = new wxNotebook( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, 0 );
	
    //Load Imaging Data Panel - START
    wxPanel* LoadImageDataPanel = new wxPanel( notebook, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL );
    wxBoxSizer* imageDataSizer = new wxBoxSizer( wxVERTICAL );
	
    imageDataSizer->Add( 0, 10, 0, wxEXPAND, 5 ); //Load Imaging Data Panel - Top Spacer
	
    //Add Select Volview Text
    wxStaticText* staticTextVolview = new wxStaticText( LoadImageDataPanel, wxID_ANY, wxT("Select VolView .vvi File"), wxDefaultPosition, wxDefaultSize, 0 );
    staticTextVolview->Wrap( -1 );
    imageDataSizer->Add( staticTextVolview, 0, wxALIGN_BOTTOM|wxALL, 5 );
    
    //Add File Picker for Volview Volumes
    wxFilePickerCtrl* volViewFilePicker = new wxFilePickerCtrl( LoadImageDataPanel, wxID_ANY, wxEmptyString, wxT("Select a file (.vvi)"), wxT("*.*"), wxDefaultPosition, wxDefaultSize, wxFLP_DEFAULT_STYLE );
    imageDataSizer->Add( volViewFilePicker, 0, wxALL|wxEXPAND, 2 );
	
    imageDataSizer->Add( 0, 15, 0, wxEXPAND, 5 ); //Middle Spacer
	
    //Add Select DICOM Directory Text
    wxStaticText* staticTextDICOM = new wxStaticText( LoadImageDataPanel, wxID_ANY, wxT("Select DICOM Directory"), wxDefaultPosition, wxDefaultSize, 0 );
    staticTextDICOM->Wrap( -1 );
    imageDataSizer->Add( staticTextDICOM, 0, wxALIGN_BOTTOM|wxALL, 5 );
	
    //Add DICOM Directory Picker
    wxDirPickerCtrl* dICOMDirPicker = new wxDirPickerCtrl( LoadImageDataPanel, wxID_ANY, wxEmptyString, wxT("Select a folder"), wxDefaultPosition, wxDefaultSize, wxDIRP_DEFAULT_STYLE );
    imageDataSizer->Add( dICOMDirPicker, 0, wxALL|wxEXPAND, 2 );
	
    imageDataSizer->Add( 0, 10, 0, wxEXPAND, 5 ); //Another Spacer
	
    //Static Line
    wxStaticLine* imgDataStaticLine = new wxStaticLine( LoadImageDataPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
    imageDataSizer->Add( imgDataStaticLine, 0, wxEXPAND | wxALL, 5 );
	
    imageDataSizer->Add( 0, 10, 0, wxEXPAND, 5 );  //Spacer
	
    //Load Imaging Data Button
    wxButton* loadImagingDataButton = new wxButton( LoadImageDataPanel, wxID_ANY, wxT("Load Imaging Data"), wxDefaultPosition, wxDefaultSize, 0 );
    imageDataSizer->Add( loadImagingDataButton, 0, wxALL|wxEXPAND, 5 );
	
    LoadImageDataPanel->SetSizer( imageDataSizer );
    LoadImageDataPanel->Layout();
    imageDataSizer->Fit( LoadImageDataPanel );
    notebook->AddPage( LoadImageDataPanel, wxT("Load Imaging Data"), false ); //Add Panel to Notebook
    //Load Imaging Data Panel - END
	
	
    //Load CFD Simulation Data Panel - START
    wxPanel* LoadSimulationDataPanel = new wxPanel( notebook, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL );
    wxBoxSizer* simDataSizer = new wxBoxSizer( wxVERTICAL );
	
    simDataSizer->Add( 0, 10, 0, wxEXPAND, 5 ); //Spacer
	
    //Add Transient Data Text
    wxStaticText* staticTextVTU = new wxStaticText( LoadSimulationDataPanel, wxID_ANY, wxT("Select Transient Simulation Directory (.vtu Files)"), wxDefaultPosition, wxDefaultSize, 0 );
    staticTextVTU->Wrap( -1 );
    simDataSizer->Add( staticTextVTU, 0, wxALIGN_BOTTOM|wxALL, 5 );
	
    //Add Transient Data Directory Picker
    wxDirPickerCtrl* vtuDirPicker = new wxDirPickerCtrl( LoadSimulationDataPanel, wxID_ANY, wxEmptyString, wxT("Select a folder"), wxDefaultPosition, wxDefaultSize, wxDIRP_DEFAULT_STYLE );
    simDataSizer->Add( vtuDirPicker, 0, wxALL|wxEXPAND, 5 );
	
    //Add Load Directory Button
    wxButton* LoadvtuDirButton = new wxButton( LoadSimulationDataPanel, wxID_ANY, wxT("Load Directory"), wxDefaultPosition, wxDefaultSize, 0 );
    simDataSizer->Add( LoadvtuDirButton, 0, wxALL|wxEXPAND, 5 );
	
    //Add static line
    wxStaticLine* simDataStaticLine = new wxStaticLine( LoadSimulationDataPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
    simDataSizer->Add( simDataStaticLine, 0, wxEXPAND | wxALL, 5 );
	
    simDataSizer->Add( 0, 10, 0, wxEXPAND, 5 ); //Spacer
	
    //Add Select vtk File Text
    wxStaticText* staticTextVTK = new wxStaticText( LoadSimulationDataPanel, wxID_ANY, wxT("Select Static Simulation File (TAWSS and OSI .vtk Files)"), wxDefaultPosition, wxDefaultSize, 0 );
    staticTextVTK->Wrap( -1 );
    simDataSizer->Add( staticTextVTK, 0, wxALIGN_BOTTOM|wxALL, 5 );
	
    //Add vtk File Picker
    vtkFilePicker = new wxFilePickerCtrl( LoadSimulationDataPanel, wxID_ANY, wxEmptyString, wxT("Select a file (.vvi)"), wxT("*.*"), wxDefaultPosition, wxDefaultSize, wxFLP_DEFAULT_STYLE );
    simDataSizer->Add( vtkFilePicker, 0, wxALL|wxEXPAND, 5 );
	
    //Add Load File Button
    wxButton* LoadFileButton = new wxButton( LoadSimulationDataPanel, LOAD_VTK_COMMAND, wxT("Load File"), wxDefaultPosition, wxDefaultSize, 0 );
    simDataSizer->Add( LoadFileButton, 0, wxALL|wxEXPAND, 5 );
	
    LoadSimulationDataPanel->SetSizer( simDataSizer );
    LoadSimulationDataPanel->Layout();
    simDataSizer->Fit( LoadSimulationDataPanel );
    notebook->AddPage( LoadSimulationDataPanel, wxT("Load Simulation Data"), true );
    //Load CFD Simulation Data Panel - END


    //Load Pressure Wave Data Panel - START
    wxPanel* LoadPressureWavePanel = new wxPanel( notebook, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL );
    notebook->AddPage( LoadPressureWavePanel, wxT("Load Pressure Wave"), false );
    //Load Pressure Wave Data Panel - END
	
    mainSizer->Add( notebook, 1, wxEXPAND | wxALL, 5 );
	
    //Bottom of window
    wxBoxSizer* bottomSizer = new wxBoxSizer( wxHORIZONTAL );
	
    bottomSizer->Add( 0, 0, 1, wxEXPAND, 5 ); //Spacer
	
    //Add Close Button
    wxButton* CloseButton = new wxButton( this, wxID_EXIT, wxT("Close"), wxDefaultPosition, wxDefaultSize, 0 );
    bottomSizer->Add( CloseButton, 0, wxALL, 5 );
	
    mainSizer->Add( bottomSizer, 0, wxEXPAND, 5 );
	
    this->SetSizer( mainSizer );
    this->Layout();
    mainSizer->Fit( this );
	mainSizer->SetSizeHints( this );
	CenterOnParent();
	
    //Functionality Connections
    Connect( wxID_EXIT, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( LoadDataGUI::OnClose ) );
    Connect( LOAD_VTK_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( LoadDataGUI::OnLoadVTUFile ) );
}

//Close Dialog
void LoadDataGUI::OnClose( wxCommandEvent &event )
{
	Close();
}

//Loading static vtk file
void LoadDataGUI::OnLoadVTUFile( wxCommandEvent &event )
{
    wxString _FilePathAndName = vtkFilePicker->GetPath().c_str(); // Get Filename and path from the filepicker dialog
    std::string _convertedFilename = ConvertUnicode( _FilePathAndName );  // Convert filename and path to Unicode
    //std::cout << _convertedFilename << std::endl;
    
    if( FileExists( _convertedFilename ) ) //Does the file exist
    {
        if( !FileIsAlreadyLoaded( _convertedFilename ) ) //has the file already been loaded
        {
            VascSimViewUIDialog->AddDataSet( _convertedFilename ); //if no - create new dataset from filename
        }
        else
        {
            // Get already loaded DataSelectionDialog to pop up
            ShowLoadedDataSetDialog( _convertedFilename );    //if yes - show the array selection dialog for already loaded dataset
            
            std::cout << "File is Already Loaded" << std::endl;
        }
    }
    else
    {
        std::cout << "File Does NOT Exist" << std::endl;
    }
}

////////////////////////////////////////////
bool LoadDataGUI::FileExists( std::string filename )
{
    bool _exists;
    _exists = wxFileExists( wxT( filename ) );
    
    return _exists;
}

/////////////////////////////////////////////
bool LoadDataGUI::FileIsAlreadyLoaded( std::string filename )
{
    
    DataSet* _dataSet = VascSimViewUIDialog->GetDataSetByFileName( filename );
    
    if( _dataSet != NULL )
    {
            return true;
    }

    return false;    
}


/////////////////////////////////////////////
void LoadDataGUI::ShowLoadedDataSetDialog( std::string filename )
{
    DataSet* _dataSet = VascSimViewUIDialog->GetDataSetByFileName( filename );
    
    _dataSet->ShowDialog();
}










