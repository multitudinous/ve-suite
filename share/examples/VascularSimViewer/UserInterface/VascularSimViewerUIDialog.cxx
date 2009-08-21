
// --- Header Include --- //
#include "VascularSimViewerUIDialog.h"

// --- This Plugin Includes --- //
#include "LoadDataGUI.h"
#include "DataSet.h"

// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/treectrl.h>
#include <wx/sizer.h>
#include <wx/treebase.h>
#include <wx/button.h>
#include <wx/statline.h>
#include <wx/checkbox.h>
#include <wx/filename.h>

// --- C++ Includes --- //
#include <iostream>

using namespace vascularsimviewer;

BEGIN_EVENT_TABLE( VascularSimViewerUIDialog, wxDialog )
END_EVENT_TABLE()


/// Constructor
VascularSimViewerUIDialog::VascularSimViewerUIDialog(wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
    :
    UIDialog( parent, id, wxT( "VascularSimViewerUIDialog" ) ),
    mServiceList( service ),
    mLoadDataGUI( NULL ),
    _rootID( NULL )
{
    BuildDialog();
}

// Desctructor
VascularSimViewerUIDialog::~VascularSimViewerUIDialog()
{
    Disconnect( wxID_EXIT, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnClose ) );
    Disconnect( LOAD_DATA_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnLoadData ) );
    Disconnect( MANIPULATE_DATA_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnManipulateData ) );
    Disconnect( VISUALIZE_DATA_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnVisualizeData ) );
    Disconnect( REMOVE_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnRemove ) );
    Disconnect( CLEAR_ALL_VIS_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnClearAllVis ) );
    Disconnect( CLEAR_ALL_DATA_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnClearAllData ) );
    Disconnect( BOUNDING_BOX_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnBoundingBox ) );
    Disconnect( LEGEND_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnLegend ) );
    Disconnect( EDIT_LEGEND_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnEditLegend ) );
}

/// Construct GUI
void VascularSimViewerUIDialog::BuildDialog()
{
//This function was basically built with wxFormBuilder

    this->SetSizeHints( wxDefaultSize, wxDefaultSize );
	
    wxBoxSizer* mainSizer = new wxBoxSizer( wxVERTICAL );
	
    wxBoxSizer* topSizer = new wxBoxSizer( wxHORIZONTAL );
	
    //Create SimTree
    SimulationTreeCtrl = new wxTreeCtrl( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTR_DEFAULT_STYLE|wxTR_EDIT_LABELS|wxTR_HIDE_ROOT );
    topSizer->Add( SimulationTreeCtrl, 1, wxALIGN_CENTER_HORIZONTAL|wxALL|wxEXPAND, 5 );
	
	_rootID = SimulationTreeCtrl->AddRoot( wxT("Root Node"), -1, -1, NULL);
	
    //Right Side Buttons - Start
    wxBoxSizer* rightButtonsSizer = new wxBoxSizer( wxVERTICAL );
	
    //Load Data Button
    wxButton* LoadDataButton = new wxButton( this, LOAD_DATA_COMMAND, wxT("Load Data"), wxDefaultPosition, wxDefaultSize, 0 );
    rightButtonsSizer->Add( LoadDataButton, 1, wxALL|wxEXPAND, 5 );
	
    //Static Line - Top
    wxStaticLine* staticline1 = new wxStaticLine( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
    rightButtonsSizer->Add( staticline1, 0, wxEXPAND | wxALL, 5 );
	
    //Maniupulate Data Button
    wxButton* ManipulateDataButton = new wxButton( this, MANIPULATE_DATA_COMMAND, wxT("Manipulate Data"), wxDefaultPosition, wxDefaultSize, 0 );
    rightButtonsSizer->Add( ManipulateDataButton, 1, wxALL|wxEXPAND, 5 );
	
    //Visualize Data Button
    wxButton* VisualizeDataButton = new wxButton( this, VISUALIZE_DATA_COMMAND, wxT("Visualize Data"), wxDefaultPosition, wxDefaultSize, 0 );
    rightButtonsSizer->Add( VisualizeDataButton, 1, wxALL|wxEXPAND, 5 );
	
    //Static Line - Middle 
    wxStaticLine* statline2 = new wxStaticLine( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
    rightButtonsSizer->Add( statline2, 0, wxEXPAND | wxALL, 5 );
    
    //Remove Button
    wxButton* RemoveButton = new wxButton( this, REMOVE_COMMAND, wxT("Remove"), wxDefaultPosition, wxDefaultSize, 0 );
    rightButtonsSizer->Add( RemoveButton, 1, wxALL|wxEXPAND, 5 );
    
    //Clear All Vis Button
    wxButton* ClearAllVisButton = new wxButton( this, CLEAR_ALL_VIS_COMMAND, wxT("Clear All Vis"), wxDefaultPosition, wxDefaultSize, 0 );
    rightButtonsSizer->Add( ClearAllVisButton, 1, wxALL|wxEXPAND, 5 );
    
    //Clear All Data Button
    wxButton* ClearAllDataButton = new wxButton( this, CLEAR_ALL_DATA_COMMAND, wxT("Clear All Data"), wxDefaultPosition, wxDefaultSize, 0 );
    rightButtonsSizer->Add( ClearAllDataButton, 1, wxALL|wxEXPAND, 5 );
    //Right Side Buttons - End
	
	
    topSizer->Add( rightButtonsSizer, 0, wxEXPAND, 5 );
	
    mainSizer->Add( topSizer, 0, wxEXPAND, 5 );
	
    wxBoxSizer* midSizer;
    midSizer = new wxBoxSizer( wxHORIZONTAL );
	
    //Middle Buttons - Start
    //Bounding Box Ckeck
    wxCheckBox* BoundingBoxCheckBox = new wxCheckBox( this, BOUNDING_BOX_COMMAND, wxT("Bounding Box"), wxDefaultPosition, wxDefaultSize, 0 );
    midSizer->Add( BoundingBoxCheckBox, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	
    //Legend Check
    wxCheckBox* LegendCheckBox = new wxCheckBox( this, LEGEND_COMMAND, wxT("Legend"), wxDefaultPosition, wxDefaultSize, 0 );
    midSizer->Add( LegendCheckBox, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	
    //Edit Legend Button
    wxButton* EditLegendButton = new wxButton( this, EDIT_LEGEND_COMMAND, wxT("Edit Legend"), wxDefaultPosition, wxDefaultSize, 0 );
    midSizer->Add( EditLegendButton, 0, wxALL, 5 );
    //Middle Buttons - End
	
    mainSizer->Add( midSizer, 0, wxEXPAND, 5 );
	
    //Bottom Static Line
    wxStaticLine* statline3 = new wxStaticLine( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
    mainSizer->Add( statline3, 0, wxEXPAND | wxALL, 5 );
    
    wxBoxSizer* bottomSizer;
    bottomSizer = new wxBoxSizer( wxHORIZONTAL );
	
    //Spacer left of Close Button
    bottomSizer->Add( 0, 0, 1, wxEXPAND, 5 );
	
    //Close Button
    wxButton* CloseButton = new wxButton( this, wxID_EXIT, wxT("Close"), wxDefaultPosition, wxDefaultSize, 0 );
    bottomSizer->Add( CloseButton, 0, wxALL, 5 );
	
    mainSizer->Add( bottomSizer, 0, wxEXPAND, 5 );
	
    this->SetSizer( mainSizer );
    this->Layout();
    mainSizer->Fit( this );
    mainSizer->SetSizeHints( this );
    CenterOnParent();

	
    //Functionality Connections - Dynamic Connections ( I think a EVENT TABLE could also work but not sure )
    Connect( wxID_EXIT, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnClose ) );
    Connect( LOAD_DATA_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnLoadData ) );
    Connect( MANIPULATE_DATA_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnManipulateData ) );
    Connect( VISUALIZE_DATA_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnVisualizeData ) );
    Connect( REMOVE_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnRemove ) );
    Connect( CLEAR_ALL_VIS_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnClearAllVis ) );
    Connect( CLEAR_ALL_DATA_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnClearAllData ) );
    Connect( BOUNDING_BOX_COMMAND, wxEVT_COMMAND_CHECKBOX_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnBoundingBox ) );
    Connect( LEGEND_COMMAND, wxEVT_COMMAND_CHECKBOX_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnLegend ) );
    Connect( EDIT_LEGEND_COMMAND, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( VascularSimViewerUIDialog::OnEditLegend ) );

}

//Close Dialog
void VascularSimViewerUIDialog::OnClose( wxCommandEvent &event )
{
    Close();
}

//Start Load Data Dialog
void VascularSimViewerUIDialog::OnLoadData( wxCommandEvent &event )
{
    //Check if the Dialog is already running
    if( mLoadDataGUI != NULL) {
    	
        mLoadDataGUI->Show(); //Display existing Dialog on top of other windows - Show() is inherited from wxDialog
		
        return;
    }
	
    //Create the LoadDataGUI Dialog
    mLoadDataGUI = new LoadDataGUI( this, wxID_ANY, wxT("Load Data"), this );
	
    mLoadDataGUI->Show();
}


//Manipulate Data Dialog
void VascularSimViewerUIDialog::OnManipulateData( wxCommandEvent &event )
{
     std::cout << "This Will Start the Manipulate Data Dialog" << std::endl;
}

//Visualize Data Dialog
void VascularSimViewerUIDialog::OnVisualizeData( wxCommandEvent &event )
{
    std::cout << "This Will Start the Visualize Data Dialog" << std::endl;
}
	
//Remove Something from simulation tree
void VascularSimViewerUIDialog::OnRemove( wxCommandEvent &event )
{
    wxTreeItemId selectedItem = SimulationTreeCtrl->GetSelection(); //In wx2.9 change to GetFocusedItem()
    
    SimulationTreeCtrl->Delete( selectedItem );
}
	
//Clear all visualizations but leave data
void VascularSimViewerUIDialog::OnClearAllVis( wxCommandEvent &event )
{
    std::cout << "This Will Remove all Visualizations from the tree and Xplorer" << std::endl;
}

//Clear all data and visualizations
void VascularSimViewerUIDialog::OnClearAllData( wxCommandEvent &event )
{
    std::cout << "This Will Clear the Entire Tree and Xplorer" << std::endl;
}
	
//Place Bounding Box around data set
void VascularSimViewerUIDialog::OnBoundingBox( wxCommandEvent &event )
{
    std::cout << "This Will Place a Box Around the Data" << std::endl;
}

//Add legend to data set
void VascularSimViewerUIDialog::OnLegend( wxCommandEvent &event )
{
    std::cout << "This Will Create a Legend for a Specific Visualization" << std::endl;
}

//Edit Legend
void VascularSimViewerUIDialog::OnEditLegend( wxCommandEvent &event )
{
    std::cout << "This Will Start an Edit Legend UI" << std::endl;
}

//Add DataSet to SimTree
void VascularSimViewerUIDialog::AddDataSet( const std::string &filename )
{
    wxFileName _tmpTreeName( filename );  //filename contains the full file path
    wxString TreeName = _tmpTreeName.GetFullName();  //Truncated the filename to just the name and extension to be shown in the tree
    
    // Adds DataSet to the tree
    wxTreeItemId TreeId = SimulationTreeCtrl->AppendItem( _rootID, wxT( TreeName ), -1, -1, new DataSet( filename, this, mServiceList ) );
    
    // Adds DataSet to the vector of DataSetIds - Not sure if this is needed
    DataSetIds.push_back( TreeId );
}

//Return Number of DataSets
int VascularSimViewerUIDialog::GetNumDataSets()
{
    return DataSetIds.size();
}

//Return a pointer to a Dataset
std::vector< wxTreeItemId >* VascularSimViewerUIDialog::GetDataSets()
{
    return &DataSetIds;
}

//Return a pointer to a DataSet
DataSet* VascularSimViewerUIDialog::GetDataSetByFileName( const std::string& filename )
{    
    std::vector< wxTreeItemId >::iterator iter;

    for( iter=DataSetIds.begin(); iter != DataSetIds.end(); iter++ )
    {
        DataSet* _tmpDataSetPtr = NULL;
        
        /* static_cast to upcast the wxTreeItemId returned by (SimulationTreeCtrl->GetItemData( *iter )) 
        to a DataSet pointer.  Note that DataSet is inherited from wxTreeItemId. */
        _tmpDataSetPtr = static_cast< DataSet* > (SimulationTreeCtrl->GetItemData( *iter ));
    
        if( !std::strcmp( _tmpDataSetPtr->GetFileName().c_str(), filename.c_str() ) )
        {
            return _tmpDataSetPtr;
        }
    }

    return NULL;
}

//Return a pointer to the LoadDataGUI
LoadDataGUI* VascularSimViewerUIDialog::GetLoadDataGuiPtr()
{
    return mLoadDataGUI;
}
