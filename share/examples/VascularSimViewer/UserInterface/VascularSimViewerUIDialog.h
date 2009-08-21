#ifndef VASCULAR_SIM_VIEWER_UI_DIALOG_H
#define VASCULAR_SIM_VIEWER_UI_DIALOG_H


// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

// --- wxWidgets Includes --- //
#include <wx/treebase.h>

// --- C++ Includes --- //
#include <vector>


namespace ves
{
namespace conductor
{
namespace util
{
class CORBAServiceList;
}
}
}

class wxTreeCtrl;

namespace vascularsimviewer
{
class LoadDataGUI;
class DataSet;

class VascularSimViewerUIDialog : public ves::conductor::UIDialog
{
    public:
        //Constructor
        VascularSimViewerUIDialog( 
            wxWindow *parent,
            int id,
            ves::conductor::util::CORBAServiceList* service );
         
        //Destructor
        ~VascularSimViewerUIDialog();
        
        //Add Data Set to SimTree
        void AddDataSet( const std::string &filename );
        
        int GetNumDataSets();
        
        std::vector< wxTreeItemId >* GetDataSets();
        
        DataSet* GetDataSetByFileName( const std::string& filename );
        
        LoadDataGUI* GetLoadDataGuiPtr();
        
        //Commands
        enum 
        {
            LOAD_DATA_COMMAND,
            MANIPULATE_DATA_COMMAND,
            VISUALIZE_DATA_COMMAND,
            REMOVE_COMMAND,
            CLEAR_ALL_VIS_COMMAND,
            CLEAR_ALL_DATA_COMMAND,
            BOUNDING_BOX_COMMAND,
            LEGEND_COMMAND,
            EDIT_LEGEND_COMMAND
        };
        
    protected:

        //DialogTesterWindow *mDialogTesterWindow;

        LoadDataGUI *mLoadDataGUI;

    private:
    
        std::vector< wxTreeItemId > DataSetIds;
        
        // Service List used to send commands to Xplorer
        ves::conductor::util::CORBAServiceList* mServiceList;

        // Pointer to SimTree
        wxTreeCtrl *SimulationTreeCtrl;

        // Tree's Root Id
        wxTreeItemId _rootID;

        //Command to Build Dialog
        void BuildDialog();
		
        //Close Dialog
        void OnClose( wxCommandEvent &event );

        //Start Load Data Dialog
        void OnLoadData( wxCommandEvent &event );
		
        //Manipulate Data Dialog
        void OnManipulateData( wxCommandEvent &event );
		
        //Visualize Data Dialog
        void OnVisualizeData( wxCommandEvent &event );
		
        //Remove Something from simulation tree
        void OnRemove( wxCommandEvent &event );
		
        //Clear all visualizations but leave data
        void OnClearAllVis( wxCommandEvent &event );
		
        //Clear all data and visualizations
        void OnClearAllData( wxCommandEvent &event );
		
        //Place Bounding Box around data set
        void OnBoundingBox( wxCommandEvent &event );
		
        //Add legend to data set
        void OnLegend( wxCommandEvent &event );
		
        //Edit Legend
        void OnEditLegend( wxCommandEvent &event );
	

        DECLARE_EVENT_TABLE()
};

} //end of vascularsimviewer namespace

#endif // VASCULAR_SIM_VIEWER_UI_DIALOG_H
