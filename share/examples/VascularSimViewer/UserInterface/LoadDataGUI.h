#ifndef LOAD_DATA_GUI_H
#define LOAD_DATA_GUI_H

//--- wxWidget Includes ---//
#include <wx/dialog.h>

// --- C++ Includes --- //
#include <string>

//--- wxWidget Forward Declarations ---//
class wxFilePickerCtrl;

namespace vascularsimviewer
{
// --- This Plugin Forward Declarations --- //
class VascularSimViewerUIDialog;
class DataSet;

class LoadDataGUI : public wxDialog 
{
    public:
        //Constructor
        LoadDataGUI(
            wxWindow *parent,
            wxWindowID id,
            const wxString &title,
	        VascularSimViewerUIDialog* UIDialog );
				
        //Destructor
        ~LoadDataGUI();
                
        //Check to see if the file is already laoded
        bool FileIsAlreadyLoaded( std::string filename );
        

    protected:
	
    //Taken from src/ves/conductor/UIDialog.h
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
	
    private:
    
        VascularSimViewerUIDialog* VascSimViewUIDialog;
    	
        wxFilePickerCtrl* vtkFilePicker;

        //Command to Build Dialog
        void BuildDialog();
		
        //Close Dialog
        void OnClose( wxCommandEvent &event );
		
        //Load vtk File
        void OnLoadVTUFile( wxCommandEvent &event );
        
        //Show the Selection Dialog of Previously Loaded Data
        void ShowLoadedDataSetDialog();
        
        //Check to See if the file exists
        bool FileExists( std::string filename );
        
        //
        void ShowLoadedDataSetDialog( std::string filename );
              
        //Commands
        enum 
        {
            LOAD_VTK_COMMAND
        };

        DECLARE_EVENT_TABLE()
};
}  //end vascularsimviewer namespace

#endif // LOAD_DATA_GUI_H
