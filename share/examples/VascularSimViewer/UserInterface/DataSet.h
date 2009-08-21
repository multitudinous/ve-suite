#ifndef DATA_SET_H
#define DATA_SET_H

// --- C++ Includes --- //
#include <string>
#include <vector>

// --- wxWidgets Includes --- //
#include <wx/treectrl.h>

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

class wxDialog;

namespace vascularsimviewer
{
class DataSetArray;
class DataSetArraySelectionDialog;
class VascularSimViewerUIDialog;
class LoadDataGUI;

class DataSet : public wxTreeItemData
{
    public:
        //Constructor
        DataSet( const std::string &inFilename,
            VascularSimViewerUIDialog* UIDialog,
            ves::conductor::util::CORBAServiceList* service );
				
        //Destructor
        ~DataSet();
        
        // Return the fielname of this dataset
        const std::string& GetFileName();
        
        int GetNumArrays();
        
        void ShowDialog();
        
        DataSetArray* GetPtArray( int arrayIndex );
        
        LoadDataGUI* GetLoadDataGui();
        
        void LoadDataInXplorer();
	
    private:
    
        VascularSimViewerUIDialog* VascSimUIDialog;
    
        DataSetArraySelectionDialog* DataSetDialog;
        
        ves::conductor::util::CORBAServiceList* mServiceList;
    
        //Filename associated with this Dialog
        std::string mFilename;
    		
        //Vector of DataSetArrays
        std::vector< DataSetArray* > mPtDataArrays;
        
        //Command to Scan VTU File for Available Datasets
        void ScanVTUFile();  

};
}  //end vascularsimviewer namespace

#endif // DATA_SET_H
