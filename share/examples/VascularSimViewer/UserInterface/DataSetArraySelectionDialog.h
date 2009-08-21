#ifndef DATA_SET_ARRAY_SELECTION_DIALOG_H
#define DATA_SET_ARRAY_SELECTION_DIALOG_H

//--- wxWidgets Includes ---//
#include <wx/dialog.h>

// --- C++ Includes --- //
#include <string>
#include <vector>

/*
This Dialog basically keeps track of the loaded arrays fro each dataset
*/

namespace vascularsimviewer
{
class ArrayCheckBox;
class DataSet;

class DataSetArraySelectionDialog : public wxDialog
{
    public:
    
        //Constructor
        DataSetArraySelectionDialog( wxWindowID id, const wxString& title, int numArrays, DataSet* parent);
        
        //Destructor
        ~DataSetArraySelectionDialog();
        
    private:
    
        DataSet* parentDataSet;
        
        //What to do when OK is clicked
        void OnOK( wxCommandEvent &event );
        
        //Storage vector to keep track of arrays and associated checkboxes
        std::vector< ArrayCheckBox* > CheckBoxes;
        
        //NUmber of arrays
        int ArrayNumber;
        
        DECLARE_EVENT_TABLE()
        
};
}
#endif // DATA_SET_ARRAY_SELECTION_DIALOG_H
