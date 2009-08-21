#ifndef ARRAY_CHECKBOX_H
#define ARRAY_CHECKBOX_H

//--- wxWidgets Includes ---//
#include <wx/checkbox.h>

// --- C++ Includes --- ///
#include <string>


class wxTreeItemId;

namespace vascularsimviewer
{
class ArrayCheckBox : public wxCheckBox
{
    public:
        // Constructor
        ArrayCheckBox( wxWindow* parent, const std::string &name );
        
        // Destructor
        ~ArrayCheckBox();
        
        void CreateCheckBox();
        
    private:
    
        wxWindow* mParent;
    
        std::string CheckboxName;
};
}

#endif // ARRAY_CHECKBOX_H
