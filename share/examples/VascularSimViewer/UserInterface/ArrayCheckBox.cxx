//--- Header Include ---//
#include "ArrayCheckBox.h"

using namespace vascularsimviewer;

// Constructor
ArrayCheckBox::ArrayCheckBox( wxWindow* parent, const std::string &name )
    :
    wxCheckBox(),
    mParent( parent ),
    CheckboxName( name )
{
    ;
}

// Destructor
ArrayCheckBox::~ArrayCheckBox()
{
    ;
}

///////////////////////////
void ArrayCheckBox::CreateCheckBox()
{
    wxString label;
    label.append(wxT("  "));
    label.Append( CheckboxName );
    
    this->Create( mParent, wxID_ANY, label ,wxDefaultPosition, wxDefaultSize, 0 );
}


