#include "UI_TopFrame.h"
#include "UI_Frame.h"
#include "controlIds.h"

////////////////////////////////////////////////////
UI_TopFrame::UI_TopFrame(const wxString& title,
             const wxPoint& pos,
             const wxSize& size,
             long style)
: wxFrame((wxWindow *) NULL, -1, title, pos, size, style)
{
     _uiFrame = new UI_Frame(this,ID_UI_TABS);

     wxBoxSizer* _topframeSizer = new wxBoxSizer(wxHORIZONTAL);
     _topframeSizer->Add(_uiFrame,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
     _topframeSizer->Layout();
     SetSizer(_topframeSizer);
 
     SetAutoLayout(true); 
     _topframeSizer->Fit(this);  
}

UI_TopFrame::~UI_TopFrame()
{
}
