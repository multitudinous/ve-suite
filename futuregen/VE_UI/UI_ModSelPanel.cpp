#include "UI_ModSelPanel.h"
#include "UI_Frame.h"
#include "UI_ModelData.h"


BEGIN_EVENT_TABLE(UI_ModSelScroll, wxScrolledWindow)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_ModSelScroll::UI_ModSelScroll(wxWindow* parent)
:wxScrolledWindow(parent, -1, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL)
{
   int nUnitX=10;
   int nUnitY=10;
   int nPixX = 10;
   int nPixY = 10;
   SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );

   int _numModels = ((UI_ModSelPanel *)GetParent())->_modelData->GetNumberOfModels();
   cout << " Number of Models in System : " << _numModels << endl;
   //wxString _models[_numModels]; //can't pass compile _numModels needs compile time value
   //begin yang's change
    wxString *_models;
   _models = new wxString [_numModels];
   //finished yang's changes
   for ( int i=0; i<_numModels; i++)
      _models[i] = wxT(((UI_ModSelPanel *)GetParent())->_modelData->GetModelName(i));
   

   _modelSelBox = new wxRadioBox(this, RBOX_MODEL_SELECT, wxT("VE Models"), 
                                 wxDefaultPosition, wxDefaultSize, _numModels, 
                                 _models, 1, wxRA_SPECIFY_COLS);

   wxBoxSizer* _col = new wxBoxSizer(wxVERTICAL);
   _col->Add(_modelSelBox,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   SetSizer(_col);
   delete [] _models;
}

UI_ModSelScroll::~UI_ModSelScroll()
{
   delete _modelSelBox;
}


BEGIN_EVENT_TABLE(UI_ModSelPanel, wxPanel)
   EVT_RADIOBOX(RBOX_MODEL_SELECT, UI_ModSelPanel::_onModSelect)
END_EVENT_TABLE()
//////////////////////////////////////////////////
//Constructor                                   //
//////////////////////////////////////////////////
UI_ModSelPanel::UI_ModSelPanel(wxWindow* parent, UI_ModelData* _model)
:wxPanel(parent)
{
   _modelData = _model;

   _modselScroll = new UI_ModSelScroll(this);

   wxBoxSizer* _col = new wxBoxSizer(wxVERTICAL);
   _col->Add(_modselScroll,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   SetAutoLayout(true);
   SetSizer(_col);
}

UI_ModSelPanel::~UI_ModSelPanel()
{
   delete _modselScroll;
}

void UI_ModSelPanel::_onModSelect()
{
   ((UI_Frame *)GetParent())->activeModIndex = _modselScroll->_modelSelBox->GetSelection();
   ((UI_Frame *)GetParent())->OnChangeModel();
}

