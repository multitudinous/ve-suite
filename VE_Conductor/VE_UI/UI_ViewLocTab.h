#ifndef _VE_UI_VIEWLOC_TAB_H_
#define _VE_UI_VIEWLOC_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>

enum VIEWLOC_TAB_IDS{
   VIEWLOC_RBOX,
   VIEWLOC_LOAD_BUTTON,
   VIEWLOC_ACCEPTNEWVPNAME_BUTTON,
   VIEWLOC_CANCELNEWVPNAME_BUTTON,
   VIEWLOC_REMOVEVP_COMBOBOX,
   VIEWLOC_MOVETOVP_COMBOBOX,
   VIEWLOC_NEWFLY_BUTTON,
   VIEWLOC_ACCEPTNEWFLYNAME_BUTTON,
   VIEWLOC_CANCELNEWFLYNAME_BUTTON,
   VIEWLOC_ACTIVEFLYSEL_COMBOBOX,
   VIEWLOC_ADDVPTOFLYSEL_COMBOBOX,
   VIEWLOC_INSERTVPINFLYSEL_COMBOBOX,
   VIEWLOC_REMOVEVPFROMFLYSEL_COMBOBOX,
   VIEWLOC_RUNFLY_BUTTON,
   VIEWLOC_STOPFLY_BUTTON,
   VIEWLOC_FLYBUILDER_LISTBOX
};

class UI_ViewLocTabScroll: public wxScrolledWindow{
public:
   UI_ViewLocTabScroll(wxWindow* parent);
   ~UI_ViewLocTabScroll();

   wxButton* _addnewviewptButton;
   wxButton* _newvwptNameOKButton;
   wxButton* _newvwptNameCancelButton;
   wxButton* _addnewflythroughButton;
   wxButton* _newflythroughNameOKButton;
   wxButton* _newflythroughNameCancelButton;
   wxButton* _runactiveflyButton;
   wxButton* _stopactiveflyButton;


   wxComboBox* _removevwptSel;
   wxComboBox* _movetovwptSel;
   wxComboBox* _activeflySel;
   wxComboBox* _addvptoflySel;
   wxComboBox* _insertvpinflySel;
   wxComboBox* _removevpfromflySel;

   wxListBox* _flybuilderListBox;

   wxTextCtrl* _newvwptNameCtrl;
   wxTextCtrl* _newflythroughNameCtrl;
protected:

   DECLARE_EVENT_TABLE()
};




class UI_ViewLocTab : public wxPanel{
public:
   UI_ViewLocTab(wxNotebook* tControl);
   ~UI_ViewLocTab( void );

   int numStoredLocations;
   wxString* _defaultName;

protected:
   void _buildPage();
   
   UI_ViewLocTabScroll* _viewLocScroll;
   wxNotebook* _parent;

   //the controls
   //wxRadioBox* _locationsRBox;
   
   //wxButton* _removeButton;
   //wxButton* _writeButton;
   //wxButton* _readButton;
   //wxButton* _moveButton;
   //wxButton* _applynameButton;
   //wxTextCtrl* _viewpointName;
   //event handlers
   void _onViewLoc(wxCommandEvent& event);
   void _onLoad(wxCommandEvent& event);
   void _onAcceptNewVPName(wxCommandEvent& event);
   void _onCancelNewVPName(wxCommandEvent& event);
   void _onRemoveVP(wxCommandEvent& event);
   void _onMoveToVP(wxCommandEvent& event);
   void _onBuildNewFlyButton(wxCommandEvent& event);
   void _onAcceptNewFlyName(wxCommandEvent& event);
   void _onCancelNewFlyName(wxCommandEvent& event);
   void _onActiveFlySel(wxCommandEvent& event);
   void _onAddVPtoFlySel(wxCommandEvent& event);
   void _onInsertVPinFlySel(wxCommandEvent& event);
   void _onRemoveVPfromFlySel(wxCommandEvent& event);
   void _onStartActiveFly(wxCommandEvent& event);
   void _onStopFly(wxCommandEvent& event);
   void _onFlyBuilderListBox(wxCommandEvent& event);


   //void _onWrite(wxCommandEvent& event);

   //void _onRead(wxCommandEvent& event);
   //void _onDetach(wxCommandEvent& event);
   //void _onMove(wxCommandEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_VIEWLOC_TAB_H_
